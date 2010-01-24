/*
Copyright (c) 2009 by Juliusz Chroboczek

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <pthread.h>
#include <sched.h>

#include "threadpool.h"

#ifndef USE_ATOMIC_INTRINSICS

/* This should be safe, since every lock will ack as a memory barrier. */

typedef volatile int atomic_bool;

static inline int
atomic_test(atomic_bool *a)
{
    return *a;
}

static inline void
atomic_set(atomic_bool *a)
{
    *a = 1;
}

static inline void
atomic_reset(atomic_bool *a)
{
    *a = 0;
}

#else

/* But if you're paranoid, and have a recent version of gcc or clang... */

typedef int atomic_bool;

static inline int
atomic_test(atomic_bool *a)
{
    return __sync_fetch_and_or(a, 0);
}
    
static inline void
atomic_set(atomic_bool *a)
{
    __sync_fetch_and_or(a, 1);
}

static inline void
atomic_reset(atomic_bool *a)
{
    __sync_fetch_and_and(a, 0);
}

#endif

struct threadpool_queue {
    threadpool_item_t *first;
    threadpool_item_t *last;
};

struct threadpool {
    int maxthreads, threads, idle;
    threadpool_queue_t scheduled, scheduled_back;
    /* Set when we request that all threads die. */
    int dying;
    /* If this is false, we are guaranteed that scheduled_back is empty. */
    atomic_bool have_scheduled_back;
    /* Protects everything except the atomics above. */
    pthread_mutex_t lock;
    /* Signalled whenever a new continuation is enqueued or dying is set. */
    pthread_cond_t cond;
    /* Signalled whenever a thread dies. */
    pthread_cond_t die_cond;
    threadpool_func_t *wakeup;
    void *wakeup_closure;
};

threadpool_t *
threadpool_create(int maxthreads,
                  threadpool_func_t *wakeup, void *wakeup_closure)
{
    threadpool_t *tp;
    tp = calloc(1, sizeof(threadpool_t));
    if(tp == NULL)
        return NULL;

    tp->maxthreads = maxthreads;
    tp->wakeup = wakeup;
    tp->wakeup_closure = wakeup_closure;
    pthread_mutex_init(&tp->lock, NULL);
    pthread_cond_init(&tp->cond, NULL);
    pthread_cond_init(&tp->die_cond, NULL);

    return tp;
}

int
threadpool_die(threadpool_t *threadpool, int canblock)
{
    int done;

    pthread_mutex_lock(&threadpool->lock);

    threadpool->dying = 1;
    pthread_cond_broadcast(&threadpool->cond);

    while(threadpool->threads > 0) {
        if(threadpool->scheduled_back.first || !canblock)
            break;
        pthread_cond_wait(&threadpool->die_cond, &threadpool->lock);
    }

    done = threadpool->threads == 0;

    pthread_mutex_unlock(&threadpool->lock);
    return done;
}

int
threadpool_destroy(threadpool_t *threadpool)
{
    int dead;

    pthread_mutex_lock(&threadpool->lock);
    dead =
        threadpool->threads == 0 &&
        threadpool->scheduled.first == NULL &&
        threadpool->scheduled_back.first == NULL;
    pthread_mutex_unlock(&threadpool->lock);

    if(!dead)
        return -1;

    pthread_cond_destroy(&threadpool->cond);
    pthread_cond_destroy(&threadpool->die_cond);
    pthread_mutex_destroy(&threadpool->lock);
    free(threadpool);
    return 1;
}

static threadpool_item_t *
threadpool_dequeue(threadpool_queue_t *queue)
{
    threadpool_item_t *item;

    if(queue->first == NULL)
        return NULL;

    item = queue->first;
    queue->first = item->next;
    if(item->next == NULL)
        queue->last = NULL;
    return item;
}

static void *
thread_main(void *pool)
{
    threadpool_t *threadpool = pool;
    threadpool_item_t *item;
    threadpool_func_t *func;
    void *closure;

 again:
    pthread_mutex_lock(&threadpool->lock);

    if(threadpool->scheduled.first == NULL) {
        struct timespec ts;

        if(threadpool->dying)
            goto die;

        /* Beware when benchmarking.  Under Linux with NPTL, idle threads
           are slightly counter-productive in some benchmarks, but
           extremely productive in others. */

        /* This constant may need to be tweaked. */
        if(threadpool->idle >= 2)
            goto die;

        /* Don't bother with POSIX clocks. */
        ts.tv_sec = time(NULL) + 1;
        ts.tv_nsec = 0;

        threadpool->idle++;
        pthread_cond_timedwait(&threadpool->cond, &threadpool->lock, &ts);
        threadpool->idle--;
        if(threadpool->scheduled.first == NULL)
            goto die;
    }

    item = threadpool_dequeue(&threadpool->scheduled);
    pthread_mutex_unlock(&threadpool->lock);

    func = item->func;
    closure = item->closure;
    free(item);

    func(closure);
    goto again;

 die:
    threadpool->threads--;
    pthread_cond_broadcast(&threadpool->die_cond);
    pthread_mutex_unlock(&threadpool->lock);
    return NULL;
}

/* This is called with the pool locked. */
static int
threadpool_new_thread(threadpool_t *threadpool)
{
    pthread_t thread;
    pthread_attr_t attr;
    int rc;

    assert(threadpool->threads < threadpool->maxthreads);

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    rc = pthread_create(&thread, &attr, thread_main, (void*)threadpool);
    if(rc) {
        errno = rc;
        return -1;
    }
    threadpool->threads++;
    return 1;
}

/* There's a reason for the inconvenient interface: we want to perform the
   allocation outside of the critical region, and only take the lock when
   inserting the new cell. */

static threadpool_item_t *
threadpool_item_alloc(threadpool_func_t *func, void *closure)
{
    threadpool_item_t *item;

    item = malloc(sizeof(threadpool_item_t));
    if(item == NULL)
        return NULL;

    item->func = func;
    item->closure = closure;
    item->next = NULL;

    return item;
}

static void
threadpool_enqueue(threadpool_queue_t *queue, threadpool_item_t *item)
{
    item->next = NULL;
    if(queue->last)
        queue->last->next = item;
    else
        queue->first = item;
    queue->last = item;
}

int
threadpool_schedule(threadpool_t *threadpool,
                    threadpool_func_t *func, void *closure)
{
    threadpool_item_t *item;
    int rc = 0;
    int dosignal = 1;

    item = threadpool_item_alloc(func, closure);
    if(item == NULL)
        return -1;

    pthread_mutex_lock(&threadpool->lock);
    threadpool_enqueue(&threadpool->scheduled, item);
    if(threadpool->idle == 0) {
        dosignal = 0;
        if(threadpool->threads < threadpool->maxthreads) {
            rc = threadpool_new_thread(threadpool);
            if(rc < 0 && threadpool->threads > 0)
                rc = 0;             /* we'll recover */
        }
    }
    if(dosignal)
        pthread_cond_signal(&threadpool->cond);
    pthread_mutex_unlock(&threadpool->lock);

    return rc;
}

int
threadpool_schedule_back(threadpool_t *threadpool,
                         threadpool_func_t *func, void *closure)
{
    threadpool_item_t *item;
    int wake = 1;

    item = threadpool_item_alloc(func, closure);
    if(item == NULL)
        return -1;

    pthread_mutex_lock(&threadpool->lock);
    if(threadpool->have_scheduled_back)
        wake = 0;
    /* Order is important. */
    atomic_set(&threadpool->have_scheduled_back);
    threadpool_enqueue(&threadpool->scheduled_back, item);
    pthread_mutex_unlock(&threadpool->lock);

    if(wake && threadpool->wakeup)
        threadpool->wakeup(threadpool->wakeup_closure);

    return 0;
}

threadpool_item_t *
threadpool_get_back(threadpool_t *threadpool)
{
    threadpool_item_t *item;

    if(!atomic_test(&threadpool->have_scheduled_back))
        return NULL;

    pthread_mutex_lock(&threadpool->lock);
    item = threadpool->scheduled_back.first;
    /* Order is important. */
    threadpool->scheduled_back.first = NULL;
    threadpool->scheduled_back.last = NULL;
    atomic_reset(&threadpool->have_scheduled_back);
    pthread_mutex_unlock(&threadpool->lock);

    return item;
}

void
threadpool_items_run(threadpool_item_t *items)
{
    while(items) {
        threadpool_item_t *first;
        threadpool_func_t *func;
        void *closure;
        first = items;
        items = items->next;
        func = first->func;
        closure = first->closure;
        free(first);
        func(closure);
    }
}
