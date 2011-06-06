/*
Copyright (c) 2008-2010,
  Gabriel Kerneis     <kerneis@pps.jussieu.fr>
Copyright (c) 2004-2005,
  Juliusz Chroboczek  <jch@pps.jussieu.fr>

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

#define _GNU_SOURCE

#define NO_CPS_PROTO
#include "cpc_runtime.h"

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <sys/time.h>
#include <fcntl.h>
#include <stddef.h>

#include <poll.h> /* cpc_d_io_wait */

#include "threadpool/threadpool.h"

#define EV_STANDALONE 1
#include "ev.c"

union cpc_watcher {
    struct ev_watcher w;
    struct ev_io io;
    struct ev_timer timer;
};

typedef struct cpc_thread {
    union cpc_watcher ev;
    struct cpc_condvar *condvar;
    struct cpc_thread *next;
    cpc_sched *sched;
    int state;
    struct cpc_continuation cont;
} cpc_thread;

static inline cpc_continuation *
get_cont(cpc_thread *t)
{
    return (cpc_continuation *)(((char *)t) + offsetof(struct cpc_thread, cont));
}

static inline cpc_thread *
get_thread(cpc_continuation *c)
{
    return (cpc_thread *)(((char *)c) - offsetof(struct cpc_thread, cont));
}

struct cpc_sched {
    threadpool_t *pool;
    struct cpc_sched *next;
};
static pthread_t main_loop_id;
#define IS_DETACHED (cpc_default_threadpool && !pthread_equal(main_loop_id,pthread_self()))
#define MAX_THREADS 20
cpc_sched *cpc_default_threadpool = NULL;
struct ev_loop *cpc_default_loop = NULL;

typedef struct cpc_thread_queue {
    struct cpc_thread *head;
    struct cpc_thread *tail;
} cpc_thread_queue;

typedef struct cpc_sched_queue {
    struct cpc_sched *head;
    struct cpc_sched *tail;
} cpc_sched_queue;

struct cpc_condvar {
    int refcount;
    cpc_thread_queue queue;
};

#define STATE_UNKNOWN -1
#define STATE_SLEEPING -2
#define STATE_DETACHED -3

static cpc_thread_queue ready = {NULL, NULL};
static ev_idle ready_watcher;
static ev_async attach_watcher;
static cpc_sched_queue schedulers = {NULL, NULL};

static void cpc_invoke_continuation(struct cpc_continuation *c);

volatile int detached_count = 0;

/*** Basic operations on continuations ***/

void
cpc_print_continuation(struct cpc_continuation *c, char *s)
{
    int i = 0;
    if (c == NULL) {
        fprintf(stderr, "(%s) NULL continuation\n", s);
        return;
    }
    cpc_thread *t = get_thread(c);
    fprintf(stderr, "(%s) Continuation %p:\n"
        "  condvar%p\tnext: %p\n"
        "  state: %d\tlength: %u\tsize: %u\n",
        s,c,
        t->condvar,t->next,
        t->state,c->length,c->size);
    while(i < c->length) {
        fprintf(stderr,"%.2x",(unsigned char)c->c[i]);
        i++;
        fprintf(stderr, (i%16 == 0 || i == c->length) ? "\n" : " ");
    }
}

struct cpc_thread *
cpc_thread_get(int size)
{
    struct cpc_thread *t;
    t = malloc(sizeof(struct cpc_thread) + (size - 1));
    t->cont.size = size;
    return t;
}

static void
free_detached_cb(void *closure)
{
    detached_count--;
}

void
cpc_continuation_free(struct cpc_continuation *c)
{
    cpc_thread *t = get_thread(c);
    assert((t->state == STATE_UNKNOWN || t->state == STATE_DETACHED) && t->condvar == NULL);
    if(t->state == STATE_DETACHED)
        threadpool_schedule_back(t->sched->pool, free_detached_cb, NULL);
    free(t);
}

cpc_continuation *
cpc_continuation_expand(struct cpc_continuation *c, int n)
{
    int size;
    cpc_thread *d, *t;

    if(c == (void*)0)
        size = n + 20;
    else
        size = c->size * 2 + n;

    d = cpc_thread_get(size);

    if(c == (void*)0) {
        d->cont.length = 0;
        d->condvar = NULL;
        d->next = NULL;
        d->sched = cpc_default_sched;
        d->state = STATE_UNKNOWN;
        ev_init(&d->ev.w, NULL);
        return get_cont(d);
    }

    t = get_thread(c);

    memcpy(d->cont.c, c->c, c->length);

    d->cont.length = c->length;
    d->condvar = t->condvar;
    d->next = t->next;
    d->sched = t->sched;
    d->state = t->state;
    assert(!ev_is_active(&t->ev));
    ev_init(&d->ev.w, NULL);

    free(t);

    return get_cont(d);
}

/*** Basic operations on queues ***/

static inline void
enqueue(cpc_thread_queue *queue, cpc_thread *t)
{
    assert(t);

    t->next = NULL;

    if(queue->head == NULL) {
        queue->head = queue->tail = t;
    } else {
        queue->tail->next = t;
        queue->tail = t;
    }
}

static inline cpc_thread *
dequeue(cpc_thread_queue *queue)
{
    cpc_thread *t;
    if(queue->head == NULL)
        return NULL;
    t = queue->head;
    queue->head = t->next;
    if(queue->head == NULL)
        queue->tail = NULL;
    t->next = NULL;
    return t;
}    

static void
schedule_cb(struct ev_loop *loop, ev_idle *w, int revents)
{
    cpc_thread_queue q;
    cpc_thread *thread;

    q = ready;
    ready.head = ready.tail = NULL;
    ev_idle_stop(cpc_default_loop, w);

    while(1) {
        thread = dequeue(&q);
        if(thread == NULL) break;
        assert(thread->state == STATE_UNKNOWN && thread->condvar == NULL);
        cpc_invoke_continuation(get_cont(thread));
    }
}

static inline void
schedule(cpc_thread *thread)
{
    if(!ready.head)
        ev_idle_start(cpc_default_loop, &ready_watcher);
    enqueue(&ready, thread);
}



static void
cond_enqueue(cpc_thread_queue *queue, cpc_thread *t)
{
    assert(t);

    t->next = NULL;

    if(queue->head == NULL) {
        queue->head = queue->tail = t;
    } else {
        queue->tail->next = t;
        queue->tail = t;
    }
}

static cpc_thread *
cond_dequeue(cpc_thread_queue *queue)
{
    cpc_thread *t;
    if(queue->head == NULL)
        return NULL;
    t = queue->head;
    queue->head = t->next;
    if(queue->head == NULL)
        queue->tail = NULL;
    t->next = NULL;
    return t;
}    

static void
cond_dequeue_1(cpc_thread_queue *queue, cpc_thread *cont)
{
    cpc_thread *t, *target;
    t = queue->head;
    if(t == cont) {
        queue->head = queue->head->next;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(t->next != cont)
        t = t->next;
    target = t->next;
    t->next = target->next;
    target->next = NULL;
    if(t->next == NULL)
        queue->tail = t;
}

// XXX
static void
dequeue_other(cpc_thread *thread)
{
    if(thread->state == STATE_SLEEPING) {
        assert(ev_is_active(&thread->ev.w));
        ev_timer_stop(cpc_default_loop, &thread->ev.timer);
    } else if(thread->state >= 0) {
        assert(ev_is_active(&thread->ev.w));
        ev_io_stop(cpc_default_loop, &thread->ev.io);
    } else {
        assert(thread->state == STATE_UNKNOWN);
        assert(!ev_is_active(&thread->ev.w));
    }
    thread->state = STATE_UNKNOWN;
}

/*** Condition variables ***/

cpc_condvar *
cpc_condvar_get()
{
    cpc_condvar *cond;
    cond = malloc(sizeof(cpc_condvar));
    cond->refcount = 1;
    cond->queue.head = NULL;
    cond->queue.tail = NULL;
    return cond;
}

cpc_condvar *
cpc_condvar_retain(cpc_condvar *cond)
{
    assert(!IS_DETACHED);
    cond->refcount++;
    return cond;
}

void
cpc_condvar_release(cpc_condvar *cond)
{
    assert(!IS_DETACHED && cond->refcount > 0);
    cond->refcount--;
    if(cond->refcount == 0) {
        assert(cond->queue.head == NULL);
        free(cond);
    }
}

int
cpc_condvar_count(cpc_condvar *cond)
{
    assert(!IS_DETACHED);

    cpc_thread *thread = cond->queue.head;
    int i = 0;

    thread = cond->queue.head;
    while(thread) {
        i++;
        thread = thread->next;
    }
    return i;
}

#ifdef CPC_COMPACT_CONTINUATIONS
#define LAST_ARG(type, arg) type arg;}__attribute((packed))
#else
#define LAST_ARG(type, arg) type arg __attribute((aligned));}
#endif

#define cps_expand1(name,type,arg)\
    struct name##_arglist {\
        LAST_ARG(type,arg);\
    cpc_continuation *\
    name(struct cpc_continuation *cont)\
    {\
        struct name##_arglist *cpc_arguments ;\
        cpc_arguments = (struct name##_arglist *) cpc_dealloc(cont,\
                        (int )sizeof(struct name##_arglist ));\
        type arg = cpc_arguments -> arg;\
        cpc_thread *thread = get_thread(cont);

#define cps_expand3(name,type1,arg1,type2,arg2,type3,arg3) \
    struct name##_arglist {\
        type1 arg1;\
        type2 arg2;\
        LAST_ARG(type3,arg3);\
    cpc_continuation *\
    name(struct cpc_continuation *cont)\
    {\
        struct name##_arglist *cpc_arguments ;\
        cpc_arguments = (struct name##_arglist *) cpc_dealloc(cont,\
                        (int )sizeof(struct name##_arglist ));\
        type1 arg1 = cpc_arguments -> arg1;\
        type2 arg2 = cpc_arguments -> arg2;\
        type3 arg3 = cpc_arguments -> arg3;\
        cpc_thread *thread = get_thread(cont);


/* cps int cpc_wait(cpc_condvar *cond) */
cps_expand1(cpc_wait, cpc_condvar *, cond)
    assert(!IS_DETACHED && thread->condvar == NULL && thread->state == STATE_UNKNOWN);
    thread->condvar = cond;
    cond_enqueue(&cond->queue, thread);
    return NULL;
}

void
cpc_signal(cpc_condvar *cond)
{
    int rc = CPC_CONDVAR;
    assert(!IS_DETACHED);
    cpc_thread *thread = cond_dequeue(&cond->queue);
    if(thread == NULL)
        return;
    assert(thread->condvar == cond);
    thread->condvar = NULL;
    cpc_continuation_patch(get_cont(thread), sizeof(int), &rc);
    dequeue_other(thread);
    schedule(thread);
}

void
cpc_signal_all(cpc_condvar *cond)
{
    int rc = CPC_CONDVAR;
    cpc_thread *thread;

    assert(!IS_DETACHED);
    while(1) {
        thread = cond_dequeue(&cond->queue);
        if(thread == NULL)
            break;
        assert(thread->condvar == cond);
        thread->condvar = NULL;
        cpc_continuation_patch(get_cont(thread), sizeof(int), &rc);
        dequeue_other(thread);
        schedule(thread);
    }
}

/*** cpc_sleep ***/

static void
sleep_cb(struct ev_loop *loop, ev_timer *w, int revents)
{
    const int rc = CPC_TIMEOUT;
    cpc_thread *thread = (cpc_thread *)w;
    cpc_continuation *cont = get_cont(thread);

    assert(thread->state == STATE_SLEEPING);
    if(thread->condvar)
        cond_dequeue_1(&thread->condvar->queue, thread);
    thread->condvar = NULL;
    thread->state = STATE_UNKNOWN;
    cpc_continuation_patch(cont, sizeof(int), &rc);
    cpc_invoke_continuation(cont);
    /* The watcher is stopped automatically. */
}

/* cps int cpc_sleep(int sec, int usec, cpc_condvar *cond) */
cps_expand3(cpc_sleep, int, sec, int, usec, cpc_condvar *, cond)
    struct timeval when;

    assert(cont);

    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        when.tv_sec = sec;
        when.tv_usec = usec;
        select(0, NULL, NULL, NULL, &when); // XXX
        const int rc = CPC_TIMEOUT;
        cpc_continuation_patch(cont, sizeof(int), &rc);
        return cont;
    }

    assert(thread->condvar == NULL && thread->state == STATE_UNKNOWN);
    if(cond) {
        thread->condvar = cond;
        cond_enqueue(&cond->queue, thread);
    }
    thread->state = STATE_SLEEPING;
    assert(!ev_is_active(&thread->ev.w));
    ev_timer_init(&thread->ev.timer, sleep_cb,
            ((ev_tstamp) sec) + usec * 1e-6, 0. );
    ev_timer_start(cpc_default_loop, &thread->ev.timer);
    return NULL;
}

/*** cpc_io_wait ***/

static inline int
cpc_d_io_wait(int fd, int direction)
{
    struct pollfd pfd[1];
    int pollevent = 0;
    int rc;

    pollevent |= (direction & CPC_IO_OUT) ? POLLOUT : 0;
    pollevent |= (direction & CPC_IO_IN) ? POLLIN : 0;

    pfd[0].fd = fd;
    pfd[0].events = pollevent;
    pfd[0].revents = 0;

    rc = poll(pfd, 1, -1);
    if(rc < 0)
        return -1;

    rc = 0;
    rc |= (pfd[0].revents & POLLOUT) ? CPC_IO_OUT : 0;
    rc |= (pfd[0].revents & POLLIN) ? CPC_IO_IN : 0;

    return rc;
}

static void
io_wait_cb(struct ev_loop *loop, ev_io *w, int revents)
{
    cpc_thread *thread = (cpc_thread *)w;
    cpc_continuation *cont = get_cont(thread);

    if(thread->condvar) {
        cond_dequeue_1(&thread->condvar->queue, thread);
    }
    thread->condvar = NULL;
    thread->state = STATE_UNKNOWN;

    ev_io_stop(cpc_default_loop, w);
    cpc_continuation_patch(cont, sizeof(int), &revents);
    cpc_invoke_continuation(get_cont((cpc_thread *)w));
}

/* cps int cpc_io_wait(int fd, int direction, cpc_condvar *cond) */
cps_expand3(cpc_io_wait, int, fd, int, direction, cpc_condvar *, cond)
    assert(cont);

    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        int rc = cpc_d_io_wait(fd, direction);
        cpc_continuation_patch(cont, sizeof(int), &rc);
        return cont;
    }

    assert(thread->condvar == NULL && thread->state == STATE_UNKNOWN);
    assert((fd & (7 << 29)) == 0);

    if(cond) {
        thread->condvar = cond;
        cond_enqueue(&cond->queue, thread);
    }

    thread->state = fd | ((direction & 3) << 29);
    assert(!ev_is_active(&thread->ev.w));
    ev_io_init(&thread->ev.io, io_wait_cb, fd, direction);
    ev_io_start(cpc_default_loop, &thread->ev.io);
    return NULL;
}

void
cpc_signal_fd(int fd, int direction)
{
    assert(!IS_DETACHED);
    ev_feed_fd_event(cpc_default_loop, fd, direction);
}

/*** cpc_attach ****/

cpc_continuation *cpc_prim_attach(cpc_thread*);
cpc_continuation *cpc_prim_detach(cpc_sched*, cpc_thread*);

/* cps cpc_sched *cpc_attach(cpc_sched *sched) */
cps_expand1(cpc_attach, cpc_sched *, sched)
    /* Return the previous scheduler */
    cpc_continuation_patch(cont, sizeof(cpc_sched *), &thread->sched);

    if(sched == cpc_default_sched)
        return cpc_prim_attach(thread);
    else
        return cpc_prim_detach(sched, thread);
}

static inline void
spawn_cb(void *closure)
{
    struct cpc_thread *thread = (struct cpc_thread *)closure;

    /* If the state is UNKNOWN, then the continuation comes from a
     * cpc_spawn. */
    assert((thread->state == STATE_UNKNOWN || thread->state == STATE_DETACHED)
        && thread->condvar == NULL);
    if(thread->state == STATE_DETACHED)
        thread->state = STATE_UNKNOWN;
    thread->sched = cpc_default_sched;
    schedule(thread); // XXX
}

static void
attach_cb(void *closure)
{
    spawn_cb(closure);
    detached_count--;
}

cpc_continuation *
cpc_prim_attach(cpc_thread *thread)
{
    if(thread->state == STATE_UNKNOWN) {
        assert(!IS_DETACHED && thread->condvar == NULL);
        assert(thread->sched == cpc_default_sched);
        return get_cont(thread);
    }

    assert(thread->state == STATE_DETACHED);
    threadpool_schedule_back(thread->sched->pool, attach_cb, thread);
    return NULL;
}

static void
perform_detach(void *closure)
{
    cpc_thread *thread = (cpc_thread *)closure;
    thread->state = STATE_DETACHED;
    cpc_invoke_continuation(get_cont(thread));
}

cpc_continuation *
cpc_prim_detach(cpc_sched *sched, cpc_thread *thread)
{
    int rc;

    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        /* Already in the good threadpool. */
        if(thread->sched == sched) {
            return get_cont(thread);
        }
    } else {
        assert(thread->state == STATE_UNKNOWN && thread->condvar == NULL);
        assert(!IS_DETACHED);
        if(detached_count == 0) { /* First detached thread */
            assert(!ev_is_active(&attach_watcher));
            ev_async_start(cpc_default_loop, &attach_watcher);
        }
        detached_count++;
        if(sched == NULL)
            sched = cpc_default_threadpool;
    }
    thread->sched = sched;
    rc = threadpool_schedule(sched->pool, perform_detach, (void *)thread);
    if (rc < 0) {
      perror("threadpool_schedule");
      exit(1);
    }
    return NULL;
}

void
wakeup(void *pp)
{
    ev_async_send(cpc_default_loop, &attach_watcher);
}

cpc_sched *
cpc_threadpool_get(int max_threads)
{
    cpc_sched_queue *q = &schedulers;

    assert(!IS_DETACHED);

    if(max_threads <= 0 || max_threads > MAX_THREADS)
        max_threads = MAX_THREADS;

    cpc_sched *sched = malloc(sizeof(cpc_sched));

    sched->pool = threadpool_create(max_threads, wakeup, NULL);
    sched->next = NULL;

    if(q->head == NULL) {
        q->head = q->tail = sched;
    } else {
        q->tail->next = sched;
        q->tail = sched;
    }
    return sched;
}

int
cpc_threadpool_release(cpc_sched *sched)
{
    cpc_sched *s = schedulers.head;
  
    if(threadpool_die(sched->pool, 0) < 0)
        return -1;
    if(threadpool_destroy(sched->pool) < 0)
        return -1;

    while(s) {
        if(s->next == sched) {
            s->next = sched->next;
            break;
        } else {
            s = s->next;
        }
    }
    free(sched);
    return 0;
}

static void
fetch_from_threadpools(struct ev_loop *loop, ev_async *w, int revents)
{
    assert(detached_count > 0);
    cpc_sched *s = schedulers.head;
    while(s) {
        threadpool_items_run(threadpool_get_back(s->pool));
        s = s->next;
    }
    if(detached_count == 0) /* Nobody else is detached */
        ev_async_stop(loop, w);
}


/*** cpc_yield and cpc_spawn ***/

/* cps void cpc_yield(void) */
cpc_continuation *
cpc_yield(struct cpc_continuation *cont)
{
    cpc_thread *thread = get_thread(cont);
    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        return cont;
    }

    assert(!IS_DETACHED && thread->state == STATE_UNKNOWN &&
        thread->condvar == NULL);

    schedule(thread);
    return NULL;
}


/* cps void cpc_done(void) */
cpc_continuation *
cpc_done(struct cpc_continuation *cont)
{
    cpc_continuation_free(cont);
    return NULL;
}

void
cpc_prim_spawn(struct cpc_continuation *cont, struct cpc_continuation *context)
{
    cpc_thread *thread = get_thread(cont);

    assert(thread->state == STATE_UNKNOWN && thread->condvar == NULL);
 
    /* If context is NULL, we have to perform a syscall to know if we
     * are detached or not */
    if(context == NULL && IS_DETACHED) {
        threadpool_schedule_back(thread->sched->pool, spawn_cb, thread);
    }
    /* Otherwise, trust the context */
    else if (context && get_thread(context)->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        threadpool_schedule_back(thread->sched->pool, spawn_cb, thread);
    }
    else {
        assert(!IS_DETACHED);
        // XXX Hack to initialise the loop on the first cpc_spawn
        if(!cpc_default_loop) {
            cpc_default_loop = ev_default_loop(0);
            ev_idle_init(&ready_watcher, schedule_cb);
            ev_set_priority(&ready_watcher, EV_MAXPRI);
            ev_async_init(&attach_watcher, fetch_from_threadpools);
        }
        schedule(thread);
    }
}

/*** Executing continuations with trampolines ***/

#if DEBUG
long long int invoke_count = 0;
#endif

static void
cpc_invoke_continuation(struct cpc_continuation *c)
{
    cpc_function *f;

    while(c) {
      if(c->length == 0) {
        cpc_continuation_free(c);
        return;
      }

      c->length -= PTR_SIZE;
      f = *(cpc_function**)(c->c + c->length);
      c = (*f)(c);
#if DEBUG
      invoke_count++;
#endif
    }
}

void
cpc_main_loop(void)
{
    assert(CPC_IO_IN == EV_READ);
    assert(CPC_IO_OUT == EV_WRITE);

    main_loop_id = pthread_self();

    cpc_default_threadpool = cpc_threadpool_get(MAX_THREADS);

    if(!cpc_default_loop) {
        cpc_default_loop = ev_default_loop(0);
        ev_idle_init(&ready_watcher, schedule_cb);
        ev_set_priority(&ready_watcher, EV_MAXPRI);
        ev_async_init(&attach_watcher, fetch_from_threadpools);
    }

    ev_run(cpc_default_loop, 0);

    while(cpc_threadpool_release(cpc_default_threadpool) < 0);
}

cpc_sched *
cpc_get_sched(cpc_continuation *cont) {
  return get_thread(cont)->sched;
}

int
cpc_gettimeofday(cpc_continuation *cont, struct timeval *tv)
{
    cpc_thread *thread = get_thread(cont);
    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        return gettimeofday(tv, NULL); /* XXX */
    }
    assert(thread->state == STATE_UNKNOWN && !IS_DETACHED);
    ev_tstamp now = ev_now(cpc_default_loop);
    tv->tv_sec = (time_t) now;
    tv->tv_usec = ((suseconds_t)(now * 1e6)) % 1000000;
    return 0;
}

time_t
cpc_time(cpc_continuation *cont, time_t *t)
{
    cpc_thread *thread = get_thread(cont);

    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        return time(t); /* XXX */
    }
    assert(thread->state == STATE_UNKNOWN && !IS_DETACHED);
    ev_tstamp now = ev_now(cpc_default_loop);
    if(t)
        *t = (time_t)now;
    return (time_t)now;
}
