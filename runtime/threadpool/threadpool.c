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

#include "compatibility.h"
#include "threadpool.h"

#define IOCPKEY_ITEM  0
#define IOCPKEY_DYING 1

static DWORD WINAPI thread_main(void *pool);

#ifndef USE_ATOMIC_INTRINSICS

/* This should be safe, since every lock will act as a memory barrier. */

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

typedef struct threadpool_queue {
    threadpool_item_t *first;
    threadpool_item_t *last;
} threadpool_queue_t;

struct threadpool {
    int maxthreads;
    volatile long threads;
    /* Set when we request that all threads die. */
    int dying;
    int paquets_number;
    HANDLE completionPort;
    threadpool_func_t *wakeup;
    void *wakeup_closure;
};

threadpool_t *
threadpool_create(int maxthreads,
                  threadpool_func_t *wakeup, void *wakeup_closure)
{
    HANDLE thread;
    threadpool_t *tp;
    int tmp;
    tp = calloc(1, sizeof(threadpool_t));
    if(tp == NULL)
        return NULL;

    tp->threads = maxthreads;
    tp->maxthreads = maxthreads;
    tp->wakeup = wakeup;
    tp->wakeup_closure = wakeup_closure;
    assert(tp->dying == 0);
    /* En vrai, la bonne valeur de concurrence serait à préciser en argument. */
    tp->completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
    if(tp->completionPort == NULL) {
        return NULL;
    }
    tmp = 0;
    while(maxthreads-- > 0) {
        thread = CreateThread(NULL, 0, thread_main, (void*) tp,
                              0, NULL);
        if(thread == NULL) {
            print_error("Unable to create all the requested threads");
            if(tmp > 0) {
                tp->threads = tmp;
                return tp;
            } else {
                goto fail;
            }
        }
        CloseHandle(thread);
    }
    return tp;
 fail:
    free(tp);
    return NULL;
}

int
threadpool_die(threadpool_t *threadpool, int canblock)
{
    int i = threadpool->threads;
    threadpool->dying = 1;
    while(i-- > 0)
        PostQueuedCompletionStatus(threadpool->completionPort, 0,
                                   IOCPKEY_DYING, NULL);
    return 0;
}

int
threadpool_destroy(threadpool_t *threadpool)
{
    int dead = threadpool->dying && threadpool->threads == 0;
    if(!dead) return -1;
    CloseHandle(threadpool->completionPort);
    free(threadpool);
    return 1;
}

static DWORD WINAPI
thread_main(void *pool)
{
    threadpool_t *threadpool = pool;
    threadpool_item_t *item;
    threadpool_func_t *func;
    void *closure;
    int rc;
    DWORD nbytes;
    DWORD completionKey;
    OVERLAPPED *poverlapped;

    while(1) {
        rc = GetQueuedCompletionStatus(threadpool->completionPort,
                                       &nbytes,
                                       &completionKey,
                                       &poverlapped,
                                       threadpool->dying ? 0 : INFINITE);
        if(!rc) {
            if(poverlapped != NULL) {
                print_error("GetQueuedCompletionStatus");
                /* impossible, since we only use PostQCP */
                continue;
            }
            goto die; /* timeout: no more paquets */
        }

        switch(completionKey) {
        case IOCPKEY_ITEM:
            item = (void*)poverlapped;
            func = item->func;
            closure = item->closure;
            free(item);
            func(closure);
            break;
        case IOCPKEY_DYING:
            goto die;
        default: assert(0);
        }
    }

 die:
    InterlockedDecrement(&threadpool->threads);
    return 0;
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

int
threadpool_schedule(threadpool_t *threadpool,
                    threadpool_func_t *func, void *closure)
{
    threadpool_item_t *item;

    item = threadpool_item_alloc(func, closure);
    if(item == NULL)
        return -1;

    PostQueuedCompletionStatus(threadpool->completionPort, 0,
                               IOCPKEY_ITEM, (void*)item);
    return 1;
}

int
threadpool_schedule_back(threadpool_t *threadpool,
                         threadpool_func_t *func, void *closure)
{
    threadpool->wakeup(closure); /* closure is a cpc_thread */
    return 0;
}
