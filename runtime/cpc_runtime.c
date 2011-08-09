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

typedef struct cpc_thread {
    struct cpc_thread *next;
    struct cpc_condvar *condvar;
    struct cpc_thread *cond_next;
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
static int p[2];

const int cpc_pessimise_runtime = 0;

typedef struct cpc_thread_queue {
    struct cpc_thread *head;
    struct cpc_thread *tail;
} cpc_thread_queue;

typedef struct cpc_timed_thread {
    cpc_thread *thread;
    struct timeval time;
    unsigned int heap_index;
} cpc_timed_thread;

typedef struct cpc_timed_thread_queue {
    cpc_timed_thread **heap;
    unsigned int size;
    unsigned int length;
} cpc_timed_thread_queue;

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
static cpc_timed_thread_queue sleeping = {NULL, 0, 0};
static cpc_sched_queue schedulers = {NULL, NULL};
static cpc_thread_queue *fd_queues = NULL;
static int num_fds = 0, size_fds = 0;
static fd_set readfds, writefds, exceptfds;

static struct timeval cpc_now;

static void recompute_fdsets(int fd);

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
        "  next: %p\tcondvar%p\tcond_next: %p\n"
        "  state: %d\tlength: %u\tsize: %u\n",
        s,c,
        t->next,t->condvar,t->cond_next,
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
        d->cond_next = NULL;
        d->next = NULL;
        d->sched = cpc_default_sched;
        d->state = STATE_UNKNOWN;
        return get_cont(d);
    }

    t = get_thread(c);

    memcpy(d->cont.c, c->c, c->length);

    d->cont.length = c->length;
    d->condvar = t->condvar;
    d->cond_next = t->cond_next;
    d->next = t->next;
    d->sched = t->sched;
    d->state = t->state;

    free(t);

    return get_cont(d);
}

/*** Basic operations on queues ***/

static void
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

static cpc_thread *
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
dequeue_1(cpc_thread_queue *queue, cpc_thread *thread)
{
    cpc_thread *t, *target;
    t = queue->head;
    if(t == thread) {
        queue->head = queue->head->next;
        thread->next = NULL;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(t->next != thread)
        t = t->next;
    target = t->next;
    t->next = target->next;
    target->next = NULL;
    if(t->next == NULL)
        queue->tail = t;
}

static void
cond_enqueue(cpc_thread_queue *queue, cpc_thread *t)
{
    assert(t);

    t->cond_next = NULL;

    if(queue->head == NULL) {
        queue->head = queue->tail = t;
    } else {
        queue->tail->cond_next = t;
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
    queue->head = t->cond_next;
    if(queue->head == NULL)
        queue->tail = NULL;
    t->cond_next = NULL;
    return t;
}    

static void
cond_dequeue_1(cpc_thread_queue *queue, cpc_thread *cont)
{
    cpc_thread *t, *target;
    t = queue->head;
    if(t == cont) {
        queue->head = queue->head->cond_next;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(t->cond_next != cont)
        t = t->cond_next;
    target = t->cond_next;
    t->cond_next = target->cond_next;
    target->cond_next = NULL;
    if(t->cond_next == NULL)
        queue->tail = t;
}

static int
timeval_cmp(struct timeval *t1, struct timeval *t2)
{
    if(t1->tv_sec < t2->tv_sec)
        return -1;
    else if(t1->tv_sec > t2->tv_sec)
        return +1;
    else if(t1->tv_usec < t2->tv_usec)
        return -1;
    else if(t1->tv_usec > t2->tv_usec)
        return +1;
    else
        return 0;
}

static void
timeval_plus(struct timeval *d, struct timeval *s1, int sec, int usec)
{
    d->tv_usec = s1->tv_usec + usec;
    d->tv_sec = s1->tv_sec + sec;
    if(d->tv_usec >= 1000000) {
        d->tv_sec += 1;
        d->tv_usec -= 1000000;
    }
}

static void
timeval_minus(struct timeval *d, struct timeval *s1, struct timeval *s2)
{
    if(s1->tv_usec >= s2->tv_usec) {
        d->tv_usec = s1->tv_usec - s2->tv_usec;
        d->tv_sec = s1->tv_sec - s2->tv_sec;
    } else {
        d->tv_usec = s1->tv_usec + 1000000 - s2->tv_usec;
        d->tv_sec = s1->tv_sec - s2->tv_sec - 1;
    }
    if(d->tv_sec < 0)
        d->tv_sec = d->tv_usec = 0;
}

#define UP(i) ((i-1)/2)
#define LEFT(i) (2*(i)+1)
#define RIGHT(i) (2*(i)+2)

static inline void
heap_expand(cpc_timed_thread_queue *heap)
{
    int length;
    cpc_timed_thread **h;

    length = 2 * heap->size;

    h = realloc(heap->heap, length * sizeof(cpc_timed_thread *));
    if(h)
        heap->heap = h;
    else
        {perror("realloc"); exit(1);}
    heap->length = length;
}

static inline void
heap_insert(cpc_timed_thread_queue *heap,
                cpc_timed_thread *tt)
{
    int i = tt->heap_index;
    struct timeval *time = &tt->time;

    if(heap->size > heap->length)
        heap_expand(heap);

    for(;i > 0 && timeval_cmp(&heap->heap[UP(i)]->time, time) > 0;
            i = UP(i)) {
        heap->heap[i] = heap->heap[UP(i)];
        assert(heap->heap[i]->heap_index == UP(i));
        heap->heap[i]->heap_index = i;
    }
    heap->heap[i] = tt;
    heap->heap[i]->heap_index = i;
}

/* Assume that heap[i]->time is infinite (hence it must end as a leaf of
 * the heap). */
static inline void
heapify_delete(cpc_timed_thread_queue *heap, unsigned int i)
{
    unsigned int l, r, min;

    cpc_timed_thread *tt = heap->heap[heap->size-1];
    tt->heap_index = heap->heap[i]->heap_index;
    --heap->size;
    heap_insert(heap, tt);
    tt = heap->heap[i];

    while(1) {
        l = LEFT(i);
        r = RIGHT(i);
        if(l < heap->size)
            if(r < heap->size)
                if(timeval_cmp(&heap->heap[l]->time, &heap->heap[r]->time) < 0)
                    min = l;
                else
                    min = r;
            else
                min = l;
        else
            break;
        if(timeval_cmp(&heap->heap[min]->time, &tt->time) > 0)
            break;
        heap->heap[i] = heap->heap[min];
        heap->heap[i]->heap_index = i;
        i = min;
    }
    heap->heap[i] = tt;
    tt->heap_index = i;
    heap->heap[heap->size] = NULL;
}

static inline void
heap_delete(cpc_timed_thread_queue *heap,
                cpc_timed_thread *tt) {
    assert(heap->size > 0 && tt->heap_index <= heap->size);
    heapify_delete(heap, tt->heap_index);
}
static void
timed_enqueue(cpc_timed_thread_queue *queue, cpc_thread *t,
              struct timeval *time)
{
    cpc_timed_thread *tt;

    assert(t);

    tt = malloc(sizeof(struct cpc_timed_thread));
    tt->thread = t;

    tt->time = *time;

    tt->heap_index = queue->size++;
    heap_insert(queue, tt);
    /* Trick: store a pointer to tt in the unused "next" field of t.
     * This is essential in timed_dequeue_1. */
    t->next = (cpc_thread *) tt;
}

static cpc_thread*
timed_dequeue(cpc_timed_thread_queue *queue, struct timeval *time)
{
    cpc_timed_thread *tt;
    cpc_thread *thread;

    if(queue->size == 0 || timeval_cmp(time, &queue->heap[0]->time) < 0)
        return NULL;

    tt = queue->heap[0];
    heap_delete(queue, tt);

    thread = tt->thread;
    free(tt);
    return thread;
}

void
timed_dequeue_1(cpc_timed_thread_queue *queue,
                cpc_thread *thread)
{
    /* See trick in timed_enqueue. */
    cpc_timed_thread *tt = (cpc_timed_thread *) thread->next;
    thread->next = NULL;
    heap_delete(queue, tt);
    free(tt);
}

static void
dequeue_other(cpc_thread *thread)
{
    if(thread->state == STATE_SLEEPING)
        timed_dequeue_1(&sleeping, thread);
    else if(thread->state >= 0 && (thread->state & ((1 << 29) - 1)) <= size_fds) {
        dequeue_1(&fd_queues[thread->state & ((1 << 29) - 1)], thread);
        recompute_fdsets(thread->state & ((1 << 29) - 1));
    } else
        assert(thread->state == STATE_UNKNOWN);
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
        thread = thread->cond_next;
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
    enqueue(&ready, thread);
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
        enqueue(&ready, thread);
    }
}

/*** cpc_sleep ***/

/* cps int cpc_sleep(int sec, int usec, cpc_condvar *cond) */
cps_expand3(cpc_sleep, int, sec, int, usec, cpc_condvar *, cond)
    struct timeval when;

    assert(cont);

    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        when.tv_sec = sec;
        when.tv_usec = usec;
        select(0, NULL, NULL, NULL, &when); // XXX
        int rc = CPC_TIMEOUT;
        cpc_continuation_patch(cont, sizeof(int), &rc);
        return cont;
    }

    assert(thread->condvar == NULL && thread->state == STATE_UNKNOWN);
    if(cond) {
        thread->condvar = cond;
        cond_enqueue(&cond->queue, thread);
    }
    thread->state = STATE_SLEEPING;
    timeval_plus(&when, &cpc_now, sec, usec);
    timed_enqueue(&sleeping, thread, &when);
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
fd_queues_expand(int n)
{
    cpc_thread_queue *new;

    assert(n >= size_fds && n <= FD_SETSIZE);
    new = malloc(n * sizeof(struct cpc_thread_queue));
    if(size_fds > 0) {
        memcpy(new, fd_queues,
               size_fds * sizeof(struct cpc_thread_queue));
    } else {
        FD_ZERO(&readfds);
        FD_ZERO(&writefds);
        FD_ZERO(&exceptfds);
    }

    memset(new + size_fds, 0,
           (n - size_fds) * sizeof(struct cpc_thread_queue));
    free(fd_queues);
    fd_queues = new;
    size_fds = n;
}

static void
recompute_fdsets(int fd)
{
    int r = 0, w = 0;
    cpc_thread *thread;

    thread = fd_queues[fd].head;
    while(thread) {
        if(((thread->state >> 29) & CPC_IO_IN))
            r = 1;
        if(((thread->state >> 29) & CPC_IO_OUT))
            w = 1;
        if(r && w)
            break;
        thread = thread->next;
    }
    if(r)
        FD_SET(fd, &readfds);
    else
        FD_CLR(fd, &readfds);
    if(w)
        FD_SET(fd, &writefds);
    else
        FD_CLR(fd, &writefds);
    if(r || w)
        FD_SET(fd, &exceptfds);
    else
        FD_CLR(fd, &exceptfds);

    if(r || w) {
        if(num_fds <= fd + 1)
            num_fds = fd + 1;
    } else if(fd == num_fds - 1) {
        while(!FD_ISSET(num_fds - 1, &readfds) &&
              !FD_ISSET(num_fds - 1, &writefds) &&
              !FD_ISSET(num_fds - 1, &exceptfds)) {
            num_fds--;
            if(num_fds == 0)
                break;
        }
    }
}
        
/* cps int cpc_io_wait(int fd, int direction, cpc_condvar *cond) */
cps_expand3(cpc_io_wait, int, fd, int, direction, cpc_condvar *, cond)
    int rc;

    assert(cont);

    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        rc = cpc_d_io_wait(fd, direction);
        cpc_continuation_patch(cont, sizeof(int), &rc);
        return cont;
    }

    assert(thread->condvar == NULL && thread->state == STATE_UNKNOWN);
    assert((fd & (7 << 29)) == 0);

    if(cond) {
        thread->condvar = cond;
        cond_enqueue(&cond->queue, thread);
    }
    if(size_fds <= fd)
        fd_queues_expand(fd + 10);

    thread->state = fd | ((direction & 3) << 29);
    enqueue(&fd_queues[fd], thread);
    recompute_fdsets(fd);
    return NULL;
}

static void
requeue_io_ready(int fd, int direction)
{
    cpc_thread *thread;
    thread = fd_queues[fd].head;
    while(thread) {
        if((thread->state >> 29) & direction) {
            dequeue_1(&fd_queues[fd], thread); /* XXX */
            thread->state = STATE_UNKNOWN;
            cpc_continuation_patch(get_cont(thread), sizeof(int), &direction);
            if(thread->condvar) {
                cond_dequeue_1(&thread->condvar->queue, thread);
            }
            thread->condvar = NULL;
            enqueue(&ready, thread);
        }
        thread = thread->next;
    }
    recompute_fdsets(fd);
}

void
cpc_signal_fd(int fd, int direction)
{
    assert(!IS_DETACHED);
    if(fd < size_fds)
        requeue_io_ready(fd, direction);
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
    enqueue(&ready, thread); // XXX
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
    int *p = (int*)pp;
    write(p[1], "a", 1);
}

cpc_sched *
cpc_threadpool_get(int max_threads)
{
    cpc_sched_queue *q = &schedulers;

    assert(!IS_DETACHED);

    if(max_threads <= 0 || max_threads > MAX_THREADS)
        max_threads = MAX_THREADS;

    cpc_sched *sched = malloc(sizeof(cpc_sched));

    sched->pool = threadpool_create(max_threads, wakeup, (void *)p);
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

    enqueue(&ready, thread);
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
        threadpool_schedule_back(get_thread(context)->sched->pool, spawn_cb, thread);
    }
    else {
        assert(!IS_DETACHED);
        enqueue(&ready, thread);
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

#define BUF_SIZE 4096

void
cpc_main_loop(void)
{
    struct cpc_thread *thread;
    struct cpc_thread_queue q;
    struct timeval when;
    int rc, i, done;
    fd_set r_readfds, r_writefds, r_exceptfds;
    char buf[BUF_SIZE];

    main_loop_id = pthread_self();

    cpc_default_threadpool = cpc_threadpool_get(MAX_THREADS);

    rc = pipe(p);
    if(rc < 0) {
        perror("pipe");
        exit(1);
    }
    for(i=0; i < 2; i++) {
        rc = fcntl(p[i], F_GETFL, 0);
        if(rc < 0) goto fail;
        rc = fcntl(p[i], F_SETFL, rc | O_NONBLOCK);
        if(rc < 0) goto fail;
        continue;
      fail:
        perror("fcntl");
        exit(1);
    }

    /* Make sure cpc_now has a reasonable initial value. */
    gettimeofday(&cpc_now, NULL);

#ifdef DEBUG
    struct timeval begin = cpc_now;
#endif

    while(ready.head || sleeping.size > 0 || num_fds > 0 ||
          detached_count > 0) {
        q = ready;
        ready.head = ready.tail = NULL;
        while(1) {
            thread = dequeue(&q);
            if(thread == NULL) break;
            assert(thread->state == STATE_UNKNOWN && thread->condvar == NULL);
            cpc_invoke_continuation(get_cont(thread));
        }
        if(ready.head && sleeping.size == 0 && num_fds == 0 &&
           detached_count == 0 && !cpc_pessimise_runtime)
            continue;

        gettimeofday(&cpc_now, NULL);

        rc = CPC_TIMEOUT;
        if(sleeping.size > 0) {
            while(1) {
                thread = timed_dequeue(&sleeping, &cpc_now);
                if(thread == NULL) break;
                assert(thread->state == STATE_SLEEPING);
                if(thread->condvar)
                    cond_dequeue_1(&thread->condvar->queue, thread);
                thread->condvar = NULL;
                thread->state = STATE_UNKNOWN;
                cpc_continuation *cont = get_cont(thread);
                cpc_continuation_patch(cont, sizeof(int), &rc);
                cpc_invoke_continuation(cont);
            }
        }

        if(ready.head && sleeping.size == 0 && num_fds == 0 &&
           detached_count == 0 && !cpc_pessimise_runtime)
            continue;

        memcpy(&r_readfds, &readfds, sizeof(fd_set));
        memcpy(&r_writefds, &writefds, sizeof(fd_set));
        memcpy(&r_exceptfds, &exceptfds, sizeof(fd_set));
        int old_num_fds = num_fds;
        if(detached_count > 0) {
            FD_SET(p[0], &r_readfds);
            num_fds = num_fds > p[0] + 1 ? num_fds : p[0] + 1;
        }
        gettimeofday(&cpc_now, NULL);
        if(ready.head || sleeping.size > 0) {
            if(ready.head) {
                when.tv_sec = 0;
                when.tv_usec = 0;
            } else {
                timeval_minus(&when, &sleeping.heap[0]->time, &cpc_now);
            }
            rc = select(num_fds, &r_readfds, &r_writefds, &r_exceptfds,
                        &when);
            num_fds = old_num_fds;
        } else {
            if(num_fds == 0)
                continue;
            rc = select(num_fds, &r_readfds, &r_writefds, &r_exceptfds,
                        NULL);
            num_fds = old_num_fds;
        }
        if(rc == 0 || (rc < 0 && errno == EINTR)) {
            continue;
        } else if(rc < 0) {
            perror("select");
            sleep(1);
            continue;
        }

        if(detached_count > 0 && FD_ISSET(p[0], &r_readfds)) {
            read(p[0], buf, BUF_SIZE);
            cpc_sched *s = schedulers.head;
            while(s) {
                threadpool_items_run(threadpool_get_back(s->pool));
                s = s->next;
            }
        }

        /* 1 for reads, 2 for writes.  0x100 means allow starvation. */
#ifndef PREFER
#define PREFER 3
#endif

        done = 0;
        for(i = 0; i < num_fds; i++) {
            int dir = 0;
            if(rc <= 0)
                break;
            if((PREFER & 1) && FD_ISSET(i, &r_readfds))
                dir |= CPC_IO_IN;
            if((PREFER & 2) && FD_ISSET(i, &r_writefds))
                dir |= CPC_IO_OUT;
            if(FD_ISSET(i, &r_exceptfds))
                dir |= (CPC_IO_IN | CPC_IO_OUT);
            if(dir) {
                requeue_io_ready(i, dir);
                rc--;
                done = 1;
            }
        }

#if (PREFER & 3) != 3
        if((PREFER & 0x100) && done)
            continue;
        for(i = 0; i < num_fds; i++) {
            int dir = 0;
            if(rc <= 0)
                break;
            if(FD_ISSET(i, &r_readfds))
                dir |= CPC_IO_IN;
            if(FD_ISSET(i, &r_writefds))
                dir |= CPC_IO_OUT;
            if(dir) {
                requeue_io_ready(i, dir);
                rc--;
            }
        }
#endif
    }

    while(cpc_threadpool_release(cpc_default_threadpool) < 0);
    close(p[0]);
    close(p[1]);

#ifdef DEBUG
    gettimeofday(&cpc_now, NULL);
    timeval_minus(&when, &cpc_now, &begin);
    fprintf(stderr, "Time spent in cpc_main_loop: %ld.%06ld\n"
        "Continuation invocations: %lld\n",
        when.tv_sec, when.tv_usec, invoke_count);
#endif

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
    *tv = cpc_now;
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
    if(t)
        *t = cpc_now.tv_sec;
    return cpc_now.tv_sec;
}
