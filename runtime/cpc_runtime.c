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
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <sys/time.h>
#include <fcntl.h>

#define NO_CPS_PROTO
#include "cpc_runtime.h"

#include <poll.h> /* cpc_d_io_wait */

#include "threadpool/threadpool.h"

struct cpc_sched {
    threadpool_t *pool;
    struct cpc_sched *next;
};
static pthread_t main_loop_id;
#define IS_DETACHED (cpc_default_pool && !pthread_equal(main_loop_id,pthread_self()))
#define MAX_THREADS 20
cpc_sched *cpc_default_pool = NULL;
static int p[2];

const int cpc_pessimise_runtime = 0;

typedef struct cpc_continuation_queue {
    struct cpc_continuation *head;
    struct cpc_continuation *tail;
} cpc_continuation_queue;

typedef struct cpc_timed_continuation {
    cpc_continuation *continuation;
    struct timeval time;
    unsigned int heap_index;
} cpc_timed_continuation;

typedef struct cpc_timed_continuation_queue {
    cpc_timed_continuation **heap;
    unsigned int size;
    unsigned int length;
} cpc_timed_continuation_queue;

typedef struct cpc_sched_queue {
    struct cpc_sched *head;
    struct cpc_sched *tail;
} cpc_sched_queue;

struct cpc_condvar {
    int refcount;
    cpc_continuation_queue queue;
};

#define STATE_UNKNOWN -1
#define STATE_SLEEPING -2
#define STATE_DETACHED -3

static cpc_continuation_queue ready = {NULL, NULL};
static cpc_timed_continuation_queue sleeping = {NULL, 0, 0};
static cpc_sched_queue schedulers = {NULL, NULL};
static cpc_continuation_queue *fd_queues = NULL;
static int num_fds = 0, size_fds = 0;
static fd_set readfds, writefds, exceptfds;

static struct timeval cpc_now;

static void recompute_fdsets(int fd);

static inline void exhaust_ready(cpc_continuation *);

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
    fprintf(stderr, "(%s) At %p %lu:\n%p\t%p\t%p\t%p\t%d\t%u\t%u\n",s,c,
    sizeof(cpc_function*),
    c->next,c->condvar,c->cond_next,c->ready,c->state,c->length,c->size);
    while(i < c->length) {
        fprintf(stderr,"%02x",c->c[i]);
        i++;
        if(i%16 == 0 || i == c->length)
            fprintf(stderr,"\n");
        else if(i%2 == 0)
            fprintf(stderr, " ");
    }
}

struct cpc_continuation *
cpc_continuation_get(int size)
{
    struct cpc_continuation *c;
    c = malloc(sizeof(struct cpc_continuation) + (size - 1));
    c->size = size;
    return c;
}

static void
free_detached_cb(void *closure)
{
    detached_count--;
}

void
cpc_continuation_free(struct cpc_continuation *c)
{
    assert((c->state == STATE_UNKNOWN || c->state == STATE_DETACHED) && c->condvar == NULL);
    if(c->state == STATE_DETACHED)
        threadpool_schedule_back(c->sched->pool, free_detached_cb, NULL);
    free(c);
}

cpc_continuation *
cpc_continuation_expand(struct cpc_continuation *c, int n)
{
    int size;
    cpc_continuation *d;

    if(c == (void*)0)
        size = n + 20;
    else
        size = c->size * 2 + n;

    d = cpc_continuation_get(size);

    if(c == (void*)0) {
        d->length = 0;
        d->condvar = NULL;
        d->cond_next = NULL;
        d->next = NULL;
        d->sched = cpc_default_sched;
        d->state = STATE_UNKNOWN;
        d->ready = NULL;
        return d;
    }

    memcpy(d->c, c->c, c->length);

    d->length = c->length;
    d->condvar = c->condvar;
    d->cond_next = c->cond_next;
    d->next = c->next;
    d->sched = c->sched;
    d->state = c->state;
    d->ready = c->ready;

    free(c);

    return d;
}

/*** Basic operations on queues ***/

static void
enqueue(cpc_continuation_queue *queue, cpc_continuation *c)
{
    if(c == NULL)
        c = cpc_continuation_expand(NULL, 0);

    c->next = NULL;

    if(queue->head == NULL) {
        queue->head = queue->tail = c;
    } else {
        queue->tail->next = c;
        queue->tail = c;
    }
}

/*
static void
enqueue_head(cpc_continuation_queue *queue, cpc_continuation *c)
{
    if(c == NULL)
        c = cpc_continuation_expand(NULL, 0);

    if(queue->head == NULL) {
        c->next = NULL;
        queue->head = queue->tail = c;
    } else {
        c->next = queue->head;
        queue->head = c;
    }
}
*/

static cpc_continuation *
dequeue(cpc_continuation_queue *queue)
{
    cpc_continuation *cont;
    if(queue->head == NULL)
        return NULL;
    cont = queue->head;
    queue->head = cont->next;
    if(queue->head == NULL)
        queue->tail = NULL;
    cont->next = NULL;
    return cont;
}    

static void
dequeue_1(cpc_continuation_queue *queue, cpc_continuation *cont)
{
    cpc_continuation *c, *target;
    c = queue->head;
    if(c == cont) {
        queue->head = queue->head->next;
        cont->next = NULL;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(c->next != cont)
        c = c->next;
    target = c->next;
    c->next = target->next;
    target->next = NULL;
    if(c->next == NULL)
        queue->tail = c;
}

static void
cond_enqueue(cpc_continuation_queue *queue, cpc_continuation *c)
{
    if(c == NULL)
        c = cpc_continuation_expand(NULL, 0);

    c->cond_next = NULL;

    if(queue->head == NULL) {
        queue->head = queue->tail = c;
    } else {
        queue->tail->cond_next = c;
        queue->tail = c;
    }
}

static cpc_continuation *
cond_dequeue(cpc_continuation_queue *queue)
{
    cpc_continuation *cont;
    if(queue->head == NULL)
        return NULL;
    cont = queue->head;
    queue->head = cont->cond_next;
    if(queue->head == NULL)
        queue->tail = NULL;
    cont->cond_next = NULL;
    return cont;
}    

static void
cond_dequeue_1(cpc_continuation_queue *queue, cpc_continuation *cont)
{
    cpc_continuation *c, *target;
    c = queue->head;
    if(c == cont) {
        queue->head = queue->head->cond_next;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(c->cond_next != cont)
        c = c->cond_next;
    target = c->cond_next;
    c->cond_next = target->cond_next;
    target->cond_next = NULL;
    if(c->cond_next == NULL)
        queue->tail = c;
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
heap_expand(cpc_timed_continuation_queue *heap)
{
    int length;
    cpc_timed_continuation **h;

    length = 2 * heap->size;

    h = realloc(heap->heap, length * sizeof(cpc_timed_continuation *));
    if(h)
        heap->heap = h;
    else
        {perror("realloc"); exit(1);}
    heap->length = length;
}

static inline void
heap_insert(cpc_timed_continuation_queue *heap,
                cpc_timed_continuation *tc)
{
    int i = tc->heap_index;
    struct timeval *time = &tc->time;

    if(heap->size > heap->length)
        heap_expand(heap);

    for(;i > 0 && timeval_cmp(&heap->heap[UP(i)]->time, time) > 0;
            i = UP(i)) {
        heap->heap[i] = heap->heap[UP(i)];
        assert(heap->heap[i]->heap_index == UP(i));
        heap->heap[i]->heap_index = i;
    }
    heap->heap[i] = tc;
    heap->heap[i]->heap_index = i;
}

/* Assume that heap[i]->time is infinite (hence it must end as a leaf of
 * the heap). */
static inline void
heapify_delete(cpc_timed_continuation_queue *heap, unsigned int i)
{
    unsigned int l, r, min;

    cpc_timed_continuation *tc = heap->heap[heap->size-1];
    tc->heap_index = heap->heap[i]->heap_index;
    --heap->size;
    heap_insert(heap, tc);
    tc = heap->heap[i];

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
        if(timeval_cmp(&heap->heap[min]->time, &tc->time) > 0)
            break;
        heap->heap[i] = heap->heap[min];
        heap->heap[i]->heap_index = i;
        i = min;
    }
    heap->heap[i] = tc;
    tc->heap_index = i;
    heap->heap[heap->size] = NULL;
}

static inline void
heap_delete(cpc_timed_continuation_queue *heap,
                cpc_timed_continuation *tc) {
    assert(heap->size > 0 && tc->heap_index <= heap->size);
    heapify_delete(heap, tc->heap_index);
}
static void
timed_enqueue(cpc_timed_continuation_queue *queue, cpc_continuation *c,
              struct timeval *time)
{
    cpc_timed_continuation *tc;

    if(c == NULL)
        c = cpc_continuation_expand(NULL, 0);

    tc = malloc(sizeof(struct cpc_timed_continuation));
    tc->continuation = c;

    tc->time = *time;

    tc->heap_index = queue->size++;
    heap_insert(queue, tc);
    /* Trick: store a pointer to tc in the unused "next" field of c.
     * This is essential in timed_dequeue_1. */
    c->next = (cpc_continuation *) tc;
}

static cpc_continuation*
timed_dequeue(cpc_timed_continuation_queue *queue, struct timeval *time)
{
    cpc_timed_continuation *tc;
    cpc_continuation *cont;

    if(queue->size == 0 || timeval_cmp(time, &queue->heap[0]->time) < 0)
        return NULL;

    tc = queue->heap[0];
    heap_delete(queue, tc);

    cont = tc->continuation;
    free(tc);
    return cont;
}

void
timed_dequeue_1(cpc_timed_continuation_queue *queue,
                cpc_continuation *cont)
{
    /* See trick in timed_enqueue. */
    cpc_timed_continuation *tc = (cpc_timed_continuation *) cont->next;
    cont->next = NULL;
    heap_delete(queue, tc);
    free(tc);
}

static void
dequeue_other(cpc_continuation *cont)
{
    if(cont->state == STATE_SLEEPING)
        timed_dequeue_1(&sleeping, cont);
    else if(cont->state >= 0 && (cont->state & ((1 << 29) - 1)) <= size_fds) {
        dequeue_1(&fd_queues[cont->state & ((1 << 29) - 1)], cont);
        recompute_fdsets(cont->state & ((1 << 29) - 1));
    } else
        assert(cont->state == STATE_UNKNOWN);
    cont->state = STATE_UNKNOWN;
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

    cpc_continuation *cont = cond->queue.head;
    int i = 0;

    cont = cond->queue.head;
    while(cont) {
        i++;
        cont = cont->next;
    }
    return i;
}

/* cps int cpc_wait(cpc_condvar *cond) */
struct cpc_wait_arglist {
   cpc_condvar *cond ;
} __attribute__((__packed__)) ;
void cpc_wait(struct cpc_continuation *cont)
{
    cpc_condvar *cond;
    struct cpc_wait_arglist *cpc_arguments ;

    cpc_arguments = (struct cpc_wait_arglist *) cpc_dealloc(cont,
                    (int )sizeof(struct cpc_wait_arglist ));
    cond = cpc_arguments->cond;

    assert(!IS_DETACHED && cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    cont->condvar = cond;
    cond_enqueue(&cond->queue, cont);
}

void
cpc_signal(cpc_condvar *cond)
{
    int rc = CPC_CONDVAR;
    assert(!IS_DETACHED);
    cpc_continuation *cont;
    cont = cond_dequeue(&cond->queue);
    if(cont == NULL)
        return;
    assert(cont->condvar == cond);
    cont->condvar = NULL;
    cpc_continuation_patch(cont, sizeof(int), &rc);
    dequeue_other(cont);
    enqueue(&ready, cont);
}

void
cpc_signal_all(cpc_condvar *cond)
{
    int rc = CPC_CONDVAR;
    cpc_continuation *cont;

    assert(!IS_DETACHED);
    while(1) {
        cont = cond_dequeue(&cond->queue);
        if(cont == NULL)
            break;
        assert(cont->condvar == cond);
        cont->condvar = NULL;
        cpc_continuation_patch(cont, sizeof(int), &rc);
        dequeue_other(cont);
        enqueue(&ready, cont);
    }
}

/*** cpc_sleep ***/

/* cps int cpc_sleep(int sec, int usec, cpc_condvar *c) */
struct cpc_sleep_arglist {
   int sec ;
   int usec ;
   cpc_condvar *cond ;
} __attribute__((__packed__)) ;
void cpc_sleep(struct cpc_continuation *cont)
{
    int sec, usec;
    cpc_condvar *cond;
    struct cpc_sleep_arglist *cpc_arguments ;

    cpc_arguments = (struct cpc_sleep_arglist *) cpc_dealloc(cont,
                    (int )sizeof(struct cpc_sleep_arglist ));
    sec = cpc_arguments->sec;
    usec = cpc_arguments->usec;
    cond = cpc_arguments->cond;

    struct timeval when;

    if(cont == NULL)
        cont = cpc_continuation_expand(NULL, 0);

    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        when.tv_sec = sec;
        when.tv_usec = usec;
        select(0, NULL, NULL, NULL, &when); // XXX
        int rc = CPC_TIMEOUT;
        cpc_continuation_patch(cont, sizeof(int), &rc);
        cpc_invoke_continuation(cont);
        return;
    }

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    cont->state = STATE_SLEEPING;
    timeval_plus(&when, &cpc_now, sec, usec);
    timed_enqueue(&sleeping, cont, &when);
}

/*** cpc_io_wait ***/

static inline int
cpc_d_io_wait(int fd, int direction)
{
    struct pollfd pfd[1];
    int pollevent = 0;
    int rc;

    pollevent |= (direction | CPC_IO_OUT) ? POLLOUT : 0;
    pollevent |= (direction | CPC_IO_IN) ? POLLIN : 0;

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
    cpc_continuation_queue *new;

    assert(n >= size_fds && n <= FD_SETSIZE);
    new = malloc(n * sizeof(struct cpc_continuation_queue));
    if(size_fds > 0) {
        memcpy(new, fd_queues,
               size_fds * sizeof(struct cpc_continuation_queue));
    } else {
        FD_ZERO(&readfds);
        FD_ZERO(&writefds);
        FD_ZERO(&exceptfds);
    }

    memset(new + size_fds, 0,
           (n - size_fds) * sizeof(struct cpc_continuation_queue));
    free(fd_queues);
    fd_queues = new;
    size_fds = n;
}

static void
recompute_fdsets(int fd)
{
    int r = 0, w = 0;
    cpc_continuation *cont;

    cont = fd_queues[fd].head;
    while(cont) {
        if(((cont->state >> 29) & CPC_IO_IN))
            r = 1;
        if(((cont->state >> 29) & CPC_IO_OUT))
            w = 1;
        if(r && w)
            break;
        cont = cont->next;
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
        
/* cps int cpc_io_wait(int fd, int direction, cpc_condvar *c) */
struct cpc_io_wait_arglist {
   int fd ;
   int direction ;
   cpc_condvar *cond ;
} __attribute__((__packed__)) ;
void cpc_io_wait(struct cpc_continuation *cont)
{
    int fd, direction, rc;
    cpc_condvar *cond;
    struct cpc_io_wait_arglist *cpc_arguments ;

    cpc_arguments = (struct cpc_io_wait_arglist *) cpc_dealloc(cont,
                    (int )sizeof(struct cpc_io_wait_arglist ));
    fd = cpc_arguments->fd;
    direction = cpc_arguments->direction;
    cond = cpc_arguments->cond;


    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        rc = cpc_d_io_wait(fd, direction);
        cpc_continuation_patch(cont, sizeof(int), &rc);
        cpc_invoke_continuation(cont);
        return;
    }

    if(cont == NULL)
        return;

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    assert((fd & (7 << 29)) == 0);

    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    if(size_fds <= fd)
        fd_queues_expand(fd + 10);

    cont->state = fd | ((direction & 3) << 29);
    enqueue(&fd_queues[fd], cont);
    recompute_fdsets(fd);
}

static void
requeue_io_ready(int fd, int direction)
{
    cpc_continuation *c;
    c = fd_queues[fd].head;
    while(c) {
        if((c->state >> 29) & direction) {
            dequeue_1(&fd_queues[fd], c); /* XXX */
            c->state = STATE_UNKNOWN;
            cpc_continuation_patch(c, sizeof(int), &direction);
            if(c->condvar) {
                cond_dequeue_1(&c->condvar->queue, c);
            }
            c->condvar = NULL;
            enqueue(&ready, c);
        }
        c = c->next;
    }
    recompute_fdsets(fd);
}

void
cpc_signal_fd(int fd, int direction)
{
    assert(!IS_DETACHED);
    requeue_io_ready(fd, direction);
}

/*** cpc_attach ****/

void cpc_prim_attach(cpc_continuation*);
void cpc_prim_detach(cpc_sched*, cpc_continuation*);

/* cps cpc_sched *cpc_attach(cpc_sched *pool) */
struct cpc_attach_arglist {
   cpc_sched *sched;
} __attribute__((__packed__)) ;
void
cpc_attach(struct cpc_continuation *cont)
{
    cpc_sched *sched;
    struct cpc_attach_arglist *cpc_arguments ;

    cpc_arguments = (struct cpc_attach_arglist *) cpc_dealloc(cont,
                    (int )sizeof(struct cpc_attach_arglist ));
    sched = cpc_arguments->sched;

    /* Return the previous scheduler */
    cpc_continuation_patch(cont, sizeof(cpc_sched *), &cont->sched);

    if(sched == cpc_default_sched)
        cpc_prim_attach(cont);
    else
        cpc_prim_detach(sched, cont);
}

static inline void
spawn_cb(void *cont)
{
    struct cpc_continuation *c = (struct cpc_continuation *)cont;

    /* If the state is UNKNOWN, then the continuation comes from a
     * cpc_spawn. */
    assert((c->state == STATE_UNKNOWN || c->state == STATE_DETACHED)
        && c->condvar == NULL);
    if(c->state == STATE_DETACHED)
        c->state = STATE_UNKNOWN;
    c->sched = cpc_default_sched;
    enqueue(&ready, c); // XXX Or maybe: exhaust_ready(c); ???
}

static void
attach_cb(void *cont)
{
    spawn_cb(cont);
    detached_count--;
}

void
cpc_prim_attach(cpc_continuation *cont)
{
    if(cont->state == STATE_UNKNOWN) {
        assert(!IS_DETACHED && cont->condvar == NULL);
        assert(cont->sched == cpc_default_sched);
        cpc_invoke_continuation(cont);
        return;
    }

    assert(cont->state == STATE_DETACHED);
    threadpool_schedule_back(cont->sched->pool, attach_cb, cont);
}

static void
perform_detach(void *cont)
{
    struct cpc_continuation *c = (struct cpc_continuation *)cont;
    c->state = STATE_DETACHED;
    exhaust_ready(c);
}

void
cpc_prim_detach(cpc_sched *sched, cpc_continuation *cont)
{
    int rc;

    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        /* Already in the good threadpool. */
        if(cont->sched == sched) {
            cpc_invoke_continuation(cont);
            return;
        }
    } else {
        assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
        assert(!IS_DETACHED);
        detached_count++;
        if(sched == NULL)
            sched = cpc_default_pool;
    }
    cont->sched = sched;
    rc = threadpool_schedule(sched->pool, perform_detach, (void *)cont);
    if (rc < 0) {
      perror("threadpool_schedule");
      exit(1);
    }
    return;
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

/* cps void cpc_yield(int mode) */
struct cpc_yield_arglist {
   int mode;
} __attribute__((__packed__)) ;
void cpc_yield(struct cpc_continuation *cont)
{
    int mode;
    struct cpc_yield_arglist *cpc_arguments ;

    cpc_arguments = (struct cpc_yield_arglist *) cpc_dealloc(cont,
                    (int )sizeof(struct cpc_yield_arglist ));
    mode = cpc_arguments->mode;

    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        cpc_invoke_continuation(cont);
        return;
    }

    assert(!IS_DETACHED && cont->state == STATE_UNKNOWN &&
        cont->condvar == NULL);

    enqueue(&ready, cont);
}


/* cps void cpc_done(void) */
void
cpc_done(struct cpc_continuation *cont)
{
    cpc_continuation_free(cont);
}

void
cpc_prim_spawn(struct cpc_continuation *cont, struct cpc_continuation *context)
{
    assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
 
    /* If context is NULL, we have to perform a syscall to know if we
     * are detached or not */
    if(context == NULL && IS_DETACHED) {
        threadpool_schedule_back(cont->sched->pool, spawn_cb, cont);
    }
    /* Otherwise, trust the context */
    else if (context && context->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        threadpool_schedule_back(cont->sched->pool, spawn_cb, cont);
    }
    else {
        assert(!IS_DETACHED);
        enqueue(&ready, cont);
    }
}

/*** Executing continuations with trampolines ***/

static inline void
exhaust_ready(cpc_continuation *c)
{
    cpc_continuation *cpc_ready_1 = c;
    c->ready = &cpc_ready_1;
    while(cpc_ready_1) {
        c = cpc_ready_1;
        cpc_ready_1 = NULL;
        cpc_really_invoke_continuation(c);
    }
}

#define BUF_SIZE 4096

void
cpc_main_loop(void)
{
    struct cpc_continuation *c;
    struct cpc_continuation_queue q;
    struct timeval when;
    int rc, i, done;
    fd_set r_readfds, r_writefds, r_exceptfds;
    char buf[BUF_SIZE];

    main_loop_id = pthread_self();

    cpc_default_pool = cpc_threadpool_get(MAX_THREADS);

    rc = pipe2(p, O_NONBLOCK);
    if(rc < 0) {
        perror("pipe");
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
            c = dequeue(&q);
            if(c == NULL) break;
            assert(c->state == STATE_UNKNOWN && c->condvar == NULL);
            exhaust_ready(c);
        }
        if(ready.head && sleeping.size == 0 && num_fds == 0 &&
           detached_count == 0 && !cpc_pessimise_runtime)
            continue;

        gettimeofday(&cpc_now, NULL);

        rc = CPC_TIMEOUT;
        if(sleeping.size > 0) {
            while(1) {
                c = timed_dequeue(&sleeping, &cpc_now);
                if(c == NULL) break;
                assert(c->state == STATE_SLEEPING);
                if(c->condvar)
                    cond_dequeue_1(&c->condvar->queue, c);
                c->condvar = NULL;
                c->state = STATE_UNKNOWN;
                cpc_continuation_patch(c, sizeof(int), &rc);
                exhaust_ready(c);
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

    while(cpc_threadpool_release(cpc_default_pool) < 0);
    close(p[0]);
    close(p[1]);

#ifdef DEBUG
    gettimeofday(&cpc_now, NULL);
    timeval_minus(&when, &cpc_now, &begin);
    fprintf(stderr, "Time spent in cpc_main_loop: %ld.%06ld\n",
        when.tv_sec, when.tv_usec);
#endif

}

double
cpc_gettime(void)
{
    assert(!IS_DETACHED);
    return ((double) cpc_now.tv_sec + 1E6 * cpc_now.tv_usec);
}
