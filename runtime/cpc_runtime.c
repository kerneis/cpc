/* 
Copyright (c) 2004, 2005 by Juliusz Chroboczek.
Experimental; do not redistribute.
*/

#include <sys/time.h>
#include <time.h>
#include <sys/select.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "cpc_runtime.h"

const int cpc_pessimise_runtime = 0;

typedef struct cpc_continuation_queue {
    struct cpc_continuation *head;
    struct cpc_continuation *tail;
} cpc_continuation_queue;

typedef struct cpc_timed_continuation {
    cpc_continuation *continuation;
    struct timeval time;
    struct cpc_timed_continuation *next;
} cpc_timed_continuation;

typedef struct cpc_timed_continuation_queue {
    struct cpc_timed_continuation *head;
    struct cpc_timed_continuation *tail;
} cpc_timed_continuation_queue;

struct cpc_condvar {
    int refcount;
    cpc_continuation_queue queue;
};

#define STATE_UNKNOWN -1
#define STATE_SLEEPING -2

cpc_continuation *cpc_ready_1;

static cpc_continuation_queue ready = {NULL, NULL};
static cpc_timed_continuation_queue sleeping = {NULL, NULL};
static cpc_continuation_queue *fd_queues = NULL;
static int num_fds = 0, size_fds = 0;
static fd_set readfds, writefds, exceptfds;

static struct timeval cpc_now;

static void recompute_fdsets(int fd);

struct cpc_continuation *
cpc_continuation_get(int size)
{
    struct cpc_continuation *c;
    c = malloc(sizeof(struct cpc_continuation) + (size - 1));
    c->size = size;
    return c;
}

void
cpc_continuation_free(struct cpc_continuation *c)
{
    assert(c->state == STATE_UNKNOWN && c->condvar == NULL);
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
        d->state = STATE_UNKNOWN;
        return d;
    }

    memcpy(d->c, c->c, c->length);

    d->length = c->length;
    d->condvar = c->condvar;
    d->cond_next = c->cond_next;
    d->state = c->state;
    free(c);

    return d;
}

struct cpc_continuation *
cpc_continuation_copy(struct cpc_continuation *c)
{
    int i;
    cpc_continuation *d;

    if(c == NULL) return NULL;

    d = cpc_continuation_get(c->size);
    for(i = 0; i < c->length; i++)
        d->c[i] = c->c[i];
    d->length = c->length;
    d->condvar = c->condvar;
    d->state = c->state;

    return d;
}

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
    cpc_continuation *c, *next;
    c = queue->head;
    if(c == cont) {
        queue->head = queue->head->next;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(c->next != cont)
        c = c->next;
    next = c->next;
    c->next = c->next->next;
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
    cpc_continuation *c, *next;
    c = queue->head;
    if(c == cont) {
        queue->head = queue->head->cond_next;
        if(queue->head == NULL)
            queue->tail = NULL;
        return;
    }
    while(c->cond_next != cont)
        c = c->cond_next;
    next = c->cond_next;
    c->cond_next = c->cond_next->cond_next;
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
    if(s1->tv_usec > s2->tv_usec) {
        d->tv_usec = s1->tv_usec - s2->tv_usec;
        d->tv_sec = s1->tv_sec - s2->tv_sec;
    } else {
        d->tv_usec = s1->tv_usec + 1000000 - s2->tv_usec;
        d->tv_sec = s1->tv_sec - s2->tv_sec - 1;
    }
    if(d->tv_sec < 0)
        d->tv_sec = d->tv_usec = 0;
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

    if(queue->head == NULL ||
       timeval_cmp(time, &queue->head->time) < 0) {
        /* Insert at head */
        tc->next = queue->head;
        queue->head = tc;
        if(tc->next == NULL)
            queue->tail = tc;
    } else if(timeval_cmp(time, &queue->tail->time) >= 0) {
        /* Insert at tail */
        tc->next = NULL;
        queue->tail->next = tc;
        queue->tail = tc;
    } else {
        cpc_timed_continuation *other;
        other = queue->head;
        while(timeval_cmp(time, &other->next->time) >= 0)
            other = other->next;
        tc->next = other->next;
        other->next = tc->next;
    }
}

static cpc_continuation*
timed_dequeue(cpc_timed_continuation_queue *queue, struct timeval *time)
{
    cpc_timed_continuation *tc;
    cpc_continuation *cont;

    if(queue->head == NULL || timeval_cmp(time, &queue->head->time) < 0)
        return NULL;

    tc = queue->head;
    queue->head = tc->next;
    if(queue->head == NULL)
        queue->tail = NULL;
    cont = tc->continuation;
    free(tc);
    return cont;
}

void
timed_dequeue_1(cpc_timed_continuation_queue *queue,
                cpc_continuation *cont)
{
    cpc_timed_continuation *tc, *next;
    tc = queue->head;
    if(tc->continuation == cont) {
        queue->head = queue->head->next;
        if(queue->head == NULL)
            queue->tail = NULL;
        free(tc);
        return;
    }
    while(tc->next->continuation != cont)
        tc = tc->next;
    next = tc->next;
    tc->next = tc->next->next;
    if(tc->next == NULL)
        queue->tail = tc;
    free(next);
}

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
    cond->refcount++;
    return cond;
}

void
cpc_condvar_release(cpc_condvar *cond)
{
    assert(cond->refcount > 0);
    cond->refcount--;
    if(cond->refcount == 0) {
        assert(cond->queue.head == NULL);
        free(cond);
    }
}

void
cpc_schedule(struct cpc_continuation *cont)
{
    enqueue(&ready, cont);
}

void
cpc_prim_wait(cpc_condvar *cond, cpc_continuation *cont)
{
    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    cont->condvar = cond;
    cond_enqueue(&cond->queue, cont);
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

void
cpc_signal(cpc_condvar *cond)
{
    cpc_continuation *cont;
    cont = cond_dequeue(&cond->queue);
    if(cont == NULL)
        return;
    assert(cont->condvar == cond);
    cont->condvar = NULL;
    dequeue_other(cont);
    enqueue(&ready, cont);
}

void 
cpc_signal_all(cpc_condvar *cond)
{
    cpc_continuation *cont;

    while(1) {
        cont = cond_dequeue(&cond->queue);
        if(cont == NULL)
            break;
        assert(cont->condvar == cond);
        cont->condvar = NULL;
        dequeue_other(cont);
        enqueue(&ready, cont);
    }
}

void
cpc_prim_sleep(int sec, int usec, cpc_condvar *cond, cpc_continuation *cont)
{
    struct timeval when;

    if(cont == NULL)
        cont = cpc_continuation_expand(NULL, 0);

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    cont->state = STATE_SLEEPING;
    timeval_plus(&when, &cpc_now, sec, usec);
    timed_enqueue(&sleeping, cont, &when);
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
        
void
cpc_prim_io_wait(int fd, int direction, cpc_condvar *cond,
                 cpc_continuation *cont)
{
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

    return;
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

static inline void
exhaust_ready_1()
{
    cpc_continuation *c;
    while(cpc_ready_1) {
        c = cpc_ready_1;
        cpc_ready_1 = NULL;
        cpc_really_invoke_continuation(c);
    }
}

void 
cpc_main_loop(void)
{
    struct cpc_continuation *c;
    struct cpc_continuation_queue q;
    struct timeval when;
    int rc, i, done;
    fd_set r_readfds, r_writefds, r_exceptfds;

    cpc_ready_1 = NULL;

    /* Make sure cpc_now has a reasonable initial value. */
    gettimeofday(&cpc_now, NULL);

    while(ready.head || sleeping.head || num_fds > 0) {
        q = ready;
        ready.head = ready.tail = NULL;
        while(1) {
            c = dequeue(&q);
            if(c == NULL) break;
            assert(c->state == STATE_UNKNOWN && c->condvar == NULL);
            cpc_really_invoke_continuation(c);
            exhaust_ready_1();
        }
        if(ready.head && !sleeping.head && num_fds == 0 &&
           !cpc_pessimise_runtime)
            continue;

        gettimeofday(&cpc_now, NULL);

        if(sleeping.head) {
            while(1) {
                c = timed_dequeue(&sleeping, &cpc_now);
                if(c == NULL) break;
                assert(c->state == STATE_SLEEPING);
                if(c->condvar)
                    cond_dequeue_1(&c->condvar->queue, c);
                c->condvar = NULL;
                c->state = STATE_UNKNOWN;
                cpc_really_invoke_continuation(c);
                exhaust_ready_1();
            }
        }

        if(ready.head && !sleeping.head && num_fds == 0 &&
           !cpc_pessimise_runtime)
            continue;

        memcpy(&r_readfds, &readfds, sizeof(fd_set));
        memcpy(&r_writefds, &writefds, sizeof(fd_set));
        memcpy(&r_exceptfds, &exceptfds, sizeof(fd_set));
        gettimeofday(&cpc_now, NULL);
        if(ready.head || sleeping.head) {
            if(ready.head) {
                when.tv_sec = 0;
                when.tv_usec = 0;
            } else {
                timeval_minus(&when, &sleeping.head->time, &cpc_now);
            }
            rc = select(num_fds, &r_readfds, &r_writefds, &r_exceptfds,
                        &when);
        } else {
            if(num_fds == 0)
                break;
            rc = select(num_fds, &r_readfds, &r_writefds, &r_exceptfds,
                        NULL);
        }
        if(rc == 0 || (rc < 0 && errno == EINTR)) {
            continue;
        } else if(rc < 0) {
            perror("select");
            sleep(1);
            continue;
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
}
