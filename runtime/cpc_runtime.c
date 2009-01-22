/* 
Copyright (c) 2004, 2005 by Juliusz Chroboczek.
Experimental; do not redistribute.
*/

#define EPOLL_MAX_EVENTS 1024

#include <sys/time.h>
#include <sys/epoll.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "cpc_runtime.h"

#include "wp.c"

const int cpc_pessimise_runtime = 0;

#define STATE_UNKNOWN -1
#define STATE_SLEEPING -2

static wp_t *pool = NULL;
static pthread_mutex_t pool_m = PTHREAD_MUTEX_INITIALIZER;

cpc_scheduler cpc_main_scheduler = {
    NULL, // ready_1
    {NULL, NULL}, // ready
    {NULL, NULL}, // sleeping
    NULL, // fd_queues
    0, 0, 0, // epfd, num_fds, size_fds
    {NULL, NULL}, // attaching
    PTHREAD_MUTEX_INITIALIZER, // attach_m
    {0,0} // now
};

static void recompute_dequeue(int fd, cpc_scheduler *sched);

struct cpc_continuation *
cpc_continuation_get(int size)
{
    struct cpc_continuation *c;
    c = malloc(sizeof(struct cpc_continuation) + (size - 1));
    c->size = size;
    c->sched = &cpc_main_scheduler;
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
    d->sched = c->sched;
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
    /* XXX Why is c->cond_next not copied? */
    d->sched = c->sched;
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

cpc_scheduler *
cpc_scheduler_new(void)
{
    cpc_scheduler *sched;
    sched = malloc(sizeof(cpc_scheduler));
    sched->ready_1 = NULL;
    sched->ready.head = sched->ready.tail = NULL;
    sched->sleeping.head = sched->sleeping.tail = NULL;
    sched->attaching.head = sched->attaching.tail = NULL;
    sched->fd_queues = NULL;
    sched->num_fds = 0;
    sched->size_fds = 0;
    pthread_mutex_init(&sched->attach_m, NULL);
    gettimeofday(&sched->now, NULL);

    sched->epfd = epoll_create(10);
    if(sched->epfd < 0) {
        perror("epoll_create");
        exit(1);
    }

    return sched;
}

void
cpc_scheduler_free(cpc_scheduler *sched)
{
    /* TODO: Queues clean-up and sanity checks */
    close(sched->epfd);
    pthread_mutex_destroy(&sched->attach_m);
    if(sched != &cpc_main_scheduler)
        free(sched);
    return;
}

void
cpc_schedule(struct cpc_continuation *cont)
{
    if(cont->sched)
        enqueue(&cont->sched->ready, cont);
    else
        cpc_prim_detach(cont);

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
        timed_dequeue_1(&cont->sched->sleeping, cont);
    else if(cont->state >= 0 && (cont->state & ((1 << 29) - 1)) <= cont->sched->size_fds) {
        dequeue_1(&cont->sched->fd_queues[cont->state & ((1 << 29) - 1)], cont);
        recompute_dequeue(cont->state & ((1 << 29) - 1), cont->sched);
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
    enqueue(&cont->sched->ready, cont);
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
        enqueue(&cont->sched->ready, cont);
    }
}

void
cpc_prim_sleep(int sec, int usec, cpc_condvar *cond, cpc_continuation *cont)
{
    struct timeval when;

    if(cont == NULL)
        cont = cpc_continuation_expand(NULL, 0);

    if(cont->sched == NULL) {
        sleep(sec);
        usleep(usec);
        cpc_invoke_continuation(cont);
        return;
    }

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    cont->state = STATE_SLEEPING;
    timeval_plus(&when, &cont->sched->now, sec, usec);
    timed_enqueue(&cont->sched->sleeping, cont, &when);
}

static void
fd_queues_expand(int n, cpc_scheduler *sched)
{
    cpc_continuation_queue *new;
    cpc_continuation_queue *fd_queues = sched->fd_queues;

    int size_fds = sched->size_fds;

    assert(n >= size_fds);
    new = malloc(n * sizeof(struct cpc_continuation_queue));
    if(size_fds > 0) {
        memcpy(new, fd_queues,
               size_fds * sizeof(struct cpc_continuation_queue));
    }

    memset(new + size_fds, 0,
           (n - size_fds) * sizeof(struct cpc_continuation_queue));
    free(fd_queues);
    sched->fd_queues = new;
    sched->size_fds = n;
}

static void
recompute_enqueue(int fd, cpc_continuation *c)
{
    int r = 0, w = 0, op = EPOLL_CTL_MOD, rc = 0;
    cpc_continuation *cont;
    cpc_continuation_queue *fd_queues = c->sched->fd_queues;
    struct epoll_event event;
    event.events = 0;
    event.data.fd = fd;

    if(fd_queues[fd].head == NULL)
        op = EPOLL_CTL_ADD;
    enqueue(&fd_queues[fd], c);

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
        event.events |= (EPOLLIN | EPOLLPRI | EPOLLRDHUP);
    if(w)
        event.events |= EPOLLOUT;

    assert(r || w);
    if(c->sched->num_fds <= fd + 1)
        c->sched->num_fds = fd + 1;

    rc = epoll_ctl(c->sched->epfd, op, fd, &event);
    if(rc < 0)
        perror("epoll_ctl");
}

static void
recompute_dequeue(int fd, cpc_scheduler *sched)
{
    int r = 0, w = 0, op = EPOLL_CTL_MOD, rc = 0;
    cpc_continuation *cont;
    cpc_continuation_queue *fd_queues = sched->fd_queues;
    struct epoll_event event;
    event.events = 0;
    event.data.fd = fd;

    if(fd_queues[fd].head == NULL) {
        op = EPOLL_CTL_DEL;
        assert(fd_queues[sched->num_fds - 1].head != NULL || fd == sched->num_fds - 1);
        while(fd_queues[sched->num_fds - 1].head == NULL) {
            sched->num_fds--;
            if(sched->num_fds == 0)
                break;
        }
    } else {
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
            event.events |= (EPOLLIN | EPOLLPRI | EPOLLRDHUP);
        if(w)
            event.events |= EPOLLOUT;

        assert(r || w);
        assert(sched->num_fds >= fd + 1);
    }
    rc = epoll_ctl(sched->epfd, op, fd, &event);
    if(rc < 0)
        perror("epoll_ctl");
}

void
cpc_prim_io_wait(int fd, int direction, cpc_condvar *cond,
                 cpc_continuation *cont)
{
    if(cont == NULL || cont->sched == NULL)
        return;

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    assert((fd & (7 << 29)) == 0);

    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    if(cont->sched->size_fds <= fd)
        fd_queues_expand(fd + 10, cont->sched);

    cont->state = fd | ((direction & 3) << 29);
    recompute_enqueue(fd, cont);

    return;
}

void
perform_attach(cpc_continuation *cont)
{
    pthread_mutex_lock(&cont->sched->attach_m);
    enqueue(&cont->sched->attaching, cont);
    pthread_mutex_unlock(&cont->sched->attach_m);
}

void
cpc_prim_attach(cpc_scheduler *sched, cpc_continuation *cont)
{
    if(cont == NULL)
        return;

    if(cont->sched == sched) {
        cpc_schedule(cont);
        return;
    }

    if(cont->sched) {
        // detach the continuation if it is already attached,
        // to avoid blocking the whole scheduler.
        cont->sched = sched;
        (void) wp_run_task(pool, (void *) cont,
                (wp_process_cb) perform_attach, NULL);
    } else {
        cont->sched = sched;
        perform_attach(cont);
    }
}

void
cpc_prim_detach(cpc_continuation *cont)
{
    if(cont->sched) {
        cont->sched = NULL;
        (void) wp_run(pool, (void *) cont);
    } else {
        // the continuation is already detached
        cpc_invoke_continuation(cont);
    }
}

static void
requeue_io_ready(int fd, int direction, cpc_scheduler *sched)
{
    cpc_continuation *c;
    c = sched->fd_queues[fd].head;
    while(c) {
        if((c->state >> 29) & direction) {
            dequeue_1(&sched->fd_queues[fd], c); /* XXX */
            c->state = STATE_UNKNOWN;
            if(c->condvar) {
                cond_dequeue_1(&c->condvar->queue, c);
            }
            c->condvar = NULL;
            enqueue(&c->sched->ready, c);
        }
        c = c->next;
    }
    recompute_dequeue(fd, sched);
}

static inline void
exhaust_ready_1(cpc_scheduler *sched)
{
    cpc_continuation *c;

    if(!sched)
        return;

    while(sched->ready_1) {
        c = sched->ready_1;
        sched->ready_1 = NULL;
        cpc_really_invoke_continuation(c);
    }
}

void
cpc_scheduler_loop(cpc_scheduler *sched)
{
    struct cpc_continuation *c;
    struct cpc_continuation_queue q;
    struct timeval when;
    int rc, i, done;

    struct epoll_event events[EPOLL_MAX_EVENTS];

    loop:
    while(sched->ready.head || sched->sleeping.head || sched->attaching.head || sched->num_fds > 0) {
        if(sched->attaching.head) {
            pthread_mutex_lock(&sched->attach_m);
            while(sched->attaching.head)
                enqueue(&sched->ready, dequeue(&sched->attaching));
            pthread_mutex_unlock(&sched->attach_m);
        }
        q = sched->ready;
        sched->ready.head = sched->ready.tail = NULL;
        while(1) {
            c = dequeue(&q);
            if(c == NULL) break;
            assert(c->state == STATE_UNKNOWN && c->condvar == NULL);
            cpc_really_invoke_continuation(c);
            exhaust_ready_1(c->sched);
        }
        if(sched->ready.head && !sched->sleeping.head && sched->num_fds == 0 &&
           !cpc_pessimise_runtime)
            continue;

        gettimeofday(&sched->now, NULL);

        if(sched->sleeping.head) {
            while(1) {
                c = timed_dequeue(&sched->sleeping, &sched->now);
                if(c == NULL) break;
                assert(c->state == STATE_SLEEPING);
                if(c->condvar)
                    cond_dequeue_1(&c->condvar->queue, c);
                c->condvar = NULL;
                c->state = STATE_UNKNOWN;
                cpc_really_invoke_continuation(c);
                exhaust_ready_1(c->sched);
            }
        }

        if(sched->ready.head && !sched->sleeping.head && sched->num_fds == 0 &&
           !cpc_pessimise_runtime)
            continue;

        gettimeofday(&sched->now, NULL);
        if(sched->ready.head || sched->sleeping.head) {
            if(sched->ready.head)
                rc = epoll_wait(sched->epfd, events, EPOLL_MAX_EVENTS, 0);
            else {
                timeval_minus(&when, &sched->sleeping.head->time, &sched->now);
                rc = epoll_wait(sched->epfd, events, EPOLL_MAX_EVENTS,
                                when.tv_sec * 1000 + when.tv_usec / 1000);
            }
        } else {
            if(sched->num_fds == 0)
                break;
            rc = epoll_wait(sched->epfd, events, EPOLL_MAX_EVENTS, -1);
        }
        if(rc == 0 || (rc < 0 && errno == EINTR)) {
            continue;
        } else if(rc < 0) {
            perror("epoll_wait");
            sleep(1);
            continue;
        }


        /* 1 for reads, 2 for writes.  0x100 means allow starvation. */
#ifndef PREFER
#define PREFER 3
#endif

        done = 0;
        for(i = 0; i < rc; i++) {
            int dir = 0;
            if((PREFER & 1) && (events[i].events & (EPOLLIN | EPOLLPRI | EPOLLRDHUP)))
                dir |= CPC_IO_IN;
            if((PREFER & 2) && (events[i].events & EPOLLOUT))
                dir |= CPC_IO_OUT;
            if(events[i].events & (EPOLLERR | EPOLLHUP))
                dir |= (CPC_IO_IN | CPC_IO_OUT);
            if(dir) {
                requeue_io_ready(events[i].data.fd, dir, sched);
                done = 1;
            }
        }

#if (PREFER & 3) != 3
        if((PREFER & 0x100) && done)
            continue;
        for(i = 0; i < rc; i++) {
            int dir = 0;
            if(events[i].events & (EPOLLIN | EPOLLPRI | EPOLLRDHUP))
                dir |= CPC_IO_IN;
            if(events[i].events & EPOLLOUT)
                dir |= CPC_IO_OUT;
            if(dir)
                requeue_io_ready(events[i].data.fd, dir, sched);
        }
#endif
    }
    if(pool) {
        wp_wait(pool);
        if(sched->attaching.head)
            goto loop;
        else
            wp_free(pool, WP_IMMEDIATE);
    }
    cpc_scheduler_free(sched);
}

void
cpc_scheduler_start(cpc_scheduler *sched)
{
    pthread_t thread;
    pthread_attr_t attr;
    int rc;

    pthread_mutex_lock(&pool_m);
    if(!pool) {
        pool = wp_new(0, (wp_process_cb) cpc_really_invoke_continuation, NULL);
        if(!pool) {
            perror("wp_new");
            exit(1);
        }
    }
    pthread_mutex_unlock(&pool_m);

    /* Initialize and set thread detached attribute */
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&attr, 64 * 1024);

    rc = pthread_create(&thread, &attr, (void*)cpc_scheduler_loop, (void *)sched);

    if(rc) {
        perror("pthread_create");
        exit(1);
    }
}

void
cpc_main_loop(void)
{
    gettimeofday(&cpc_main_scheduler.now, NULL);

    cpc_main_scheduler.epfd = epoll_create(10);
    if(cpc_main_scheduler.epfd < 0) {
        perror("epoll_create");
        exit(1);
    }

    pthread_mutex_lock(&pool_m);
    if(!pool) {
        pool = wp_new(0, (wp_process_cb) cpc_really_invoke_continuation, NULL);
        if(!pool) {
            perror("wp_new");
            exit(1);
        }
    }
    pthread_mutex_unlock(&pool_m);

    cpc_scheduler_loop(&cpc_main_scheduler);
}
