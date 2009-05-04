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
#include "ev.c"

struct ev_loop *loop = NULL;
ev_idle run;

typedef struct cpc_continuation_queue {
    struct cpc_continuation *head;
    struct cpc_continuation *tail;
} cpc_continuation_queue;

struct cpc_condvar {
    int refcount;
    cpc_continuation_queue queue;
};

#define STATE_UNKNOWN -1
#define STATE_SLEEPING -2

cpc_continuation *cpc_ready_1;

static cpc_continuation_queue ready = {NULL, NULL};

static void io_cb (struct ev_loop *, ev_io *, int);
static void timer_cb (struct ev_loop *, ev_io *, int);

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
    assert(c->state == STATE_UNKNOWN && c->condvar == NULL &&
    !ev_is_active(&c->io) && !ev_is_active(&c->timer));
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
        ev_init(&d->timer, timer_cb);
        ev_init(&d->io, io_cb);
        return d;
    }

    memcpy(d->c, c->c, c->length);

    d->length = c->length;
    d->condvar = c->condvar;
    d->cond_next = c->cond_next;
    d->state = c->state;
    d->io = c->io;
    d->timer = c->timer;
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
    d->io = c->io;
    d->timer = c->timer;

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
    // XXX
    if(loop)
        ev_idle_start(loop, &run);
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

static void
dequeue_other(cpc_continuation *cont)
{
    if(cont->state == STATE_SLEEPING)
        ev_timer_stop(loop, &cont->timer);
    else if(cont->state >= 0) {
        ev_io_stop(loop, &cont->io);
    } else
        assert(cont->state == STATE_UNKNOWN);
    cont->state = STATE_UNKNOWN;
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
    ev_tstamp timeout;
    
    if(cont == NULL)
        cont = cpc_continuation_expand(NULL, 0);

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN &&
    !ev_is_active(&cont->timer));
    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    cont->state = STATE_SLEEPING;
    timeout = sec + ((ev_tstamp) usec) / 1000000.;
    ev_timer_set(&cont->timer, timeout, 0.);
    ev_timer_start(loop, &cont->timer);
    return;
}

void
cpc_prim_io_wait(int fd, int direction, cpc_condvar *cond,
                 cpc_continuation *cont)
{
    if(cont == NULL)
        return;

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN &&
    !ev_is_active(&cont->io));

    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }

    cont->state = fd /*| ((direction & 3) << 29)*/;
    ev_io_set(&cont->io, fd, direction);
    ev_io_start(loop, &cont->io);
    return;
}

void
cpc_prim_attach(cpc_continuation *cont)
{
    cpc_really_invoke_continuation(cont);
    return;
}

void
cpc_prim_detach(cpc_continuation *cont)
{
    cpc_really_invoke_continuation(cont);
    return;
}

static void
io_cb(struct ev_loop *loop, ev_io *w, int revents)
{
    /* Ugly pointer arithmetic to get the continuation including the
     * watcher w */
    struct cpc_continuation *c = (struct cpc_continuation *)
    (((char *) w ) - offsetof(struct cpc_continuation, io));
    
    assert(c->state == w->fd);

    ev_io_stop(loop, w);
    c->state = STATE_UNKNOWN;
    if(c->condvar) {
        cond_dequeue_1(&c->condvar->queue, c);
    }
    c->condvar = NULL;
    enqueue(&ready, c);
}

static void
timer_cb(struct ev_loop *loop, ev_io *w, int revents)
{
    /* Ugly pointer arithmetic to get the continuation including the
     * watcher w */
    struct cpc_continuation *c = (struct cpc_continuation *)
    (((char *)w) - offsetof(struct cpc_continuation, timer));
    
    assert(c->state == STATE_SLEEPING);

    ev_timer_stop(loop, w);
    c->state = STATE_UNKNOWN;
    if(c->condvar) {
        cond_dequeue_1(&c->condvar->queue, c);
    }
    c->condvar = NULL;
    enqueue(&ready, c);
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

static void
idle_cb(struct ev_loop *loop, ev_idle *w, int revents)
{
    struct cpc_continuation *c;
    struct cpc_continuation_queue q;

    ev_idle_stop(loop, w);
    q = ready;
    ready.head = ready.tail = NULL;
    while((c = dequeue(&q))) {
        assert(c->state == STATE_UNKNOWN && c->condvar == NULL &&
                !ev_is_active(&c->timer) && !ev_is_active(&c->io));
        cpc_really_invoke_continuation(c);
        exhaust_ready_1();
    }
}

void 
cpc_main_loop(void)
{
    ev_prepare prepare;

    cpc_ready_1 = NULL;
    loop = ev_default_loop(0);
    ev_idle_init(&run, idle_cb);
    ev_idle_start(loop, &run);

    ev_loop(loop, 0);
}
