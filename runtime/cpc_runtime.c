/* 
Copyright (c) 2008, 2009 by Gabriel Kerneis.
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
#include <pthread.h>


#include "cpc_runtime.h"
#include "ev.c"

#include "nft_pool.h"

static struct ev_loop *loop = NULL;
static ev_idle run;

static pthread_t main_loop_id;

#define IS_DETACHED (loop && !pthread_equal(main_loop_id,pthread_self()))

#define MAX_THREADS 50
static nft_pool_t *thread_pool;

static ev_async attach_sig;
static pthread_mutex_t attach_mutex = PTHREAD_MUTEX_INITIALIZER;

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
#define STATE_DETACHED -3
#define STATE_GARBAGE -4

static cpc_continuation_queue ready = {NULL, NULL};
static cpc_continuation_queue attach_queue = {NULL, NULL};

static void io_cb (struct ev_loop *, ev_io *, int);
static void timer_cb (struct ev_loop *, ev_timer *, int);
static void idle_cb (struct ev_loop *, ev_idle *, int);
static inline void exhaust_ready(cpc_continuation *);

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
    /* Detached continuation are re-attached in a GARBAGE state and
     * freed by attach_cb */
    if(c->state == STATE_DETACHED) {
        c->state = STATE_GARBAGE;
        cpc_prim_attach(c);
        return;
    }
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
        d->ready = NULL;
        return d;
    }

    memcpy(d->c, c->c, c->length);

    d->length = c->length;
    d->condvar = c->condvar;
    d->cond_next = c->cond_next;
    d->state = c->state;
    d->ready = c->ready;
    /* Otherwise, you have to use a pointer to a watcher, and use malloc. */
    assert(c->state == STATE_UNKNOWN || c->state == STATE_DETACHED);
    //d->watcher = c->watcher;
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
    d->ready = c->ready;
    assert(c->state == STATE_UNKNOWN || c->state == STATE_DETACHED); // See above.

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
    if(loop && queue == &ready)
        ev_idle_start(loop, &run);
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
        ev_timer_stop(loop, &cont->watcher.timer);
    else if(cont->state >= 0) {
        ev_io_stop(loop, &cont->watcher.io);
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
    assert(cont);
    if(IS_DETACHED)
        if(cont->state == STATE_UNKNOWN)
            cpc_prim_attach(cont); /* spawn in detached mode */
        else { /* yield in detached mode */
            assert(cont->state == STATE_DETACHED);
            cpc_invoke_continuation(cont);
        }
    else {
        assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
        enqueue(&ready, cont);
     }
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

int
cpc_condvar_count(cpc_condvar *cond)
{
    cpc_continuation *cont = cond->queue.head;
    int i = 0;

    cont = cond->queue.head;
    while(cont) {
        i++;
        cont = cont->next;
    }
    return i;
}

void
cpc_prim_sleep(int sec, int usec, cpc_condvar *cond, cpc_continuation *cont)
{
    ev_tstamp timeout;
    
    if(IS_DETACHED) {
        assert(cond == NULL);
        sleep(sec);
        usleep(usec);
        cpc_invoke_continuation(cont);
        return;
    }

    if(cont == NULL)
        cont = cpc_continuation_expand(NULL, 0);

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }
    cont->state = STATE_SLEEPING;
    timeout = sec + ((ev_tstamp) usec) / 1000000.;
    ev_timer_init(&cont->watcher.timer, timer_cb, timeout, 0.);
    ev_timer_start(loop, &cont->watcher.timer);
    return;
}

void
cpc_prim_io_wait(int fd, int direction, cpc_condvar *cond,
                 cpc_continuation *cont)
{
    if(IS_DETACHED) {
        assert(cond == NULL);
        cpc_invoke_continuation(cont);
        return;
    }

    if(cont == NULL)
        return;

    assert(cont->condvar == NULL && cont->state == STATE_UNKNOWN);

    if(cond) {
        cont->condvar = cond;
        cond_enqueue(&cond->queue, cont);
    }

    cont->state = fd /*| ((direction & 3) << 29)*/;
    ev_io_init(&cont->watcher.io, io_cb, fd, direction);
    ev_io_start(loop, &cont->watcher.io);
    return;
}

void
cpc_prim_attach(cpc_continuation *cont)
{
    if(cont->state == STATE_DETACHED)
        cont->state = STATE_UNKNOWN;
    else if(cont->state == STATE_UNKNOWN)
        assert(IS_DETACHED); /* spawn in detached mode */
    else
        assert(cont->state == STATE_GARBAGE);
    pthread_mutex_lock (&attach_mutex);
    enqueue(&attach_queue, cont);
    pthread_mutex_unlock (&attach_mutex);
    ev_async_send(loop, &attach_sig);
    return;
}

void *
perform_detach(void *cont)
{
    struct cpc_continuation *c = (struct cpc_continuation *)cont;
    c->state = STATE_DETACHED;
    exhaust_ready(c);
    return NULL;
}

void
cpc_prim_detach(cpc_continuation *cont)
{
    assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
    ev_ref(loop);
    nft_pool_add(thread_pool, (void(*)(void*)) perform_detach, cont);
    return;
}

static void
io_cb(struct ev_loop *loop, ev_io *w, int revents)
{
    /* Ugly pointer arithmetic to get the continuation including the
     * watcher w */
    struct cpc_continuation *c = (struct cpc_continuation *)
    (((char *) w ) - offsetof(struct cpc_continuation, watcher));

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
timer_cb(struct ev_loop *loop, ev_timer *w, int revents)
{
    /* Ugly pointer arithmetic to get the continuation including the
     * watcher w */
    struct cpc_continuation *c = (struct cpc_continuation *)
    (((char *)w) - offsetof(struct cpc_continuation, watcher));

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

static void
idle_cb(struct ev_loop *loop, ev_idle *w, int revents)
{
    struct cpc_continuation *c;
    struct cpc_continuation_queue q;

    ev_idle_stop(loop, w);
    q = ready;
    ready.head = ready.tail = NULL;
    while((c = dequeue(&q))) {
        assert(c->state == STATE_UNKNOWN && c->condvar == NULL);
        exhaust_ready(c);
    }
}

static void
attach_cb(struct ev_loop *loop, ev_async *w, int revents)
{
    struct cpc_continuation *c;

    pthread_mutex_lock (&attach_mutex);
    while((c = dequeue(&attach_queue))) {
        if(c->state == STATE_GARBAGE) {
            free(c);
            ev_unref(loop);
            continue;
        }
        assert(c->state == STATE_UNKNOWN && c->condvar == NULL);
        ev_unref(loop);
        exhaust_ready(c);
    }
    pthread_mutex_unlock (&attach_mutex);
}

void 
cpc_main_loop(void)
{
    loop = ev_default_loop(0);

    main_loop_id = pthread_self();

    ev_async_init(&attach_sig, attach_cb);
    ev_async_start(loop, &attach_sig);
    ev_unref(loop);

    ev_idle_init(&run, idle_cb);
    ev_set_priority(&run, EV_MAXPRI);
    ev_idle_start(loop, &run);

    thread_pool = nft_pool_create(MAX_THREADS, 0);

    ev_loop(loop, 0);

    nft_pool_destroy(thread_pool);
}
