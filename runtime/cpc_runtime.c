/*
Copyright (c) 2008, 2009 by Gabriel Kerneis.
Copyright (c) 2004, 2005 by Juliusz Chroboczek.
Experimental; do not redistribute.
*/

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>

#include "cpc_runtime.h"
#include "ev.c"

#include <poll.h> /* cpc_d_io_wait */

#include "nft_pool.h"

static struct ev_loop *loop = NULL;
static ev_idle run;

static pthread_t main_loop_id;

#define IS_DETACHED (loop && !pthread_equal(main_loop_id,pthread_self()))

#define MAX_THREADS 20
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
/* Detached continuations that must be discarded */
#define STATE_GARBAGE -4

static cpc_continuation_queue ready = {NULL, NULL};
static cpc_continuation_queue attach_queue = {NULL, NULL};

static void io_cb (struct ev_loop *, ev_io *, int);
static void timer_cb (struct ev_loop *, ev_timer *, int);
static void idle_cb (struct ev_loop *, ev_idle *, int);
static inline void exhaust_ready(cpc_continuation *);

/*** Basic operations on continuations ***/

void
cpc_print_continuation(struct cpc_continuation *c, char *s)
{
    int i = 0;
    fprintf(stderr,"(%s) At %p %lu:\n%p\t%p\t%p\t%p\t%d\t%u\t%u\n",s,c,
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

void
cpc_continuation_free(struct cpc_continuation *c)
{
    /* Detached continuations are re-attached in a GARBAGE state and
     * freed by attach_cb, so that the event loop is aware they have
     * been discarded. */
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
    /* The ev_watcher struct is stored directly in the continuation. But
     * it must not be copied since libev uses pointers to this struct.
     * Rather than using malloc and a pointer to the structure, we claim
     * that cpc_continuation_expand and cpc_continuation_copy cannot be
     * called when the continuation is sleeping or io_waiting. Checking
     * the claim, just in case we missed something:
     */
    assert(("This is a bug, please report it.",
            c->state == STATE_UNKNOWN || c->state == STATE_DETACHED));
    /* And do not copy c->watcher since it's meaningless garbage. */

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
    assert(("This is a bug, please report it.", /* See above for more details. */
            c->state == STATE_UNKNOWN || c->state == STATE_DETACHED));

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

void
cpc_prim_wait(cpc_condvar *cond, cpc_continuation *cont)
{
    assert(!IS_DETACHED && cont->condvar == NULL && cont->state == STATE_UNKNOWN);
    cont->condvar = cond;
    cond_enqueue(&cond->queue, cont);
}

void
cpc_signal(cpc_condvar *cond)
{
    assert(!IS_DETACHED);
    cpc_continuation *cont;
    cont = cond_dequeue(&cond->queue);
    if(cont == NULL)
        return;
    assert(cont->condvar == cond);
    cont->condvar = NULL;
    dequeue_other(cont);
    enqueue(&ready, cont);
    if(loop)
        ev_idle_start(loop, &run);
}

void
cpc_signal_all(cpc_condvar *cond)
{
    cpc_continuation *cont;

    assert(!IS_DETACHED);
    while(1) {
        cont = cond_dequeue(&cond->queue);
        if(cont == NULL)
            break;
        assert(cont->condvar == cond);
        cont->condvar = NULL;
        dequeue_other(cont);
        enqueue(&ready, cont);
        if(loop)
            ev_idle_start(loop, &run);
    }
}

/*** cpc_sleep ***/

void
cpc_prim_sleep(int sec, int usec, cpc_condvar *cond, cpc_continuation *cont)
{
    ev_tstamp timeout = sec + ((ev_tstamp) usec) / 1e6;

    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        ev_sleep(timeout); /* Uses nanosleep or select */
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
    ev_timer_init(&cont->watcher.timer, timer_cb, timeout, 0.);
    ev_timer_start(loop, &cont->watcher.timer);
    return;
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
    if(loop)
        ev_idle_start(loop, &run);
}

/*** cpc_io_wait ***/

void
cpc_signal_fd(int fd, int direction)
{
    ev_feed_fd_event (loop, fd, direction);
}

static inline int
cpc_d_io_wait(int fd, int direction)
{
    struct pollfd pfd[1];
    int pollevent = direction == CPC_IO_OUT ? POLLOUT : POLLIN;
    int rc;

    pfd[0].fd = fd;
    pfd[0].events = pollevent;
    pfd[0].revents = 0;

    rc = poll(pfd, 1, -1);
    if(rc < 0)
        return -1;

    if(pfd[0].revents & pollevent)
        return 1;

    return 0;
}

void
cpc_prim_io_wait(int fd, int direction, cpc_condvar *cond,
                 cpc_continuation *cont)
{
    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        cpc_d_io_wait(fd, direction); // XXX No error handling
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
    if(loop)
        ev_idle_start(loop, &run);
}

/*** cpc_attach ****/

void
cpc_prim_attach(cpc_continuation *cont)
{
    if(!IS_DETACHED) {
        assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
        cpc_invoke_continuation(cont);
        return;
    }
    if(cont->state == STATE_DETACHED)
        cont->state = STATE_UNKNOWN;
    else
        /* cont must be freed, but we have to attach it first, so that
         * the event loop doesn't wait for it forever. */
        assert(cont->state == STATE_GARBAGE);
    pthread_mutex_lock (&attach_mutex);
    enqueue(&attach_queue, cont);
    pthread_mutex_unlock (&attach_mutex);
    ev_async_send(loop, &attach_sig);
    return;
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

/*** cpc_detach ***/

static void *
perform_detach(void *cont)
{
    struct cpc_continuation *c = (struct cpc_continuation *)cont;
    c->state = STATE_DETACHED;
    exhaust_ready(c);
    return NULL;
}

void
cpc_prim_detach(struct nft_pool *pool, cpc_continuation *cont)
{
    if(cont->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        cpc_invoke_continuation(cont);
        return;
    }
    assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
    ev_ref(loop);
    if(pool == NULL)
        pool = thread_pool;
    nft_pool_add(pool, (void(*)(void*)) perform_detach, cont);
    return;
}

struct nft_pool *
cpc_threadpool_get(int max_threads)
{
    if(max_threads <= 0 || max_threads > MAX_THREADS)
        max_threads = MAX_THREADS;
    return nft_pool_create(max_threads, 0);
}

void
cpc_threadpool_release(struct nft_pool *pool)
{
    /* ntf_pool_destroy is blocking until every thread in the pool is
     * done, so we destroy it in a separate thread. */
     pthread_t t;
     pthread_attr_t attr;
     pthread_attr_init(&attr);
     pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
     pthread_create(&t, &attr, (void(*)(void*)) nft_pool_destroy, (void *)pool);
     pthread_attr_destroy(&attr);
}

/*** cpc_yield and cpc_spawn ***/

void
cpc_schedule(struct cpc_continuation *cont)
{
    assert(cont);
    if(IS_DETACHED)
        if(cont->state == STATE_UNKNOWN)
            /* spawn in detached mode */
            cpc_prim_attach(cont);
        else {
            /* yield in detached mode */
            assert(cont->state == STATE_DETACHED);
            cpc_invoke_continuation(cont);
        }
    else {
        assert(cont->state == STATE_UNKNOWN && cont->condvar == NULL);
        enqueue(&ready, cont);
        if(loop)
            ev_idle_start(loop, &run);
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

/*** The default callback, which runs continuations ***/

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

void
cpc_main_loop(void)
{
    loop = ev_default_loop(0);

    main_loop_id = pthread_self();

    ev_async_init(&attach_sig, attach_cb);
    ev_async_start(loop, &attach_sig);
    ev_unref(loop);

    ev_idle_init(&run, idle_cb);
    /* Executing continuations must be the task of highest priority. */
    ev_set_priority(&run, EV_MAXPRI);
    ev_idle_start(loop, &run);

    thread_pool = nft_pool_create(MAX_THREADS, 0);

    ev_loop(loop, 0);

    nft_pool_destroy(thread_pool);
}
