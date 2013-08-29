/*
Copyright (c) 2008-2011,
  Gabriel Kerneis     <kerneis@pps.univ-paris-diderot.fr>
Copyright (c) 2004-2005,
  Juliusz Chroboczek  <jch@pps.univ-paris-diderot.fr>

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

#include "compatibility.h"

#include "threadpool/threadpool.h"

typedef struct cpc_thread {
    struct cpc_thread *next;
    struct cpc_condvar *condvar;
    struct cpc_thread *cond_next;
    cpc_sched *sched;
    int state;
    HANDLE performed_handle; /* Ces deux champs ne devraient pas être là : */
    OVERLAPPED overlapped;   /* tous les threads ne font pas d'E/S. (?) */
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

static inline OVERLAPPED *
get_overlapped(cpc_thread *t)
{
    return (OVERLAPPED *)(((char *)t) + offsetof(struct cpc_thread, overlapped));
}

static inline cpc_thread *
get_thread_from_overlapped(OVERLAPPED *o)
{
    return (cpc_thread *)(((char *)o) - offsetof(struct cpc_thread, overlapped));
}

#ifdef CPC_INDIRECT_PATCH
#define RETVAL_FIELD(type) type* cpc_retval;
#define RETVAL_SET(cont,args) do{(cont)->cpc_retval = (args)->cpc_retval;}while(0)
#else
#define RETVAL_FIELD(type)
#define RETVAL_SET(cont,args) do{}while(0)
#endif

struct cpc_sched {
    threadpool_t *pool;
    struct cpc_sched *next;
};
static DWORD main_loop_id;
#define IS_DETACHED (cpc_default_threadpool &&                  \
                     (main_loop_id != GetCurrentThreadId()))
#define MAX_THREADS 20
cpc_sched *cpc_default_threadpool = NULL;
#define IOCPKEY_IO                   0
#define IOCPKEY_ATTACH               1
#define IOCPKEY_SPAWN                2
#define IOCPKEY_DETACHED_DEATH       3
#define IOCPKEY_DETACHED_AIOWAIT     4

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

struct cpc_overlapped {
    OVERLAPPED overlapped;
    cpc_thread *thread;
    HANDLE handle;
    int status;
#define AIO_STATUS_NONE     0
#define AIO_STATUS_ON_HOLD  1
#define AIO_STATUS_DONE     2
    int64_t result;
};

#define STATE_UNKNOWN    -1
#define STATE_SLEEPING   -2
#define STATE_DETACHED   -3
#define STATE_IO_PENDING -4

static HANDLE completionPort = INVALID_HANDLE_VALUE;
static cpc_thread_queue ready = {NULL, NULL};
static cpc_timed_thread_queue sleeping = {NULL, 0, 0};
static cpc_sched_queue schedulers = {NULL, NULL};
static int num_fds = 0;

static struct timeval cpc_now;

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

void
cpc_continuation_free(struct cpc_continuation *c)
{
    cpc_thread *t = get_thread(c);
    assert((t->state == STATE_UNKNOWN || t->state == STATE_DETACHED)
           && t->condvar == NULL);
    if(t->state == STATE_DETACHED)
        PostQueuedCompletionStatus(completionPort, 0,
                                   IOCPKEY_DETACHED_DEATH, NULL);
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
    else
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

#define cps_expand1(ret_type,name,type,arg)\
    struct name##_arglist {\
        RETVAL_FIELD(ret_type);\
        LAST_ARG(type,arg);\
    cpc_continuation *\
    name(struct cpc_continuation *cont)\
    {\
        struct name##_arglist *cpc_arguments ;\
        cpc_arguments = (struct name##_arglist *) cpc_dealloc(cont,\
                        (int )sizeof(struct name##_arglist ));\
        type arg = cpc_arguments -> arg;\
        RETVAL_SET(cont, cpc_arguments);\
        cpc_thread *thread = get_thread(cont);

#define cps_expand3(ret_type,name,type1,arg1,type2,arg2,type3,arg3) \
    struct name##_arglist {\
        RETVAL_FIELD(ret_type);\
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
        RETVAL_SET(cont, cpc_arguments);\
        cpc_thread *thread = get_thread(cont);

#define cps_expand4(name,type1,arg1,type2,arg2,type3,arg3,type4,arg4)\
    struct name##_arglist {\
        type1 arg1;\
        type2 arg2;\
        type3 arg3;\
        LAST_ARG(type4,arg4);\
    cpc_continuation *\
    name(struct cpc_continuation *cont)\
    {\
        struct name##_arglist *cpc_arguments ;\
        cpc_arguments = (struct name##_arglist *) cpc_dealloc(cont,\
                        (int )sizeof(struct name##_arglist ));\
        type1 arg1 = cpc_arguments -> arg1;\
        type2 arg2 = cpc_arguments -> arg2;\
        type3 arg3 = cpc_arguments -> arg3;\
        type4 arg4 = cpc_arguments -> arg4;\
        cpc_thread *thread = get_thread(cont);


/* cps int cpc_wait(cpc_condvar *cond) */
cps_expand1(int,cpc_wait, cpc_condvar *, cond)
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
    if (thread->state == STATE_IO_PENDING) {
        /* The cancelled IO will produce a completion packet. */
        rc = CancelIoEx(thread->performed_handle, &thread->overlapped);
        assert(rc);
        return;
    }
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
        if (thread->state == STATE_IO_PENDING) {
            /* The cancelled IO will produce a completion packet. */
            rc = CancelIoEx(thread->performed_handle, &thread->overlapped);
            assert(rc);
            continue;
        }
        cpc_continuation_patch(get_cont(thread), sizeof(int), &rc);
        dequeue_other(thread);
        enqueue(&ready, thread);
    }
}

/*** cpc_sleep ***/

/* cps int cpc_sleep(int sec, int usec, cpc_condvar *cond) */
cps_expand3(int,cpc_sleep, int, sec, int, usec, cpc_condvar *, cond)
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

/*** IO ***/

cpc_overlapped *
cpc_get_overlapped(DWORD offset, DWORD offsetHigh)
{
    cpc_overlapped *ovl = malloc(sizeof(cpc_overlapped));
    ovl->overlapped.Internal = 0;
    ovl->overlapped.InternalHigh = 0;
    ovl->overlapped.Offset = offset;
    ovl->overlapped.OffsetHigh = offsetHigh;
    ovl->overlapped.hEvent = 0;
    ovl->status = AIO_STATUS_NONE;
    ovl->result = 0;
    return ovl;
}

void
cpc_set_overlapped(cpc_overlapped *ovl, DWORD offset, DWORD offsetHigh)
{
    ovl->overlapped.Internal = 0;
    ovl->overlapped.InternalHigh = 0;
    ovl->overlapped.Offset = offset;
    ovl->overlapped.OffsetHigh = offsetHigh;
    ovl->overlapped.hEvent = 0;
    ovl->status = AIO_STATUS_NONE;
    ovl->result = 0;
}

void
cpc_free_overlapped(cpc_overlapped *ovl)
{
    free(ovl);
}

/* cpc_aio_wait breaks the invariant consisting to execute the thread at the
   next loop iteration when aborded. However, this is probably the case. */
cps_expand3(int64_t,
            cpc_aio_wait,
            HANDLE, handle,
            cpc_overlapped *, ovl,
            cpc_condvar*, cond)
    ovl->thread = thread;
    if(thread->state == STATE_DETACHED) {
        assert(IS_DETACHED && cond == NULL);
        /* Warning to race conditions. Don't check nor modify ovl->status. */
        if(ovl->result != 0) {
            cpc_continuation_patch(cont, sizeof(int64_t), &ovl->result);
            return cont;
        }
        PostQueuedCompletionStatus(completionPort, 0,
                                   IOCPKEY_DETACHED_AIOWAIT, (void*)ovl);
        return NULL;
    }
    if(ovl->status == AIO_STATUS_DONE) {
        cpc_continuation_patch(cont, sizeof(int64_t), &ovl->result);
        return cont;
    }
    if(cond) {
        ovl->handle = handle;
        thread->condvar = cond;
        cond_enqueue(&cond->queue, thread);
    }
    num_fds++;
    return NULL;
}

void
cpc_signal_fd(HANDLE handle, int direction)
{
    CancelIoEx(handle, NULL);
}

/*** cpc_link ****/

cpc_continuation *cpc_prim_attach(cpc_thread*);
cpc_continuation *cpc_prim_detach(cpc_sched*, cpc_thread*);

/* cps cpc_sched *cpc_link(cpc_sched *sched) */
cps_expand1(cpc_sched *,cpc_link, cpc_sched *, sched)
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

cpc_continuation *
cpc_prim_attach(cpc_thread *thread)
{
    if(thread->state == STATE_UNKNOWN) {
        assert(!IS_DETACHED && thread->condvar == NULL);
        assert(thread->sched == cpc_default_sched);
        return get_cont(thread);
    }

    assert(thread->state == STATE_DETACHED);
    PostQueuedCompletionStatus(completionPort, 0,
                               IOCPKEY_ATTACH, (void*)thread);
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

cpc_sched *
cpc_threadpool_get(int max_threads)
{
    cpc_sched_queue *q = &schedulers;

    assert(!IS_DETACHED);

    if(max_threads <= 0 || max_threads > MAX_THREADS)
        max_threads = MAX_THREADS;

    cpc_sched *sched = malloc(sizeof(cpc_sched));

    sched->pool = threadpool_create(max_threads, NULL, NULL);
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
        PostQueuedCompletionStatus(completionPort, 0,
                                   IOCPKEY_SPAWN, (void*)thread);
    }
    /* Otherwise, trust the context */
    else if (context && get_thread(context)->state == STATE_DETACHED) {
        assert(IS_DETACHED);
        PostQueuedCompletionStatus(completionPort, 0,
                                   IOCPKEY_SPAWN, (void*)thread);
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

void
cpc_main_loop(void)
{
    struct cpc_thread *thread;
    cpc_continuation *cont;
    struct cpc_thread_queue q;
    struct timeval when;
    int rc;
    OVERLAPPED *poverlapped;
    DWORD milliseconds;
    DWORD nbytes;
    int64_t io_rc;
    DWORD completionKey;

    main_loop_id = GetCurrentThreadId();

    cpc_default_threadpool = cpc_threadpool_get(MAX_THREADS);

    if(completionPort == INVALID_HANDLE_VALUE) {
        completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL,0,0);
        if(completionPort == NULL) {
            print_error("CreateIoCompletionPort");
            exit(1);
        }
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
                cont = get_cont(thread);
                cpc_continuation_patch(cont, sizeof(int), &rc);
                cpc_invoke_continuation(cont);
            }
        }

        if(ready.head && sleeping.size == 0 && num_fds == 0 &&
           detached_count == 0 && !cpc_pessimise_runtime)
            continue;

        gettimeofday(&cpc_now, NULL);

        if(ready.head || sleeping.size > 0) {
            if(ready.head) {
                milliseconds = 0;
            } else {
                timeval_minus(&when, &sleeping.heap[0]->time, &cpc_now);
                milliseconds = 1000 * when.tv_sec + when.tv_usec / 1000;
            }
        } else {
            if(num_fds == 0 && detached_count == 0)
                continue;
            milliseconds = INFINITE;
        }

        while(1) {
            struct cpc_overlapped *ovl;
            rc = GetQueuedCompletionStatus(completionPort,
                                           &nbytes,
                                           &completionKey,
                                           &poverlapped,
                                           milliseconds);
            if(!rc) {
                if (poverlapped == NULL)
                    break;
                assert(completionKey == IOCPKEY_IO);
                if (GetLastError() == ERROR_HANDLE_EOF) {
                    io_rc = 0;
                } else {
                    io_rc = -(int64_t)GetLastError();
                }
            } else {
                io_rc = nbytes;
            }

            milliseconds = 0;

            switch(completionKey) {
            case IOCPKEY_ATTACH:
                detached_count --; /* don't break */
            case IOCPKEY_SPAWN:
                spawn_cb((cpc_thread *)poverlapped);
                break;
            case IOCPKEY_DETACHED_DEATH:
                detached_count --;
                break;
            case IOCPKEY_DETACHED_AIOWAIT:
                ovl = (void*)poverlapped;
                if(ovl->status == AIO_STATUS_DONE) {
                    thread = ovl->thread;
                    cont = get_cont(thread);
                    cpc_continuation_patch(cont, sizeof(int64_t), &ovl->result);
                    rc = threadpool_schedule(thread->sched->pool,
                                             &perform_detach, (void*)thread);
                    if (rc < 0) {
                        perror("threadpool_schedule");
                        exit(1);
                    }
                    break;
                }
                ovl->status = AIO_STATUS_ON_HOLD;
                break;
            default:
                assert(completionKey == IOCPKEY_IO && num_fds > 0);
                cpc_overlapped *ovl = (cpc_overlapped*) poverlapped;
                thread = ovl->thread;
                cont = get_cont(thread);
                if(thread->state == STATE_DETACHED) {
                    if(ovl->status == AIO_STATUS_ON_HOLD) {
                        cpc_continuation_patch(cont, sizeof(int64_t), &io_rc);
                        rc = threadpool_schedule(thread->sched->pool,
                                                 &perform_detach,(void*)thread);
                        if (rc < 0) {
                            perror("threadpool_schedule");
                            exit(1);
                        }
                        break;
                    }
                    ovl->result = io_rc;
                    ovl->status = AIO_STATUS_DONE;
                    break;
                }
                num_fds--;
                if(thread->condvar) {
                    cond_dequeue_1(&thread->condvar->queue, thread);
                    thread->condvar = NULL;
                }
                thread->state = STATE_UNKNOWN;
                cpc_continuation_patch(cont, sizeof(int64_t), &io_rc);
                enqueue(&ready, thread);
                break;
            }
        }
    }
    while(cpc_threadpool_release(cpc_default_threadpool) < 0)
        Sleep(1);

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

int
cpc_io_associate_with_completion_port(HANDLE handle)
{
    assert(completionPort != NULL);
    HANDLE cp;
    if(completionPort == INVALID_HANDLE_VALUE) {
        completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL,0,0);
        if(completionPort == NULL) {
            print_error("CreateIoCompletionPort");
            exit(1);
        }
    }
    cp = CreateIoCompletionPort(handle, completionPort, IOCPKEY_IO, 0);
    if(cp == NULL)
        return -1;
    return 0;
}
