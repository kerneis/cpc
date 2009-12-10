/* 
Copyright (c) 2008, 2009 by Gabriel Kerneis.
Copyright (c) 2004, 2005 by Juliusz Chroboczek.
Experimental; do not redistribute.
*/
#include <stddef.h>
#define EV_STANDALONE 1
#define EV_STAT_ENABLE 0
#define EV_PERIODIC_ENABLE 0
#define EV_EMBED_ENABLE 0
/* XXX This is not portable -- int should be sig_atomic_t, but this
 * would require signal.h. */
#define EV_ATOMIC_T int volatile
#include "ev.h"

typedef struct nft_pool cpc_sched;
extern cpc_sched *cpc_default_pool;
#define cpc_default_sched NULL

extern void *memcpy (void * __restrict dest, const void * __restrict src, size_t n);

typedef struct cpc_continuation {
    struct cpc_continuation *next;
    struct cpc_condvar *condvar;
    struct cpc_continuation *cond_next;
    struct cpc_continuation **ready;
    cpc_sched *sched;
    union ev_any_watcher watcher;
    int state;
    unsigned short length;
    unsigned short size;
    char c[1];
} cpc_continuation;

typedef void cpc_function(void*);
typedef struct cpc_condvar cpc_condvar;

void cpc_continuation_free(struct cpc_continuation *c);
struct cpc_continuation *cpc_continuation_expand(struct cpc_continuation *c,
                                                 int n);
struct cpc_continuation *cpc_continuation_copy(struct cpc_continuation *c);


static inline void* 
cpc_alloc(struct cpc_continuation **cp, int s)
{
    struct cpc_continuation *c;
    void *p;

    c = *cp;
    if(c == (void*)0 || s > c->size - c->length)
        c = cpc_continuation_expand(c, s);
    p = c->c + c->length;
    c->length += s;
    *cp = c;
    return p;
}

static inline void*
cpc_dealloc(struct cpc_continuation *c, int s)
{
    c->length -= s;
    return c->c + c->length;
}

static void
cpc_really_invoke_continuation(struct cpc_continuation *c)
{
    cpc_function *f;

    if(c == (void*)0)
        return;

    if(c->length == 0) {
        cpc_continuation_free(c);
        return;
    }

    c->length -= sizeof(cpc_function*);
    f = *(cpc_function**)(c->c + c->length);
    (*f)(c);
}

#ifdef CPC_TAIL_RECURSIVE_COMPILER
#define cpc_invoke_continuation cpc_really_invoke_continuation
#else
static inline void
cpc_invoke_continuation(struct cpc_continuation *c)
{
    *(c->ready) = c;
    return;
}
#endif

#define CPC_IO_IN EV_READ
#define CPC_IO_OUT EV_WRITE
#define CPC_TIMEOUT EV_TIMEOUT
#define CPC_CONDVAR EV_CUSTOM

#define CPC_NORMAL 0
#define CPC_BACKGROUND 1
#define CPC_OPPORTUNISTIC 2

static inline struct cpc_continuation *
cpc_continuation_push(cpc_continuation *c, cpc_function *f)
{
    if(c == (void*)0 || sizeof(cpc_function*) > c->size - c->length)
        c = cpc_continuation_expand(c, sizeof(cpc_function*));

    *(cpc_function**)(c->c + c->length) = f;
    c->length += sizeof(cpc_function*);
    return c;
}

static inline void
cpc_continuation_patch(cpc_continuation *cont, size_t size, void *value)
{
  void *cpc_arg;
  cpc_arg =
    ((cont)->c + (cont)->length - sizeof(cpc_function*) - size);
  memcpy(cpc_arg, value, size);
  return;
}

extern void cpc_main_loop(void);
extern void print_continuation(struct cpc_continuation *c, char *s);

extern cpc_condvar *cpc_condvar_get(void);
extern cpc_condvar *cpc_condvar_retain(cpc_condvar*);
extern void cpc_condvar_release(cpc_condvar*);
extern void cpc_signal_fd(int, int);
extern void cpc_signal(cpc_condvar*);
extern void cpc_signal_all(cpc_condvar*);
extern int cpc_condvar_count(cpc_condvar*);

extern void cpc_prim_spawn(struct cpc_continuation*, struct cpc_continuation*);
/* extern void cpc_prim_yield(struct cpc_continuation *cont); */

/* extern void cpc_prim_sleep(int, int, cpc_condvar*, cpc_continuation*);
extern void cpc_prim_wait(cpc_condvar*, cpc_continuation*);
extern void cpc_prim_io_wait(int, int, cpc_condvar*, cpc_continuation*); */

extern void cpc_prim_attach(cpc_continuation*);
extern void cpc_prim_detach(cpc_sched*, cpc_continuation*);
extern cpc_sched *cpc_threadpool_get(int);
extern void cpc_threadpool_release(cpc_sched *);

extern cpc_sched *cpc_get_sched();

extern double cpc_now(void);

#ifndef NO_CPS_PROTO
extern cps int cpc_io_wait(int fd, int direction, cpc_condvar *cond);
extern cps int cpc_sleep(int sec, int usec, cpc_condvar *cond);
extern cps int cpc_wait(cpc_condvar *cond);
extern cps void cpc_yield(int mode);
extern cps void cpc_done(void);
extern cps cpc_sched *cpc_attach(cpc_sched *pool);

#define cpc_detach() cpc_attach(cpc_get_sched() == cpc_default_sched ? cpc_default_pool : cpc_get_sched())
#define cpc_detached cpc_attached(cpc_get_sched() == cpc_default_sched ? cpc_default_pool : cpc_get_sched())

#endif


