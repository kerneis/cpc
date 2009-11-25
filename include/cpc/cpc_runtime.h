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

extern void *memcpy (void *dest, const void *src, size_t n);

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

extern double cpc_now(void);


/* Macro hacks to handle variadic cps functions */

/* The PP_NARG macro returns the number of arguments that have been
  * passed to it.
  */

#define PP_NARG(...) \
         PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...) \
         PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N( \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N
#define PP_RSEQ_N() \
         63,62,61,60,                   \
         59,58,57,56,55,54,53,52,51,50, \
         49,48,47,46,45,44,43,42,41,40, \
         39,38,37,36,35,34,33,32,31,30, \
         29,28,27,26,25,24,23,22,21,20, \
         19,18,17,16,15,14,13,12,11,10, \
         9,8,7,6,5,4,3,2,1,0

#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)
#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__

#define OVERLOAD(prefix, ...) \
    CAT(prefix, PP_NARG(__VA_ARGS__)) \
    /**/

#ifndef NO_CPS_PROTO
extern cps int cpc_io_wait(int fd, int direction, cpc_condvar *cond);
extern cps int cpc_sleep(int sec, int usec, cpc_condvar *cond);
extern cps int cpc_wait(cpc_condvar *cond);
extern cps void cpc_yield(void);
extern cps cpc_sched *cpc_set_sched(cpc_sched *pool);

#define CPC_IO_WAIT_2(fd, direction) cpc_io_wait(fd, direction, NULL)
#define CPC_IO_WAIT_3(fd, direction, c)   cpc_io_wait(fd, direction, c)

#define cpc_io_wait(...) \
    OVERLOAD(CPC_IO_WAIT_, __VA_ARGS__)(__VA_ARGS__) \
    /**/


#define CPC_SLEEP_1(sec) cpc_sleep(sec, 0, NULL)
#define CPC_SLEEP_2(sec, usec) cpc_sleep(sec, usec, NULL)
#define CPC_SLEEP_3(sec, usec, c)   cpc_sleep(sec, usec, c)

#define cpc_sleep(...) \
    OVERLOAD(CPC_SLEEP_, __VA_ARGS__)(__VA_ARGS__) \
    /**/

/* cpc_wait with timeout is handled via cpc_sleep */
#define CPC_WAIT_1(c) cpc_wait(c)
#define CPC_WAIT_2(c, s) cpc_sleep(s, 0, c)
#define CPC_WAIT_3(c, s, ms)   cpc_sleep(s, ms, c)

#define cpc_wait(...) \
    OVERLOAD(CPC_WAIT_, __VA_ARGS__)(__VA_ARGS__) \
    /**/
#endif


