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

#include <stddef.h> // size_t
#include <time.h>

#ifdef CPC_COMPACT_CONTINUATIONS

#define MAX_ALIGN 1
#define PTR_SIZE (sizeof(cpc_function *))

#else

#ifdef __BIGGEST_ALIGNMENT__
#define MAX_ALIGN __BIGGEST_ALIGNMENT__
#else
#define MAX_ALIGN __alignof(struct {char c; } __attribute__((__aligned__)))
#endif
#define PTR_SIZE MAX_ALIGN

#endif

typedef struct cpc_condvar cpc_condvar;
typedef struct cpc_sched cpc_sched;
extern cpc_sched *cpc_default_pool;
#define cpc_default_sched NULL

typedef struct cpc_continuation {
    struct cpc_continuation *next;
    struct cpc_condvar *condvar;
    struct cpc_continuation *cond_next;
    cpc_sched *sched;
    int state;
    unsigned short length;
    unsigned short size;
    char c[1];
} cpc_continuation;

typedef cpc_continuation *cpc_function(void*);

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

#define CPC_IO_IN 1
#define CPC_IO_OUT 2
#define CPC_TIMEOUT 4
#define CPC_CONDVAR 8

static inline struct cpc_continuation *
cpc_continuation_push(cpc_continuation *c, cpc_function *f)
{

    if(c == (void*)0 || PTR_SIZE > c->size - c->length)
        c = cpc_continuation_expand(c, PTR_SIZE);

    *(cpc_function**)(c->c + c->length) = f;
    c->length += PTR_SIZE;
    return c;
}

static inline void
cpc_continuation_patch(cpc_continuation *cont, size_t size, void *value)
{
  void *cpc_arg;
  cpc_arg =
    ((cont)->c + (cont)->length - PTR_SIZE - ((size - 1) / MAX_ALIGN + 1) * MAX_ALIGN);
  __builtin_memcpy(cpc_arg, value, size);
  return;
}

extern void cpc_main_loop(void);
extern void cpc_print_continuation(struct cpc_continuation *c, char *s);

extern cpc_condvar *cpc_condvar_get(void);
extern cpc_condvar *cpc_condvar_retain(cpc_condvar*);
extern void cpc_condvar_release(cpc_condvar*);
extern void cpc_signal_fd(int fd, int direction);
extern void cpc_signal(cpc_condvar*);
extern void cpc_signal_all(cpc_condvar*);
extern int cpc_condvar_count(cpc_condvar*);

extern void cpc_prim_spawn(struct cpc_continuation*, struct cpc_continuation*);

extern cpc_sched *cpc_threadpool_get(int);
extern int cpc_threadpool_release(cpc_sched *);

#ifndef NO_CPS_PROTO
extern cps int cpc_io_wait(int fd, int direction, cpc_condvar *cond);
extern cps int cpc_sleep(int sec, int usec, cpc_condvar *cond);
extern cps int cpc_wait(cpc_condvar *cond);
extern cps void cpc_yield(void);
extern cps void cpc_done(void);
extern cps cpc_sched *cpc_attach(cpc_sched *pool);

extern cpc_sched *(__attribute__((cpc_need_cont))cpc_get_sched)(void);
extern int (__attribute__((cpc_need_cont))cpc_gettimeofday)(struct timeval *tv);
extern time_t (__attribute__((cpc_need_cont,cpc_no_retain))cpc_time)(time_t *t);

#define cpc_is_detached() (cpc_get_sched() != cpc_default_sched)
#define cpc_detach() cpc_attach(cpc_is_detached() ? cpc_get_sched() : cpc_default_pool)
#define cpc_detached cpc_attached(cpc_is_detached() ? cpc_get_sched() : cpc_default_pool)

#endif
