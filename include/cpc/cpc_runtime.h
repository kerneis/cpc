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

/* CPC cannot parse anonymous functions (aka Apple's "blocks") */
#undef __BLOCKS__

#include <stddef.h> // size_t
#include <time.h>

/* If you want to build with the --packed option, do not forget to set:
#define CPC_COMPACT_CONTINUATIONS 1
This is broken on some architectures.
*/

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

#define UGLY_HACK_RETVAL 1

typedef struct cpc_condvar cpc_condvar;
typedef struct cpc_sched cpc_sched;
extern cpc_sched *cpc_default_threadpool;
#define cpc_default_sched NULL

typedef struct cpc_continuation {
    unsigned short length;
    unsigned short size;
#ifdef UGLY_HACK_RETVAL
    void *cpc_retval; // FIXME UGLY HACK
#endif
    char c[1];
} cpc_continuation;

extern void cpc_print_continuation(struct cpc_continuation *c, char *s);

typedef cpc_continuation *cpc_function(void*);

struct cpc_continuation *cpc_continuation_expand(struct cpc_continuation *c,
                                                 int n);

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
cpc_continuation_patch(cpc_continuation *cont, size_t size, const void *value)
{
  void *cpc_arg;
  cpc_arg =
#ifdef UGLY_HACK_RETVAL
    ((cont)->cpc_retval);
  if(cpc_arg == NULL) return; /* FIXME ugly ugly caller sent us NULL */
#else
    ((cont)->c + (cont)->length - PTR_SIZE - ((size - 1) / MAX_ALIGN + 1) * MAX_ALIGN);
#endif
  __builtin_memcpy(cpc_arg, value, size);
  return;
}

extern void cpc_main_loop(void);

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
extern cps cpc_sched *cpc_link(cpc_sched *pool);

extern cpc_sched *cpc_get_sched(void) __attribute__((cpc_need_cont));
extern int cpc_gettimeofday(struct timeval *tv) __attribute__((cpc_need_cont));
extern time_t cpc_time(time_t *t) __attribute__((cpc_need_cont,cpc_no_retain));

#define cpc_is_detached() (cpc_get_sched() != cpc_default_sched)
#define cpc_attach()  cpc_link  (cpc_is_detached() ? cpc_default_sched : cpc_get_sched())
#define cpc_attached  cpc_linked(cpc_is_detached() ? cpc_default_sched : cpc_get_sched())
#define cpc_detach()  cpc_link  (cpc_is_detached() ? cpc_get_sched()   : cpc_default_threadpool)
#define cpc_detached  cpc_linked(cpc_is_detached() ? cpc_get_sched()   : cpc_default_threadpool)

#endif

/* Safe functions */

#pragma cpc_no_retain("writev", "curl_easy_getinfo", "snprintf", "memcmp",  "memcpy")
#pragma cpc_no_retain("getpeername", "setsockopt", "memset", "bind", "accept", "recvfrom")
