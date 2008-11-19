/* 
Copyright (c) 2004, 2005 by Juliusz Chroboczek.
Experimental; do not redistribute.
*/

#pragma cilnoremove("type cpc_continuation", "type cpc_function", "type cpc_condvar")
struct cpc_continuation;
typedef void cpc_function(void*);
typedef struct cpc_condvar cpc_condvar;

typedef struct cpc_continuation {
    struct cpc_continuation *next;
    struct cpc_condvar *condvar;
    struct cpc_continuation *cond_next;
    int state;
    unsigned short length;
    unsigned short size;
    char c[1];
} cpc_continuation;

void cpc_continuation_free(struct cpc_continuation *c);
struct cpc_continuation *cpc_continuation_expand(struct cpc_continuation *c,
                                                 int n);
struct cpc_continuation *cpc_continuation_copy(struct cpc_continuation *c);

extern cpc_continuation *cpc_ready_1;
        
#pragma cilnoremove("cpc_alloc")
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

#pragma cilnoremove("cpc_dealloc")
static inline void*
cpc_dealloc(struct cpc_continuation *c, int s)
{
    c->length -= s;
    return c->c + c->length;
}

static void
cpc_really_invoke_continuation(struct cpc_continuation *c)
{
    extern void abort();
    extern void free();

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
    cpc_ready_1 = c;
    return;
}
#endif

#define CPC_IO_IN 1
#define CPC_IO_OUT 2

static inline struct cpc_continuation *
cpc_continuation_push(cpc_continuation *c, cpc_function *f)
{
    if(c == (void*)0 || sizeof(cpc_function*) > c->size - c->length)
        c = cpc_continuation_expand(c, sizeof(cpc_function*));

    *(cpc_function**)(c->c + c->length) = f;
    c->length += sizeof(cpc_function*);
    return c;
}

#define cpc_continuation_patch(cont, size, value) \
    do { \
        void *cpc_arg; \
        cpc_arg = \
            ((cont)->c + (cont)->length - sizeof(cpc_function*) - size); \
        memcpy(cpc_arg, &value, size); \
    } while(0)

void cpc_schedule(struct cpc_continuation *cont);
extern void cpc_main_loop(void);
extern void cpc_prim_sleep(int, int, cpc_condvar*, cpc_continuation*);
cpc_condvar *cpc_condvar_get(void);
cpc_condvar *cpc_condvar_retain(cpc_condvar*);
void cpc_condvar_release(cpc_condvar*);
void cpc_prim_wait(cpc_condvar*, cpc_continuation*);
void cpc_signal(cpc_condvar*);
void cpc_signal_all(cpc_condvar*);
void cpc_prim_io_wait(int, int, cpc_condvar*, cpc_continuation*);
