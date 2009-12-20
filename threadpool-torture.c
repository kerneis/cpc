#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>

#ifndef NIFTY
#include "threadpool.h"
#else
#include "nft_pool.h"
#endif

#define N 1000000
#define NTHREADS 10

#ifndef NIFTY
threadpool_t *threadpool;
#else
nft_pool_t *threadpool;
#endif

int sum = 0;

#ifdef MAIN_FUNC
static void
main_func(void *v)
{
    return;
}
#endif

static void
thread_func(void *v)
{
    __sync_fetch_and_add(&sum, 1);
#ifdef MAIN_FUNC
    threadpool_schedule_back(threadpool, main_func, v);
#endif
}

int
main()
{
    int i, rc;
    int p[2];
    int done;

    rc = pipe(p);
    if(rc < 0) {
        perror("pipe");
        exit(1);
    }

#ifndef NIFTY
    threadpool = threadpool_create(NTHREADS, NULL, NULL);
#else
    threadpool = nft_pool_create(NTHREADS, 0);
#endif
    if(threadpool == NULL) {
        perror("threadpool_create");
        exit(1);
    }

    for(i = 0; i < N; i++)
#ifndef NIFTY
        threadpool_schedule(threadpool, thread_func, NULL);
#else
        nft_pool_add(threadpool, thread_func, NULL);
#endif

#ifndef NIFTY
    do {
        done = threadpool_die(threadpool, 1);
        threadpool_items_run(threadpool_get_back(threadpool));
    } while(!done);

    threadpool_destroy(threadpool);
#else
    nft_pool_destroy(threadpool);
#endif

    if(sum != N)
        printf("Got %d, expected %d.\n", sum, N);
    else
        printf("Ok.\n");
        
    return 0;
}
