/*******************************************************************************
 * (C) Xenadyne Inc. 2002.  All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice appears in all copies.
 *
 * XENADYNE INC DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL XENADYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM THE
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * File: nft_pool.c
 *
 * Description:	Thread pool package.
 *
 * This package provides thread pools (AKA "work queues") and is modeled
 * on the code presented in Butenhof, section 7.2, first edition.
 *
 * $Id: nft_pool.c,v 1.6 2004/04/10 03:42:38 sean Exp $
 *******************************************************************************
 */
#include <assert.h>
#include <errno.h>
#include <stdlib.h>

#include <nft_gettime.h>
#include <nft_pool.h>

static void pool_cleanup(void * arg);
static void destroy_cleanup(void * arg);

/*
 * work_item_t	- Structure representing a work request.
 */
typedef struct work_item
{
    struct work_item * next;

    void (*function)(void *);
    void  *argument;

} work_item_t;


/*
 * nft_pool_t	- Structure describing a thread pool.
 */
struct nft_pool
{
    pthread_mutex_t	mutex;
    pthread_cond_t	cond;
    pthread_attr_t	attr;		// Create detached threads.

    work_item_t		*head, *tail;	// head and tail of work queue.

    int			valid;		// Has queue been initialized/destroyed?
    int			quit;		// pool should quit.
    int			num_threads;
    int			max_threads;
    int			idle_threads;
};

/* Live pool's should have valid equal to this.
 */
#define  POOL_VALID 0x1234ABCD

/*
 * DefaultQ	- This pool is used by default when none is specified.
 */
static nft_pool_t     * DefaultQ;
static pthread_once_t	DefaultQOnce = PTHREAD_ONCE_INIT;

static void default_q_init(void)
{
    DefaultQ = nft_pool_create(500, 128*1024);
}


/*
 * Free the resources associated with a pool.
 */
static void
pool_free(void * arg)
{
    nft_pool_t * pool = arg;
    int          r;

    /* The pool is not freeable until no work items are left.
     */
    assert(NULL == pool->head);

    r = pthread_mutex_unlock(&pool->mutex);
    assert(0 == r);

    r = pthread_mutex_destroy(&pool->mutex);
    assert(0 == r);

    r = pthread_cond_destroy(&pool->cond);
    assert(0 == r);

    r = pthread_attr_destroy(&pool->attr);
    assert(0 == r);

    free(pool);
}


/*------------------------------------------------------------------------------
 *
 * nft_pool_thread	- Thread start function to serve the pool's work queue.
 *
 *------------------------------------------------------------------------------
 */
static void *
nft_pool_thread(void * arg)
{
    nft_pool_t   * pool = arg;
    work_item_t  * item;
    int            r;

    r = pthread_mutex_lock(&pool->mutex);
    assert(0 == r);

    pthread_cleanup_push(pool_cleanup, pool);

    while (1)
    {
	struct timespec timeout;
	int             timedout = 0;

	gettime(&timeout);
	timeout.tv_sec++;

	while (!pool->head && !pool->quit)
	{
	    /* Wait up to one second for new work to arrive.
	     */
	    pool->idle_threads++;
	    r = pthread_cond_timedwait(&pool->cond, &pool->mutex, &timeout);
	    pool->idle_threads--;

	    if (r != 0)
	    {
		assert(ETIMEDOUT == r);	/* We should not get other errors. */
		timedout = 1;
		break;
	    }
	}

	if ((item = pool->head))
	{
	    void (* function)(void *) = item->function;
	    void  * argument          = item->argument;

	    /* Pop the item from the queue.
	     */
	    pool->head = item->next;
	    if (pool->tail == item)
		pool->tail =  NULL;

	    r = pthread_mutex_unlock(&pool->mutex);
	    assert(0 == r);

	    /* Free the item first, in case function calls pthread_exit().
	     */
	    free(item);

	    /* Push a handler to recover the mutex if function exits.
	     */
	    pthread_cleanup_push((void(*)(void*)) pthread_mutex_lock, &pool->mutex);

	    function(argument);

	    /* Reacquire the mutex.
	     */
	    pthread_cleanup_pop(1);
	}

	/* If there are no more work requests, and the servers
	 * have been asked to quit, then shut down this worker.
	 */
	if (!pool->head && pool->quit)
	{
	    /* Last one out wakes the thread in nft_pool_destroy().
	     */
	    if (pool->num_threads == 1)
	    {
		r = pthread_cond_broadcast(&pool->cond);
		assert(0 == r);
	    }
	    break;
	}

	/* If there's no more work, and we timed out of the wait,
	 * terminate this worker thread.
	 */
	if (!pool->head && timedout)
	    break;
    }

    /* Perform cleanup on exit.
     */
    pthread_cleanup_pop(1);

    return NULL;
}

/*
 * A cleanup function for nft_pool_thread().
 */
static void
pool_cleanup(void * arg)
{
    nft_pool_t * pool = arg;

    /* Workers are private threads, so they cannot be cancelled,
     * but the work function could call pthread_exit().
     * The mutex must be held when this function executes.
     */
    pool->num_threads--;

    /* If the pool is invalid and we're the last thread to exit,
     * and if the quit flag requests a worker free, free the pool.
     */
    if (!pool->valid &&
	(pool->num_threads == 0) &&
	(pool->quit         > 1))
    {
	pool_free(pool);
    }
    else
	pthread_mutex_unlock(&pool->mutex);
}


/*------------------------------------------------------------------------------
 *
 * nft_pool_create	- Create a thread pool.
 *
 * The max_threads parameter will be forced to one or more.
 * The stack_size may be zero, in which case the default stack size will be
 * used. If stack_size is nonzero, and less than 16K, it is forced to 16K.
 *
 * Returns NULL on malloc failure.
 *
 *------------------------------------------------------------------------------
 */
nft_pool_t *
nft_pool_create(int max_threads, int stack_size)
{
    nft_pool_t * pool = malloc(sizeof(nft_pool_t));
    int          r;

    if (pool == NULL)
	return NULL;

    if (max_threads < 1)
	max_threads = 1;

    if (stack_size != 0 &&
	stack_size < 16*1024)
	stack_size = 16*1024;

    r = pthread_mutex_init(&pool->mutex, NULL);
    assert(0 == r);

    r = pthread_cond_init(&pool->cond, NULL);
    assert(0 == r);

    r = pthread_attr_init(&pool->attr);
    assert(0 == r);

    r = pthread_attr_setdetachstate(&pool->attr, PTHREAD_CREATE_DETACHED);
    assert(0 == r);

    if (stack_size > 0)
    {
	r = pthread_attr_setstacksize(&pool->attr, stack_size);
	assert(0 == r);
    }

    pool->quit = 0;
    pool->head = pool->tail = NULL;
    pool->num_threads  = 0;
    pool->max_threads  = max_threads;
    pool->idle_threads = 0;
    pool->valid        = POOL_VALID;

    return pool;
}


/*------------------------------------------------------------------------------
 *
 * nft_pool_destroy	- Free resources associated with thread pool
 *
 *------------------------------------------------------------------------------
 */
int
nft_pool_destroy(nft_pool_t * pool)
{
    int r;

    assert(POOL_VALID == pool->valid);

    if (pool->valid != POOL_VALID)
	return EINVAL;

    r = pthread_mutex_lock(&pool->mutex);
    assert(0 == r);

    pool->valid = 0;	/* Mark as a dead pool. */
    pool->quit  = 1;	/* Tell worker threads to quit. */

    /* Push a cleanup, in case we're cancelled during pthread_cond_wait().
     */
    pthread_cleanup_push(destroy_cleanup, pool);

    /* Check whether any threads are active, and run them down;
     *
     * 1. Set the quit flag.
     * 2. Broadcast to wake idle threads.
     * 3. Wait for num_threads to go to zero.
     */
    if (pool->num_threads > 0)
    {
	if (pool->idle_threads > 0)
	{
	    r = pthread_cond_broadcast(&pool->cond);
	    assert(0 == r);
	}

	/* Wait for all of the threads to exit.
	 */
	while (pool->num_threads > 0)
	{
	    r = pthread_cond_wait(&pool->cond, &pool->mutex);
	    assert(0 == r);
	}
    }

    /* Execute the cleanup function.
     */
    pthread_cleanup_pop(1);

    return 0;
}

/*
 * A cleanup function for nft_pool_destroy().
 */
static void
destroy_cleanup(void * arg)
{
    nft_pool_t * pool = arg;
    int          r;

    if (pool->num_threads > 0)
    {
	/* nft_pool_destroy was cancelled while waiting for a worker
	 * thread to finish. Increment pool->quit, to indicate that the
	 * last worker out should free the pool.
	 */
	pool->quit++;

	r = pthread_mutex_unlock(&pool->mutex);
	assert(0 == r);
    }
    else
	pool_free(pool);
}


/*------------------------------------------------------------------------------
 *
 * nft_pool_add	- Add a work item to the pool's queue.
 *
 *------------------------------------------------------------------------------
 */
int
nft_pool_add(nft_pool_t * pool, void (*function)(void *),  void * argument)
{
    work_item_t * item;
    pthread_t     id;
    int           r;

    /* If the caller doesn't specify a pool, use the default pool.
     */
    if (pool == NULL)
    {
	r = pthread_once(&DefaultQOnce, default_q_init);
	assert(0 == r);

	pool = DefaultQ;

	if (!pool)
	    return ENOMEM;
    }

    if (pool->valid != POOL_VALID)
	return EINVAL;

    /* Allocate a work item.
     */
    if (!(item = malloc(sizeof(work_item_t))))
	return ENOMEM;

    item->function = function;
    item->argument = argument;
    item->next     = NULL;

    r = pthread_mutex_lock(&pool->mutex);
    assert(0 == r);

    /* Add this request to the end of the queue,
     * updating the head and tail pointers.
     */
    if (pool->head == NULL)
	pool->head =  item;
    else
	pool->tail->next = item;
    pool->tail = item;

    /* If there are idle threads, wake one.
     */
    if (pool->idle_threads > 0)
    {
	r = pthread_cond_signal(&pool->cond);
	assert(0 == r);
    }
    else if (pool->num_threads < pool->max_threads)
    {
	/* Spawn a new thread.
	 */
	if ((r = pthread_create(&id, &pool->attr, nft_pool_thread, pool)) == 0)
	{
	    pool->num_threads++;
	}
	else if (pool->num_threads == 0)
	{
	    /* Thread spawn failed, and no other threads are running,
	     * so return an error code.
	     */
	    pthread_mutex_unlock(&pool->mutex);
	    return r;
	}
    }

    r = pthread_mutex_unlock(&pool->mutex);
    assert(0 == r);

    return r;
}


/*******************************************************************************
 *******************************************************************************
 *
 *				TEST DRIVER
 *
 *******************************************************************************
 *******************************************************************************
 */
#ifdef MAIN

#include <stdio.h>
#ifndef WIN32
#include <unistd.h>
#else
#include <winbase.h>
#define sleep(n) _sleep(n*1000)
#endif

/*
 * This is an obnoxious function that exits the pool thread.
 */
void test_exit(void * arg)
{
    pthread_exit(0);
}

int
main(int argc, char *argv[])
{
    nft_pool_t * pl;
    int		 i, n;

    /* Do a basic smoke test.
     */
    pl = nft_pool_create(20, 128*1024);
    assert(NULL != pl);
    nft_pool_add(pl, (void(*)(void*)) puts, "Foo");
    nft_pool_add(pl, (void(*)(void*)) puts, "Bar");
    nft_pool_add(pl, (void(*)(void*)) puts, "Faz");
    nft_pool_destroy(pl);   /* Waits for queue to empty */
    assert(POOL_VALID != pl->valid);
    fputs("Test 1 passed.\n\n", stderr);

    /* Test the default pool.
     */
    nft_pool_add(NULL, (void(*)(void*)) puts, "Hello");
    nft_pool_add(NULL, (void(*)(void*)) puts, "Goodbye");
    sleep(1);
    assert(NULL == DefaultQ->head);
    fputs("Test 2 passed.\n\n", stderr);

    /* Test that destroy blocks until work is finished.
     */
    pl = nft_pool_create(20, 128*1024);
    nft_pool_add(pl, (void(*)(void*)) sleep, (void*) 2);
    sleep(1);
    nft_pool_destroy(pl);   /* Waits for queue to empty */
    assert(POOL_VALID != pl->valid);
    fputs("Test 3 passed.\n\n", stderr);

    /* Stress/Performance test - push many work items through the queue.
     */
    n = 100000;
    fprintf(stderr, "Test 4: processing %d tasks...", n);
    pl = nft_pool_create(20, 64*1024);
    for (i = 0; i < n; i++)
	nft_pool_add(pl, (void(*)(void*)) random, NULL);
    nft_pool_destroy(pl);   /* Waits for queue to empty */
    fputs("passed.\n\n", stderr);


#ifndef WIN32
    /* The following tests fail on WIN32, either because
     * cancellation is not supported, or the cleanup handlers
     * aren't run on thread exit.
     */

    /* Test a function that exits the pool thread.
     */
    pl = nft_pool_create(0, 0);
    nft_pool_add(pl, test_exit, 0);
    sleep(1);
    assert(0 == pl->num_threads);

    /* Verify that the pool is still functional.
     */
    nft_pool_add(pl, (void(*)(void*)) puts, "Test 5 passed.\n\n");
    sleep(1);

    /* Test cancellation in nft_pool_destroy.
     *
     * We submit a 4-second sleep to keep the worker thread busy,
     * then spawn nft_pool_destroy in a thread. That thread will
     * block waiting for the sleep to finish, whereupon we cancel
     * it. The canceled destroy must increment pl->quit to 2, and
     * the worker thread must free the pool when it exits.
     */
    {
	pthread_t thread;
	int       rc;
	nft_pool_add(pl, (void(*)(void*)) sleep, (void*) 4);
	sleep(1);
	rc = pthread_create(&thread, NULL, (void* (*)(void*)) nft_pool_destroy, pl);
	assert(0 == rc);
	sleep(1);
	rc = pthread_cancel(thread);
	assert(0 == rc);
	sleep(1);
	assert(2 == pl->quit);
	assert(1 == pl->num_threads);
	sleep(1);
	assert(0 == pl->num_threads);
	fputs("Test 6 passed.\n\n", stderr);
    }
#endif /* WIN32 */

#ifdef NDEBUG
    printf("You must recompile this test driver without NDEBUG!\n");
#else
    printf("All tests passed.\n");
#endif
    exit(0);
}


#endif /* MAIN */


