/******************************************************************************
 * (C) Copyright Xenadyne, Inc. 2002  All rights reserved.
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
 * File:  nft_pool.h
 *
 * PURPOSE
 *
 * This package provides a facility to submit tasks to a thread pool.
 * The thread pool is associated with one or more threads that execute
 * the tasks asynchronously.
 *
 * This package is derived from Butenhof, section 7.2, first edition,
 * but with certain changes of style. The create and destroy functions
 * operate on dynamically allocated memory, so it isn't possible to
 * initialize a static pool instance. Also, it is possible to call
 * nft_pool_add(), passing NULL as the thread pool pointer. This will
 * cause the work item to be performed by an internal "default" pool.
 *
 ******************************************************************************
 */
#ifndef nft_pool_h
#define nft_pool_h

#include <pthread.h>

typedef struct nft_pool  nft_pool_t;

/*
 * nft_pool_create
 *
 *	Initialize a thread pool.
 *
 *	The max_threads argument sets the maximum number of worker threads
 *	that will be created to service work items. It must be one or higher.
 *
 *	The stack_size argument sets the maximum size in bytes of the worker
 *	thread's stack region. It can be zero, in which case platform default
 *	is used, otherwise it must be 16K or more.
 *
 *	If either argument does not satisfy the required minimum, it will be
 *	silently increased to the minimum value.
 *	
 *	Returns NULL on a malloc failure.
 */
nft_pool_t   * nft_pool_create(int max_threads, int stack_size);


/*
 * nft_pool_destroy
 *
 *	Free resources associated with thread pool. If work items are in the
 *	pool, the calling thread blocks until all of the tasks have been
 *	performed. No new tasks may be added in this time. While waiting,
 *	the calling thread may be cancelled, in which case the pool will be
 *	freed when and if the last worker thread exits.
 *
 *	Returns zero on success, otherwise:
 *		EINVAL	The pool argument is not a valid thread pool.
 *
 */
int	nft_pool_destroy(nft_pool_t * pool);


/*
 * nft_pool_add
 *
 *	Submit a work item to the pool. The caller may specify	a null pool,
 *	in which case the item is added to a built-in default thread pool.
 *
 *	Returns zero on success, otherwise:
 *		EINVAL	The pool argument is not a valid thread pool.
 *		ENOMEM	malloc() failed.
 */
int	nft_pool_add(nft_pool_t * pool, void (*function)(void *), void * argument);


#endif	/* nft_pool_h */

