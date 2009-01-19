/*
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").  You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license in the COPYING file
 * or http://www.opensolaris.org/license/index.html.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * Gary Pennington <Gary.Pennington@dowstone.com>
 */

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "wp.h"

/*
 * Worker state
 */

/**
 * A worker thread is available for work.
 */
#define	WORKER_AVAIL	0x0

/**
 * A worker thread is busy.
 */
#define	WORKER_BUSY	0x1

/**
 * A worker thread is terminating.
 */
#define	WORKER_DIE	0x2

#if SOLARIS
#define	WP_GETTIME	(int64_t) gethrtime
#else
static int64_t
linux_hrtime(void)
{
	int64_t res;
	struct timeval tv;

	gettimeofday(&tv, NULL);
	res = tv.tv_sec * 1000000000;
	res += tv.tv_usec * 1000;

	return (res);
}
#define	WP_GETTIME	linux_hrtime
#endif

/*
 * Worker Pool Task
 */
typedef struct wp_thread_task {
	wp_process_cb		wtt_process; 	/* Processing */
	wp_report_cb		wtt_report; 	/* Reporting */
	int64_t			wtt_start;	/* Started */
	void			*wtt_data;	/* User Data */
	struct wp_thread_task	*wtt_next;	/* List Member */
} wp_thread_task_t;

/*
 * Worker Pool Thread
 */
typedef struct wp_thread {
	wp_t			*wpt_pool;	/* Owner */
	int			wpt_state;	/* Thread State */
	pthread_t		wpt_tid;	/* Thread Identifier */
	wp_thread_task_t	*wpt_task;	/* Task Details */
	pthread_cond_t		wpt_state_cv;	/* Thread State Condition */
	pthread_mutex_t		wpt_lock;	/* Thread Lock */
	struct wp_thread	*wpt_next;	/* List Member */
} wp_thread_t;

/*
 * Worker Pool.
 */
struct wp {
	pthread_mutex_t		wp_lock;	/* Pool Lock */
	pthread_cond_t		wp_state_cv;	/* Pool State Condition */
	int			wp_state;	/* Pool State */
	int			wp_size;	/* Pool Size */
	int			wp_avail;	/* Current Availability */
	wp_stats_t		*wp_stats;	/* Performance Statistics */
	wp_process_cb		wp_process; 	/* Default Processing */
	wp_report_cb		wp_report; 	/* Default Reporting */
	wp_thread_t		*wp_threads;	/* Pool Threads */
	wp_thread_t		*wp_d_threads;	/* Pool Dead Threads */
	wp_thread_task_t	*wp_queue_h;	/* Pool Queue Head */
	wp_thread_task_t	*wp_queue_t;	/* Pool Queue Tail */
};

/*
 * Globals
 */

static pthread_mutex_t wp_log_lock = PTHREAD_MUTEX_INITIALIZER;

static void *wp_worker_harness(void *);

/*
 * Functions
 */

#if DEBUG
/*
 * Dump the state of a thread for debugging purposes.
 */
static void
dump_thread(wp_thread_t *wpt)
{
	wp_log("tid: %d\n", wpt->wpt_tid);
	wp_log("\tstate: %d\n", wpt->wpt_state);
	wp_log("\tpool: %p\n", wpt->wpt_pool);
}
#endif	/* DEBUG */


/*
 * Queue a task for later processing
 */
static void
wp_queue_task(wp_t *wp, wp_thread_task_t *task)
{
	if (wp->wp_queue_h) {
		wp->wp_queue_t->wtt_next = task;
		wp->wp_queue_t = task;
	} else {
		wp->wp_queue_h = wp->wp_queue_t = task;
	}
}
	
/*
 * Allocate and queue a task for later processing.
 */
static int
wp_queue(wp_t *wp, void *data, wp_process_cb worker, wp_report_cb report)
{
	wp_thread_task_t *task;

	if ((task = malloc(sizeof (*task))) == NULL)
		return (WP_FAIL);
	task->wtt_data = data;
	task->wtt_process = worker;
	task->wtt_report = report;
	task->wtt_next = NULL;
	task->wtt_start = WP_GETTIME();
	wp_queue_task(wp, task);
	return (WP_SUCCESS);
}

/*
 * Dequeue a task for processing. It is the responsibility of the
 * caller to free the resources associated with the task.
 */
static wp_thread_task_t *
wp_dequeue(wp_t *wp)
{
	wp_thread_task_t *task;

	if (wp->wp_queue_h == NULL)
		return (NULL);
	task = wp->wp_queue_h;
	wp->wp_queue_h = wp->wp_queue_h->wtt_next;
	if (wp->wp_queue_h == NULL)
		wp->wp_queue_t = NULL;
	return (task);
}

/*
 * Provide a mechanism to keep the thread alive and active within the
 * pool. The harness is executed when the thread is created and waits
 * for work to be added to the pool (by wp_run()). Once work
 * is available, as long as the Pool is still active the user function
 * is invoked.
 */
static void *
wp_worker_harness(void *arg)
{
	wp_thread_t *wpt = (wp_thread_t *)arg;
	void *ret = NULL;
	int64_t duration;

	/*CONSTCOND*/
	while (1) {
		duration = 0;

		(void) pthread_mutex_lock(&wpt->wpt_lock);
		while (wpt->wpt_state == WORKER_AVAIL) {
			(void) pthread_cond_wait(&wpt->wpt_state_cv,
			    &wpt->wpt_lock);
		}
		(void) pthread_mutex_unlock(&wpt->wpt_lock);
		if (wpt->wpt_task) {
			if (wpt->wpt_task->wtt_process)
				ret = wpt->wpt_task->wtt_process(
				    wpt->wpt_task->wtt_data);
			duration = WP_GETTIME() - wpt->wpt_task->wtt_start;
			if (wpt->wpt_task->wtt_report)
				wpt->wpt_task->wtt_report(WP_SUCCESS, ret);
			free(wpt->wpt_task);
			wpt->wpt_task = NULL;
		}
		(void) pthread_mutex_lock(&wpt->wpt_pool->wp_lock);
		(void) pthread_mutex_lock(&wpt->wpt_lock);
		if (wpt->wpt_pool->wp_stats && duration) {
			wpt->wpt_pool->wp_stats->wps_succeeded++;
			wpt->wpt_pool->wp_stats->wps_avg_duration +=
			    (duration -
				wpt->wpt_pool->wp_stats->wps_avg_duration) /
			    (int64_t) wpt->wpt_pool->wp_stats->wps_succeeded;
		}
		if (wpt->wpt_state == WORKER_DIE) {
			(void) pthread_mutex_unlock(&wpt->wpt_lock);
			(void) pthread_mutex_unlock(&wpt->wpt_pool->wp_lock);
			break;
		}
		if (wpt->wpt_pool->wp_queue_h == NULL) {
			wpt->wpt_state = WORKER_AVAIL;
			wpt->wpt_pool->wp_avail++;
			if (wpt->wpt_pool->wp_size == wpt->wpt_pool->wp_avail)
				(void) pthread_cond_broadcast(
				    &wpt->wpt_pool->wp_state_cv);
		} else {
			wpt->wpt_task = wp_dequeue(wpt->wpt_pool);
		}
		(void) pthread_mutex_unlock(&wpt->wpt_pool->wp_lock);
		(void) pthread_mutex_unlock(&wpt->wpt_lock);
	}
	return (arg);
}

/*
 * Add a thread to the specified pool.
 */
static wp_thread_t *
wp_add_thread(wp_t *wp)
{
	wp_thread_t *wpt;
	wp_thread_task_t *task;

	
	if ((wpt = malloc(sizeof (wp_thread_t))) == NULL)
		return (NULL);
	(void) memset(wpt, 0, sizeof (wp_thread_t));
	wpt->wpt_state = WORKER_AVAIL;
	wpt->wpt_pool = wp;
	(void) pthread_cond_init(&wpt->wpt_state_cv, NULL);
	(void) pthread_mutex_init(&wpt->wpt_lock, NULL);
	if ((task = wp_dequeue(wp))) {
		wpt->wpt_state = WORKER_BUSY;
		wpt->wpt_task = task;
		wp->wp_avail--;
	}

	if (pthread_create(&wpt->wpt_tid, NULL, wp_worker_harness, wpt) < 0) {
		if (task)
			wp_queue_task(wp, task);
		(void) pthread_cond_destroy(&wpt->wpt_state_cv);
		(void) pthread_mutex_destroy(&wpt->wpt_lock);
		free(wpt);
		return (NULL);
	}
	wpt->wpt_next = wp->wp_threads;
	wp->wp_threads = wpt;
	return (wpt);
}

/*
 * Wait for the thread to finish terminating and then join it. Once
 * the join is complete, free the thread's associated resources.
 */
static int
wp_join_thread(wp_thread_t *wpt)
{
	int ret = WP_SUCCESS;

	ret = pthread_join(wpt->wpt_tid, NULL);
	(void) pthread_cond_destroy(&wpt->wpt_state_cv);
	(void) pthread_mutex_destroy(&wpt->wpt_lock);
	if (wpt->wpt_task)
		free(wpt->wpt_task);
	free(wpt);
	return (ret);
}

/*
 * Remove a thread from the supplied pool's set of threads. Add the
 * thread to the set of deleted threads which we need to join later.
 */
static int
wp_rm_thread(wp_t *wp)
{
	wp_thread_t *wpt;

	if (wp->wp_threads == NULL)
		return (WP_FAIL);
	wpt = wp->wp_threads;
	(void) pthread_mutex_lock(&wpt->wpt_lock);
	wp->wp_threads = wpt->wpt_next;
	wp->wp_size --;
	if (wpt->wpt_state ==  WORKER_AVAIL)
		wp->wp_avail--;

	wpt->wpt_state = WORKER_DIE;
	wpt->wpt_next = wp->wp_d_threads;
#if DEBUG
	wp_log("removing %d\n", wpt->wpt_tid);
	dump_thread(wpt);
#endif	/* DEBUG */
	(void) pthread_mutex_unlock(&wpt->wpt_lock);
	(void) pthread_cond_signal(&wpt->wpt_state_cv);
	wp->wp_d_threads = wpt;
	return (WP_SUCCESS);
}

/*
 * One of our threads?
 */
static int
wp_is_pool_thread(wp_t *wp)
{
	wp_thread_t *wpt;

	(void) pthread_mutex_lock(&wp->wp_lock);
	for (wpt = wp->wp_threads; wpt != NULL; wpt = wpt->wpt_next) {
		if (pthread_self() == wpt->wpt_tid) {
			(void) pthread_mutex_unlock(&wp->wp_lock);
			return (WP_TRUE);
		}
	}
	(void) pthread_mutex_unlock(&wp->wp_lock);
	return (WP_FALSE);
}

/*
 * Write the supplied message to stderr
 */
static void
wp_output(const char *fmt, va_list l)
{
	(void) pthread_mutex_lock(&wp_log_lock);
	(void) vfprintf(stderr, fmt, l);
	(void) pthread_mutex_unlock(&wp_log_lock);
}

/*
 * Add threads to the pool as specified in the size. This function
 * must only be used to grow the numbers of threads in the pool.
 */
static int
wp_init(wp_t *wp, uint32_t size)
{
	int i;

	assert (size >= wp->wp_size);

	for (i = wp->wp_size; i < size; i++) {
		if (wp_add_thread(wp) == NULL)
			return (WP_FAIL);
	}
	wp->wp_avail += size - wp->wp_size;
	wp->wp_size = size;
	if (wp->wp_size == wp->wp_avail)
		(void) pthread_cond_broadcast(&wp->wp_state_cv);
	return (WP_SUCCESS);
}

void
wp_die(const char *fmt, ...)
{
	va_list alist;

	va_start(alist, fmt);
	wp_output(fmt, alist);
	va_end(alist);
	exit(1);
}

void
wp_log(const char *fmt, ...)
{
	va_list alist;

	va_start(alist, fmt);
	wp_output(fmt, alist);
	va_end(alist);
}

wp_t *
wp_new(uint32_t size, wp_process_cb worker, wp_report_cb report)
{
	wp_t *wp;

	if (size == 0)
		size = sysconf(_SC_NPROCESSORS_ONLN);
	if ((wp = malloc(sizeof (wp_t))) == NULL)
		return (NULL);
	(void) memset(wp, 0, sizeof (*wp));
	if (pthread_mutex_init(&wp->wp_lock, NULL) != 0) {
		free(wp);
		return (NULL);
	}
	(void) pthread_cond_init(&wp->wp_state_cv, NULL);
	wp->wp_state = WP_ACTIVE;
	wp->wp_process = worker;
	wp->wp_report = report;
	wp->wp_queue_h = wp->wp_queue_t = NULL;
	wp->wp_d_threads = NULL;
	(void) pthread_mutex_lock(&wp->wp_lock);
	if (wp_init(wp, size) != 0) {
		wp_thread_t *wpt, *wpt_tmp;

		for (wpt = wp->wp_threads; wpt != NULL; wpt = wpt_tmp) {
			wpt_tmp = wpt->wpt_next;
			free(wpt);
		}
		(void) pthread_mutex_unlock(&wp->wp_lock);
		(void) pthread_mutex_destroy(&wp->wp_lock);
		free(wp);
		return (NULL);
	}
	(void) pthread_mutex_unlock(&wp->wp_lock);
	return (wp);
}

int
wp_resize(wp_t *wp, uint32_t size)
{
	wp_thread_t *wpt, *wpt_tmp;
	int ret = WP_SUCCESS;

	if (wp_is_pool_thread(wp) == WP_TRUE)
		return (WP_FAIL);

	if (size == 0)
		size = sysconf(_SC_NPROCESSORS_ONLN);

	(void) pthread_mutex_lock(&wp->wp_lock);

	if (wp->wp_state == WP_INACTIVE) {
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (WP_FAIL);
	}

	if (size == wp->wp_size) {
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (WP_SUCCESS);
	}

	/*
	 * Now do the resize. Adding and removing threads as required.
	 */
	if (size  < wp->wp_size) {
		int i, tgt;
		tgt = wp->wp_size - size;
		for (i = 0; i < tgt; i++) {
			if (wp_rm_thread(wp) != WP_SUCCESS) {
				(void) pthread_mutex_unlock(&wp->wp_lock);
				return (WP_FAIL);
			}
		}
		if (wp->wp_size == wp->wp_avail)
			(void) pthread_cond_broadcast(&wp->wp_state_cv);
	} else {
		if (wp_init(wp, size) != WP_SUCCESS) {
			(void) pthread_mutex_unlock(&wp->wp_lock);
			return (WP_FAIL);
		}
	}
	(void) pthread_mutex_unlock(&wp->wp_lock);
	/*
	 * Now wait for all our removed threads to be joined
	 */
	for (wpt = wp->wp_d_threads; wpt != NULL; wpt = wpt_tmp) {
		wpt_tmp = wpt->wpt_next;
		if (wp_join_thread(wpt) != WP_SUCCESS)
			ret = WP_FAIL;
	}
	wp->wp_d_threads = NULL;
	return (ret);
}

int
wp_run(wp_t *wp, void *user)
{
	return (wp_run_task(wp, user, wp->wp_process, wp->wp_report));
}

int
wp_run_task(wp_t *wp, void *user, wp_process_cb worker, wp_report_cb report)
{
	wp_thread_t *wpt;

	(void) pthread_mutex_lock(&wp->wp_lock);
	if (wp->wp_state != WP_ACTIVE) {
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (WP_FAIL);
	}
	if (wp->wp_avail == 0) {
		int ret;

		ret = wp_queue(wp, user, worker, report);
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (ret);
	}

	for (wpt = wp->wp_threads; wpt != NULL; wpt = wpt->wpt_next) {
		(void) pthread_mutex_lock(&wpt->wpt_lock);
		if (wpt->wpt_state == WORKER_AVAIL) {
			wp->wp_avail--;
			if (wp->wp_size == wp->wp_avail)
				(void) pthread_cond_broadcast(&wp->wp_state_cv);
			wpt->wpt_state = WORKER_BUSY;
			wpt->wpt_task =
			    malloc(sizeof (wp_thread_task_t));
			wpt->wpt_task->wtt_data = user;
			wpt->wpt_task->wtt_process = worker;
			wpt->wpt_task->wtt_report = report;
			wpt->wpt_task->wtt_start = WP_GETTIME();
			(void) pthread_mutex_unlock(&wpt->wpt_lock);
			(void) pthread_cond_signal(&wpt->wpt_state_cv);
			break;
		}
		(void) pthread_mutex_unlock(&wpt->wpt_lock);
	}
	/*
	 * It shouldn't be possible to reach this point and find that
	 * we couldn't run the job. The assertions checks this is so.
	 */
	assert(wpt != NULL);
	assert(wp->wp_avail < wp->wp_size);
	assert(wp->wp_avail >= 0);
	(void) pthread_mutex_unlock(&wp->wp_lock);

	return (WP_SUCCESS);
}

int
wp_free(wp_t *wp, int wait)
{
	wp_thread_task_t *task;

	if (wp_close(wp, wait) == WP_FAIL)
		return (WP_FAIL);
	while ((task = wp_dequeue(wp))) {
		if (task->wtt_report)
			task->wtt_report(WP_FAIL, task->wtt_data);
		if (wp->wp_stats)
			wp->wp_stats->wps_failed++;
		free(task);
	}
	(void) wp_disable_stats(wp);
	(void) pthread_cond_destroy(&wp->wp_state_cv);
	(void) pthread_mutex_destroy(&wp->wp_lock);
	free(wp);
  return WP_SUCCESS;
}

uint32_t
wp_active(wp_t *wp)
{
	uint32_t ret;

	(void) pthread_mutex_lock(&wp->wp_lock);
	ret = wp->wp_size - wp->wp_avail;
	(void) pthread_mutex_unlock(&wp->wp_lock);

	return (ret);
}

uint32_t
wp_size(wp_t *wp)
{
	uint32_t ret;

	(void) pthread_mutex_lock(&wp->wp_lock);
	ret = wp->wp_size;
	(void) pthread_mutex_unlock(&wp->wp_lock);

	return (ret);
}

int
wp_state(wp_t *wp)
{
	int ret;

	(void) pthread_mutex_lock(&wp->wp_lock);
	ret = wp->wp_state;
	(void) pthread_mutex_unlock(&wp->wp_lock);

	return (ret);
}

int
wp_close(wp_t *wp, int wait)
{
	wp_thread_t *wpt, *wpt_tmp;

	if (wp_is_pool_thread(wp) == WP_TRUE && wait == WP_WAIT)
		return (WP_FAIL);
	(void) pthread_mutex_lock(&wp->wp_lock);
	if (wp->wp_state == WP_INACTIVE) {
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (WP_FAIL);
	}
	if (wait == WP_WAIT) {
		if (wp->wp_state != WP_ACTIVE) {
			(void) pthread_mutex_unlock(&wp->wp_lock);
			return (WP_FAIL);
		}
		while (wp->wp_avail != wp->wp_size || wp->wp_queue_h)
			(void) pthread_cond_wait(&wp->wp_state_cv,
			    &wp->wp_lock);
	}

	wp->wp_state = WP_INACTIVE;

	for (wpt = wp->wp_threads; wpt != NULL; wpt = wpt_tmp) {
		wpt_tmp = wpt->wpt_next;
		if (wp_rm_thread(wp) != WP_SUCCESS) {
			(void) pthread_mutex_unlock(&wp->wp_lock);
			return (WP_FAIL);
		}
	}
	(void) pthread_mutex_unlock(&wp->wp_lock);
	for (wpt = wp->wp_d_threads; wpt != NULL; wpt = wpt_tmp) {
		wpt_tmp = wpt->wpt_next;
		wp_join_thread(wpt);
	}
	wp->wp_d_threads = NULL;
	return (WP_SUCCESS);
}

int
wp_open(wp_t *wp, uint32_t size)
{
	(void) pthread_mutex_lock(&wp->wp_lock);
	if (wp->wp_state != WP_INACTIVE) {
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (WP_FAIL);
	}
	if (wp_init(wp, size) != WP_SUCCESS) {
		(void) pthread_mutex_unlock(&wp->wp_lock);
		return (WP_FAIL);
	}
	wp->wp_state = WP_ACTIVE;
	(void) pthread_mutex_unlock(&wp->wp_lock);
	return (WP_SUCCESS);
}

void
wp_wait(wp_t *wp)
{
	(void) pthread_mutex_lock(&wp->wp_lock);
	while (wp->wp_avail != wp->wp_size || wp->wp_queue_h)
		(void) pthread_cond_wait(&wp->wp_state_cv,
		    &wp->wp_lock);
	(void) pthread_mutex_unlock(&wp->wp_lock);
}

int
wp_enable_stats(wp_t *pool)
{
	(void) pthread_mutex_lock(&pool->wp_lock);
	if ((pool->wp_stats = malloc(sizeof (wp_stats_t))) == NULL) {
		(void) pthread_mutex_unlock(&pool->wp_lock);
		return (WP_FAIL);
	}
	(void) memset(pool->wp_stats, 0, sizeof (wp_stats_t));
	(void) pthread_mutex_unlock(&pool->wp_lock);
	return (WP_SUCCESS);
}

int
wp_disable_stats(wp_t *pool)
{
	(void) pthread_mutex_lock(&pool->wp_lock);
	free(pool->wp_stats);
	pool->wp_stats = NULL;
	(void) pthread_mutex_unlock(&pool->wp_lock);
	return (WP_SUCCESS);
}

int
wp_get_stats(wp_t *pool, wp_stats_t *stats)
{
	(void) pthread_mutex_lock(&pool->wp_lock);
	if (pool->wp_stats == NULL) {
		(void) pthread_mutex_unlock(&pool->wp_lock);
		return (WP_FAIL);
	}
	(void) memcpy(stats, pool->wp_stats, sizeof (wp_stats_t));
	(void) pthread_mutex_unlock(&pool->wp_lock);
	return (WP_SUCCESS);
}
