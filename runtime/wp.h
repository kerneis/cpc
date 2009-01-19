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

#ifndef _WP_H
#define _WP_H

#include <sys/types.h>
#include <inttypes.h>

/*
 * Return Codes
 */

/**
 * Success.
 */
#define	WP_SUCCESS	0

/**
 * Failure.
 */
#define	WP_FAIL		-1

/*
 * Truth Codes
 */

/**
 * True.
 */
#define	WP_TRUE		1

/**
 * False.
 */
#define	WP_FALSE       	0

/*
 * Pool state
 */

/**
 * A worker pool is inactive.
 */
#define	WP_INACTIVE	0x0

/**
 * A worker pool is inactive.
 */
#define	WP_ACTIVE	0x1

/*
 * Wait flags (for use with wp_free()).
 */

/**
 * Immediately terminate the pool without waiting for the pool to
 * quiesce.
 */
#define	WP_IMMEDIATE	0x0

/**
 * Terminate the pool after waiting for the pool to quiesce.
 */
#define	WP_WAIT		0x1

/**
 * Represents a pool of worker threads.
 */
typedef struct wp wp_t;

/**
 * A function pointer for a worker pool task..
 */
typedef void *(*wp_process_cb)(void *);

/**
 * A function pointer for a worker pool report.
 */
typedef void (*wp_report_cb)(int status, void *);

/**
 * A worker pool statistic snapshot.
 */
typedef struct
{
	/**
	 * Failed tasks
	 */
	uint64_t wps_failed;
	/**
	 * Succeeded tasks
	 */
	uint64_t wps_succeeded;
	/**
	 * Average task duration in nanoseconds
	 */
	int64_t wps_avg_duration;
} wp_stats_t;

/*
 * Log utility functions
 */

/**
 * Write a printf-formatted message to stderr and terminate the
 * application.
 *
 * @param format printf-like format string.
 */
extern void wp_die(const char *format, ...);

/**
 * Write a printf-formatted message to stderr.
 *
 * @param format printf-like format string.
 */
extern void wp_log(const char *format, ...);

/*
 * Worker pool functions
 */

/**
 * Create a new pool of worker threads.
 *
 * If the size is set to zero, then the default worker pool size is
 * used. If the report function is NULL, then no result reporting is
 * performed.
 *
 * @param size The number of threads in the pool.
 * @param process The function which the worker pool threads will execute.
 * @param report The function used to report execution results.
 */
extern wp_t *wp_new(uint32_t size, wp_process_cb process,
    wp_report_cb report);

/**
 * Resize a pool of worker threads.
 *
 * If the size is set to zero, then the default worker pool size is
 * used.
 *
 * @param pool The pool to resize.
 * @param size The number of threads in the pool.
 */
extern int wp_resize(wp_t *pool, uint32_t size);

/**
 * Destroy the worker pool.
 *
 * If the pool is still active, then it is first closed (see
 * wp_close()). Then the pool resources are reclaimed.
 *
 * @param pool The pool to destroy.
 * @param wait Wait for the pool to empty before destroying?
 */
extern int wp_free(wp_t *pool, int wait);

/**
 * Close the worker pool.
 *
 * If the pool is still open, then it is closed. The wait parameter is
 * used to indicate whether to wait for outstanding tasks to finish
 * before closing the pool. Once a worker pool has been closed, it can
 * be re-opened with the wp_open() function.
 *
 * @param pool The pool to close.
 * @param wait Wait for the pool's outstanding tasks to complete
 * before closing?
 */
extern int wp_close(wp_t *pool, int wait);

/**
 * Run a task in the worker pool.
 *
 * Run a task in the worker pool. The supplied data is passed to the
 * thread which executes the task. Returns 0 if the task is
 * successfully initiated, -1 on failure.
 *
 * @param pool The pool to execute the task in.
 * @param data The data to pass to the worker.
 */
extern int wp_run(wp_t *pool, void *data);

/**
 * Run a (specific) task in the worker pool.
 *
 * Run a task in the worker pool. The supplied data is passed to the
 * thread which executes the task. Returns 0 if the task is
 * successfully initiated, -1 on failure.
 *
 * @param pool The pool to execute the task in.
 * @param data The data to pass to the worker.
 * @param process The function which the worker pool thread will execute.
 * @param report The function used to report execution results.
 */
extern int wp_run_task(wp_t *pool, void *data, wp_process_cb process,
    wp_report_cb report);

/**
 * Return a count of the number of active threads in the worker pool.
 *
 * This function may be used by a worker thread to determine whether
 * it's safe to stop the pool. Adding tasks to the pool (using
 * wp_run()) at the same time as checking for the number of tasks in
 * the pool requires synchronization.
 *
 * @param pool The pool to check for available workers.
 */
extern uint32_t wp_active(wp_t *pool);

/**
 * Return a count of the number of threads in the worker pool.
 *
 * This function may be used by a worker thread to check the
 * size of the pool.
 *
 * @param pool The pool for which the size is required.
 */
extern uint32_t wp_size(wp_t *pool);

/**
 * Return the state of the worker pool.
 *
 * The pool will either be active or inactive. This is identifed by
 * the return code of WP_ACTIVE or WP_INACTIVE.
 *
 * @param pool The pool to execute the task in.
 */
extern int wp_state(wp_t *pool);

/**
 * Wait for the worker pool to quiesce.
 *
 * This function will block until the worker pool has no outstanding
 * tasks. It's still possible to add tasks to the pool whilst this
 * function is active, so external co-ordination is required to ensure
 * that when this function returns no more jobs have been added to the
 * pool.
 *
 * @param pool The pool to wait for.
 */
extern void wp_wait(wp_t *pool);

/**
 * Open a closed worker pool.
 *
 * If a worker pool has been previously closed, then this function can
 * be used to re-open it. Since the worker pool threads terminated
 * when the pool was closed, this function specified the number of
 * threads to re-open the pool with.
 *
 * @param pool The pool to re-open.
 * @param size The size of the pool.
 */
extern int wp_open(wp_t *pool, uint32_t size);

/**
 * Enable worker pool reporting.
 *
 * The worker pool will now collect performance statistics. They may be
 * accessed using wp_get_stats().
 *
 * @param pool The pool to start monitoring.
 */
extern int wp_enable_stats(wp_t *pool);

/**
 * Disable worker pool reporting.
 *
 * The worker pool will now stop collecting performance statistics.
 *
 * @param pool The pool to stop monitoring.
 */
extern int wp_disable_stats(wp_t *pool);

/**
 * Access worker pool statistics.
 *
 * Retrieve worker pool statistics.
 *
 * @param pool The pool to retrieve statistics.
 * @param stat The statistics.
 */
extern int wp_get_stats(wp_t *pool, wp_stats_t *stat);

#endif	/* _WP_H */
