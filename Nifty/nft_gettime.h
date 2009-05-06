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
 * File:  nft_gettime.h
 *
 * PURPOSE
 *
 * This file defines the gettime() call, which provides the current time
 * using either ftime(), gettimeofday() or clock_gettime(), depending on
 * your preference and what your operating system supports.
 *
 * To choose the time source, define one of these compilation symbols:
 *
 *	USE_FTIME		.. for ftime()
 *	USE_GETTIMEOFDAY	.. for gettimeofday()
 *	USE_CLOCKGETTIME	.. for clock_gettime()
 *
 * By default, ftime() is used. You can change the default by editing
 * the commented #define's below.
 *
 *****************************************************************************
 */
#ifndef nft_gettime_h
#define nft_gettime_h

#include <time.h>

#if !defined(USE_FTIME) && !defined(USE_GETTIMEOFDAY) && !defined(USE_CLOCKGETTIME)
/*
 * Select the default time source by uncommenting one of these defines:
 */
#define USE_FTIME        1
// #define USE_GETTIMEOFDAY 1
// #define USE_CLOCKGETTIME 1
#endif


#ifdef    USE_FTIME
#include <sys/types.h>
#include <sys/timeb.h>
#ifndef WIN32
#include <sys/time.h>
#else
struct timespec
{
	time_t tv_sec;
	long   tv_nsec;
};
#endif /* WIN32 */
#endif /* USE_FTIME */


#ifdef    USE_GETTIMEOFDAY
#include <sys/time.h>
#endif /* USE_GETTIMEOFDAY */


#ifndef NANOSEC
#define NANOSEC         1000000000
#endif

static int
gettime(struct timespec *ts)
{
    int rc = 0;

#if defined(USE_FTIME)
    struct timeb tb;
    ftime( &tb );
    ts->tv_sec  = tb.time;
    ts->tv_nsec = tb.millitm * 1000000;	/* milli to nano secs  */

#elif defined(USE_GETTIMEOFDAY)
    struct timeval  tv;
    rc = gettimeofday(&tv, NULL);
    ts->tv_sec  = tv.tv_sec;
    ts->tv_nsec = tv.tv_usec * 1000;	/* micro to nano secs  */

#elif defined(USE_CLOCKGETTIME)
    rc = clock_gettime(CLOCK_REALTIME, ts);

#endif
    return rc;
}

#endif /* nft_gettime_h */
