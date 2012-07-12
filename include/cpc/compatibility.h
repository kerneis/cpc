/*
Copyright (c) 2012 by Matthieu Boutier.

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

#ifndef _COMPATIBILITY_H
#define _COMPATIBILITY_H

/*============================   Common headers   =========================== */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <assert.h>
#include <pthread.h>
#include <sys/time.h>
#include <stddef.h>


#ifndef SOL_TCP
# define SOL_TCP IPPROTO_TCP
#endif

enum cpc_handle_kind {
    CPC_HANDLE_SOCKET,
    CPC_HANDLE_FILE,
};


/*=================================   Unix   =================================*/
#if defined(__unix__) || defined(__unix) || defined(unix) ||    \
    defined(__APPLE__)
#ifndef(__unix__)
#define __unix__
#endif
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/uio.h>

typedef struct cpc_handle {
    int cpch_handle;
    int cpch_kind;
} cpc_handle, *cpc_handle_t;

/* cpc_iobuf */
typedef struct iovec cpc_iobuf;
static inline void cpc_iobuf_set(cpc_iobuf *iobuf, void* buf, size_t len)
{
    iobuf->iov_base = buf;
    iobuf->iov_len  = len;
}
static inline void *cpc_iobuf_getbuf(cpc_iobuf *iobuf)
{
    return iobuf->iov_base;
}
static inline size_t cpc_iobuf_getlen(cpc_iobuf *iobuf)
{
    return iobuf->iov_len;
}

/*===============================   Windows   ================================*/
#elif defined _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#ifndef WINVER
#define WINVER 0x0601
#endif


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <winsock2.h>
#include <Mswsock.h>
#include <windows.h>
#include <ws2tcpip.h>

#ifndef FD_COPY
#define FD_COPY(src, dest) do { *dest = *src; } while(0)
#endif

#ifndef socklen_t
typedef int socklen_t;
#endif

#ifndef IPV6_V6ONLY
#define IPV6_V6ONLY 27
#endif

/* WARNING: internally, cpc_handle can differ due to his kind. */
typedef struct cpc_handle {
    HANDLE   cpch_handle;
    int      cpch_kind;
} cpc_handle, *cpc_handle_t;

typedef struct cpc_handle_file {
    HANDLE   cpch_handle;
    int      cpch_kind;
    uint64_t cpch_offset;
} cpc_handle_file, *cpc_handle_file_t;

static inline void print_errno(char *str, long error_code)
{
    fprintf(stderr, "%s: %ld\n", str, error_code);
}

static inline void print_error(char *str)
{
    fprintf(stderr, "%s: %ld\n", str, (long)GetLastError());
}

static inline void WSA_print_error(char *str)
{
    fprintf(stderr, "%s: %ld\n", str, (long)WSAGetLastError());
}

/* cpc_iobuf */
typedef WSABUF cpc_iobuf;
static inline void cpc_iobuf_set(cpc_iobuf *iobuf, char FAR* buf, u_long len)
{
    iobuf->buf = buf;
    iobuf->len = len;
}
static inline void *cpc_iobuf_getbuf(cpc_iobuf *iobuf)
{
    return iobuf->buf;
}
static inline u_long cpc_iobuf_getlen(cpc_iobuf *iobuf)
{
    return iobuf->len;
}


/* MinGw doesn't have these headers. */
#ifndef SetFileCompletionNotificationModes
BOOL WINAPI SetFileCompletionNotificationModes(HANDLE FileHandle, UCHAR Flags);
#define FILE_SKIP_COMPLETION_PORT_ON_SUCCESS 0x1
#endif
#ifndef CancelIoEx
BOOL WINAPI CancelIoEx(HANDLE hFile, LPOVERLAPPED lpOverlapped);
#endif

#else
#error "Unknown Operating System."
#endif

#ifndef NO_CPS_PROTO
#pragma cpc_no_retain("cpc_iobuf_set", "cpc_iobuf_getbuf", "cpc_iobuf_getlen")
#endif

#endif /* _COMPATIBILITY_H */
