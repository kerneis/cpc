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
#include <errno.h>
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
#ifdef __unix__
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/select.h>

typedef struct cpc_handle {
    int cpch_handle;
    int cpch_kind;
} cpc_handle, *cpc_handle_t;

static inline DWORD get_last_error(cpc_handle_t *h)
{
    return errno;
}

/*===============================   Windows   ================================*/
#elif defined _WIN32
#define _WIN32_WINNT 0x0600

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <winsock2.h>
#include <Mswsock.h>
#include <windows.h>

#ifndef FD_COPY
#define FD_COPY(src, dest) do { *dest = *src; } while(0)
#endif

#ifndef socklen_t
typedef int socklen_t;
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

static inline void print_errno(char *str, long errno)
{
    fprintf(stderr, "%s: %ld\n", str, errno);
}

static inline void print_error(char *str)
{
    fprintf(stderr, "%s: %ld\n", str, (long)GetLastError());
}

static inline void WSA_print_error(char *str)
{
    fprintf(stderr, "%s: %ld\n", str, (long)WSAGetLastError());
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

#endif /* _COMPATIBILITY_H */
