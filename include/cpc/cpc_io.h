/*
Copyright (c) 2005-2007 by Juliusz Chroboczek.

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

#ifndef CPC_IO_H
#define CPC_IO_H
#include "compatibility.h"

typedef struct cpc_timeout cpc_timeout;

/* Timeouts */
cps cpc_timeout *cpc_timeout_get(int secs, int usecs);
cps void cpc_timeout_restart(cpc_timeout *timeout);
cpc_condvar *cpc_timeout_condvar(cpc_timeout *timeout);
int cpc_timeout_expired(cpc_timeout *timeout);
void cpc_timeout_destroy(cpc_timeout *timeout);

/* IO */
cpc_handle_t cpc_open_file_std(const char *path, int openflags);
cpc_handle_t cpc_open_socket_std(int no_nagle);
int cpc_setup_descriptor(HANDLE h, int nonagle);
cps int64_t cpc_write(cpc_handle_t handle, void *buf, size_t count);
cps int64_t cpc_write_cond(cpc_handle_t handle, void *buf, size_t count,
                           cpc_condvar *cond);
cps int64_t cpc_write_timeout(cpc_handle_t h, void *buf, size_t count,
                              int secs, int micros);
cps int64_t cpc_my_read(cpc_handle_t handle, char *buf, uint64_t count);
cps int64_t cpc_read(cpc_handle_t h, void *buf, size_t count);
cps int64_t cpc_read_cond(cpc_handle_t h, void *buf, size_t count,
                          cpc_condvar *cond);
cps int64_t cpc_read_timeout(cpc_handle_t h, void *buf, size_t count,
                             int secs, int micros);
cps int64_t cpc_send_cond(cpc_handle_t handle, void *buf, size_t count,
                          cpc_condvar *cond);
cps int64_t cpc_send(cpc_handle_t handle, void *buf, size_t count);
cps int64_t cpc_recv_cond(cpc_handle_t handle, void *buf, size_t count,
                          cpc_condvar *cond);
cps int64_t cpc_recv(cpc_handle_t handle, void *buf, size_t count);
cps int64_t cpc_accept(cpc_handle_t serverHandle, cpc_handle_t acceptedSocket,
                       void *buf, size_t count);

#endif /* CPC_IO_H */
