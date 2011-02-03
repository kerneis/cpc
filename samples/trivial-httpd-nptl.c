#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <signal.h>

#ifndef SOL_TCP
# define SOL_TCP IPPROTO_TCP
#endif

void *accept_connection(void *fd);
void *handle_connection(void *fd2);

static struct sockaddr_in addr;
static socklen_t len;

char *root = "/usr/share/polipo/www/";

#ifdef STATIC
int port = 7668;
static char resp[] = "HTTP/1.0 200 OK\r\n"
                    "Content-type: text/html\r\n"
                    "Server: Trivial-nptl\r\n"
                    "Connection: close\r\n\r\n"
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n"
  " \"http://www.w3.org/TR/html4/loose.dtd\">\n"
  "<html><head>\n"
  "<title>Welcome to Polipo</title>\n"
  "</head><body>\n"
  "<h1>Welcome to Polipo</h1>\n\n"
  "<p><a href=\"doc/\">The Polipo manual</a>.\n\n"
  "<p><a href=\"polipo/\">The configuration interface</a>.\n"
  "</body></html>\n";
static int resp_size = sizeof(resp);
#else
int port = 8668;
#endif

#undef htons

int
main(int argc, char *argv[])
{
    int fd, rc, threads_count;
    int one = 1;

    pthread_t thread;
    pthread_attr_t attr;

    /* Initialize and set thread detached attribute */
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&attr, 64 * 1024);

    if(argc < 2)
      threads_count = 1;
    else
      threads_count = atoi(argv[1]);
    
    signal(SIGPIPE, SIG_IGN);

    fd = socket(PF_INET, SOCK_STREAM, 0);
    if(fd < 0) { perror("socket"); exit(1); }

    rc = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one));
    if(rc < 0) perror("setsockopt(SO_REUSEADDR)");

    memset(&addr, 0, sizeof(addr));

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    rc = bind(fd, (struct sockaddr*)&addr, sizeof(addr));
    if(rc < 0) { perror("bind"); exit(1); }

    rc = listen(fd, 1024);
    if(rc < 0) { perror("listen"); exit(1); }

    while(threads_count--) {
      rc = pthread_create(&thread, &attr, accept_connection, (void *)fd);
      if (rc){ perror("pthread_create"); exit(1); }
    }

    pthread_exit(NULL); // Don't kill the other threads on exit
}

void *
accept_connection(void *fd)
{
    pthread_t thread;
    pthread_attr_t attr;

    /* Initialize and set thread detached attribute */
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&attr, 64 * 1024);
    
    while(1) {
        int rc, fd2;
        len = sizeof(addr);
        fd2 = accept((int)fd, (struct sockaddr*)&addr, &len);
        if(fd2 < 0) {
            if(errno == EINTR) {
              continue;
            } else {
                perror("accept");
                sched_yield();
                continue;
            }
        }

        rc = pthread_create(&thread, &attr, handle_connection, (void *)fd2);

        if (rc){
            perror("pthread_create");
            exit(1);
        }
    }

    /* Dead code */
    pthread_attr_destroy(&attr);
    return NULL;
}

void *
handle_connection(void *fd2)
{
    int rc, n, ffd, len;
    char *buf, *fn;
    int i, val, fd;

    fd = (int)fd2;
    val = 1;
    rc = setsockopt(fd, SOL_TCP, TCP_NODELAY, (char *)&val, sizeof(val));
    if(rc < 0)
        goto fail;

    buf = malloc(4096);
    rc = 0;

 again:
    rc += read(fd, buf + rc, 4096 - rc);
    if(rc < 0) { perror("read"); goto fail; }

    if(rc < 4)
        goto fail;

    if(memcmp(buf, "GET ", 4) != 0)
        goto fail;

    for(i = 5; i < rc; i++)
        if(buf[i] == ' ' || buf[i] == '\r' || buf[i] == '\n')
            break;
    if(i == rc && rc < 4096)
        goto again;

    len = strlen(root);

    fn = malloc(len + 1 + i - 5 + 12);
    strcpy(fn, root);
    memcpy(fn + len, buf + 5, i - 5);

    if(buf[i - 1] == '/')
        strcpy(fn + len + i - 5, "index.html");
    else
        fn[len + i - 5] = '\0';

    i--;

 search:
    while(i < rc - 3)
      if(buf[i++] == '\r' && buf[i] == '\n'){
        i++; if(buf[i++] == '\r' && buf[i] == '\n')
          goto send;
      }

    if(rc < 4096) {
        rc += read(fd, buf + rc, 4096 - rc);
        goto search;
    }

 send:
#ifdef STATIC
    rc = write(fd, &resp, resp_size);
    if(rc != resp_size) { perror("st_write"); goto fail; }
#else
    ffd = open(fn, O_RDONLY);
    if(ffd < 0) {
        int err;
        char *message;
        if(errno == ENOENT) {
            err = 404;
            message = "File doesn't exist";
        } else if(errno == EACCES || errno == EPERM) {
            err = 403;
            message = "Forbidden";
        } else if(errno == EMFILE || errno == ENFILE) {
            err = 500;
            message = "Out of file descriptors";
        } else if(errno == ENOMEM) {
            err = 500;
            message = "Out of memory";
        } else {
            err = 500;
            message = "Unknown error";
        }
        n = snprintf(buf, 4096,
                     "HTTP/1.1 %d %s\r\n"
                     "Content-Type: text/html\r\n"
                     "Server: Trivial-nptl\r\n"
                     "Connection: close\r\n"
                     "\r\n"
                     "<html><body><p>Couldn't open %s: %s</body></html>\r\n",
                     err, message, fn, message);
        free(fn);
    } else {
        free(fn);
        n = snprintf(buf, 4096,
                     "HTTP/1.1 200 OK\r\n"
                     "Content-Type: text/html\r\n"
                     "Server: Trivial-nptl\r\n"
                     "Connection: close\r\n"
                     "\r\n");
        rc = read(ffd, buf + n, 4096 - n);
        if(rc >= 0)
            n += rc;
    }

    rc = write(fd, buf, n);
    if(rc < 0) { perror("write"); if(ffd >= 0) close(ffd); goto fail; }

    if(ffd >= 0) {
        while(1) {
            n = read(ffd, buf, 4096);
            if(n <= 0) break;
            rc = write(fd, buf, 4096);
        }
    }

    close(ffd);
#endif

 fail:
    close(fd);
    free(buf);
    pthread_exit(NULL);
}
