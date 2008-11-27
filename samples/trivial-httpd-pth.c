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
#include <pth.h>
#include <signal.h>

void accept_connection(int fd);
void *handle_connection(void *fd2);

static struct sockaddr_in addr;
static socklen_t len;

char *root = "/usr/share/polipo/www/";

#ifdef STATIC
int port = 7670;
static char resp[] = "HTTP/1.0 200 OK\r\n"
                    "Content-type: text/html\r\n"
                    "Server: Trivial-pth\r\n"
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
int port = 8670;
#endif

#undef htons

int
main()
{
    int fd, rc;
    int one = 1;
    
    pth_init();
    
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

    accept_connection(fd);

    return 0;
}

void
accept_connection(int fd)
{
    pth_t thread;
    pth_attr_t attr;

    /* Initialize and set thread detached attribute */
    attr = pth_attr_new();
    pth_attr_set(attr,PTH_ATTR_JOINABLE, FALSE);
    
    while(1) {
        int fd2;
        len = sizeof(addr);
        fd2 = pth_accept(fd, (struct sockaddr*)&addr, &len);
        if(fd2 < 0) {
            if(errno == EINTR) {
              continue;
            } else {
                perror("pth_accept");
                pth_yield(NULL);
                continue;
            }
        }

        thread = pth_spawn(attr, handle_connection, (void *)fd2);

        if (thread == NULL){
            perror("pth_spawn");
            exit(1);
        }
    }

    /* Dead code */
    //pth_attr_destroy(attr);
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
    rc += pth_read(fd, buf + rc, 4096 - rc);
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
        rc += pth_read(fd, buf + rc, 4096 - rc);
        goto search;
    }

 send:
#ifdef STATIC
    rc = pth_write(fd, &resp, resp_size);
    if(rc != resp_size) { perror("pth_write"); goto fail; }
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
                     "Server: Trivial-pth\r\n"
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
                     "Server: Trivial-pth\r\n"
                     "Connection: close\r\n"
                     "\r\n");
        rc = read(ffd, buf + n, 4096 - n);
        if(rc >= 0)
            n += rc;
    }

    rc = pth_write(fd, buf, n);
    if(rc < 0) { perror("write"); if(ffd >= 0) close(ffd); goto fail; }

    if(ffd >= 0) {
        while(1) {
            n = read(ffd, buf, 4096);
            if(n <= 0) break;
            rc = pth_write(fd, buf, 4096);
            pth_yield(NULL);
        }
    }

    close(ffd);
#endif

 fail:
    close(fd);
    free(buf);
    return NULL;
}
