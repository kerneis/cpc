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
#include <st.h>
#include <signal.h>

void accept_connection(int fd);
void *handle_connection(void *fd2);

static struct sockaddr_in addr;

char *root = "/usr/share/polipo/www/";

#ifdef STATIC
int port = 7669;
static char resp[] = "HTTP/1.0 200 OK\r\n"
                    "Content-type: text/html\r\n"
                    "Server: Trivial-st\r\n"
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
int port = 8669;
#endif

#undef htons

int
main()
{
    int fd, rc;
    int one = 1;

    //signal(SIGPIPE, SIG_IGN); also done by st_init()

    st_init();

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
    struct sockaddr_in from;
    st_netfd_t st_fd,fd2;
    int fromlen = sizeof(from);
    
    if((st_fd = st_netfd_open_socket(fd)) == NULL)
      {perror("st_netfd_open_socket") ; exit(1);}
    
    while(1) {
        fd2 = st_accept(st_fd,(struct sockaddr *)&from, &fromlen, -1);
        if(fd2 == NULL) {
            if(errno == EINTR) {
              continue;
            } else {
                perror("accept");
                st_sleep(0);
                continue;
            }
        }

        if(st_thread_create(handle_connection, (void *)fd2,0,0) == NULL){
            perror("st_thread_create");
            exit(1);
        }
    }
}

void *
handle_connection(void *fd2)
{
    int rc, n, len;
    int ffd;
    char *buf, *fn;
    int i;

    st_netfd_t fd = (st_netfd_t)fd2;

    buf = malloc(4096);
    rc = 0;

 again:
    rc += st_read(fd, buf + rc, 4096 - rc,-1);
    if(rc < 0) { perror("st_read"); goto fail; }

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
        rc += st_read(fd, buf + rc, 4096 - rc,-1);
        goto search;
    }

 send:
#ifdef STATIC
    rc = st_write(fd, &resp, resp_size, 60000000);
    if(rc != resp_size) { perror("st_write"); goto fail; }
#else
    ffd = open(fn, O_RDONLY,0);
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
                     "Server: Trivial-st\r\n"
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
                     "Server: Trivial-st\r\n"
                     "Connection: close\r\n"
                     "\r\n");
        rc = read(ffd, buf + n, 4096 - n);
        if(rc >= 0)
            n += rc;
    }

    rc = st_write(fd, buf, n,-1);
    if(rc < 0) { perror("write"); if(ffd >= 0) close(ffd); goto fail; }

    if(ffd >= 0) {
        while(1) {
            n = read(ffd, buf, 4096);
            if(n <= 0) break;
            rc = st_write(fd, buf, 4096,-1);
            st_sleep(0);
        }
    }

    close(ffd);
#endif

 fail:
    st_netfd_close(fd);
    free(buf);
    st_thread_exit(NULL);
}
