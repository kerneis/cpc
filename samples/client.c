#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <pthread.h>

#define BUFFER_SIZE 4096
#define US * 1000 * 1000

#define NUMSAMPLES 1000

struct sample {
    int code;
    struct timeval b1, b2, b3;
    struct timeval end;
};

struct samples {
    int n;
    struct sample samples[NUMSAMPLES];
    struct samples *next;
};
        

static int
numthreads = 1, numrequests = 100, requests = 0, threads = 0, errors = 0, slice;
struct samples *global_samples;
pthread_mutex_t requests_lock = PTHREAD_MUTEX_INITIALIZER,
    threads_lock = PTHREAD_MUTEX_INITIALIZER,
    samples_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_barrier_t start;
    
pthread_cond_t done = PTHREAD_COND_INITIALIZER;
static char request[BUFFER_SIZE];
static int request_len;

static int debug = 0;

struct sockaddr_in addr;
int addrlen;

static void *client(void *arg);
static void display_samples(struct samples *samples);

int
main(int argc, char **argv)
{
    pthread_t thread;
    pthread_attr_t attr;
    int i, rc, port;
    char *url, *slash, *colon, *server, *path;
    struct hostent *host;

    while(1) {
        int c;
        char *end;
        c = getopt(argc, argv, "n:c:d");
        if(c < 0) break;
        switch(c) {
        case 'n':
            numrequests = strtol(optarg, &end, 0);
            if(*end) goto usage;
            break;
        case 'c':
            numthreads = strtol(optarg, &end, 0);
            if(*end) goto usage;
            break;
        case 'd':
            debug = 1;
            break;
        default:
            goto usage;
        }
    }

    if(optind >= argc)
        goto usage;

    url = argv[optind]; optind++;

    if(optind != argc)
        goto usage;

    if(strlen(url) > 500)
        goto usage;

    if(strlen(url) > 7 && memcmp(url, "http://", 7) == 0)
        url += 7;

    slash = strchr(url, '/');
    colon = strchr(url, ':');
    if(colon && slash && colon < slash) {
        char s[500];
        char p[500];
        char *end;
        memcpy(s, url, colon - url);
        s[colon - url] = '\0';
        memcpy(p, colon + 1, slash - colon - 1);
        p[slash - colon - 1] = '\0';
        server = strdup(s);
        port = strtol(p, &end, 0);
        if(*end != '\0')
            goto usage;
        path = strdup(slash);
    } else if(slash) {
        char s[500];
        memcpy(s, url, slash - url);
        s[slash - url] = '\0';
        server = strdup(s);
        port = 80;
        path = strdup(slash);
    } else if(colon) {
        char s[500];
        char *end;
        memcpy(s, url, colon - url);
        s[colon - url] = '\0';
        server = strdup(s);
        port = strtol(colon + 1, &end, 0);
        if(*end != '\0')
            goto usage;
        path = strdup("/");
    } else {
        server = strdup(url);
        port = 80;
        path = strdup("/");
    }

    slice = numrequests / numthreads / 100;
    if(slice < 1)
        slice = 1;
    if(slice < 10)
        fprintf(stderr, "Warning: tiny slice (%d).\n", slice);

    signal(SIGPIPE, SIG_IGN);

    host = gethostbyname(server);
    if(host == NULL) {
        herror(server);
        exit(1);
    }

    request_len = snprintf(request, BUFFER_SIZE,
                           "GET %s HTTP/1.1\r\n"
                           "Host: %s:%d\r\n"
                           "Connection: close\r\n"
                           "User-Agent: jch-client/0.1\r\n"
                           "\r\n",
                           path,
                           server, port);

    if(request_len < 0 || request_len >= BUFFER_SIZE)
        goto usage;

    if(host->h_length < 1) {
        fprintf(stderr, "%s: no address.\n", server);
        exit(1);
    }

    memset(&addr, 0, sizeof(addr));
    memcpy(&addr.sin_addr, host->h_addr, host->h_length);
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addrlen = sizeof(struct sockaddr_in);

    global_samples = NULL;

    rc = pthread_mutex_lock(&threads_lock);
    if(rc != 0) {
        perror("mutex lock");
        exit(1);
    }

    rc = pthread_barrier_init(&start, NULL, numthreads);
    if(rc) {
        perror("barrier init");
        exit(1);
    }

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&attr, 64 * 1024);

    for(i = 0; i < numthreads; i++) {
        rc = pthread_create(&thread, &attr, client, NULL);
        if(rc != 0) {
            perror("st_thread_create");
            exit(1);
        }
        threads++;
    }

    rc = pthread_cond_wait(&done, &threads_lock);
    if(rc != 0) {
        perror("cond_wait(done)");
        exit(1);
    }
    rc = pthread_mutex_unlock(&threads_lock);
    if(rc != 0) {
        perror("mutex unlock");
        exit(1);
    }

    printf("%d requests, %d errors.\n", requests, errors);
    display_samples(global_samples);
    return 0;

 usage:
    fprintf(stderr, "%s [-n numrequests] [-c concurrency] [-d] url\n",
            argv[0]);
    exit(1);
}

static int
timeval_minus(const struct timeval *t1, const struct timeval *t2)
{
    if(t1->tv_sec - t2->tv_sec > 2000)
        abort();
    else if(t1->tv_sec - t2->tv_sec < -2000)
        abort();

    return (t1->tv_sec - t2->tv_sec) * 1000000 +
        (t1->tv_usec - t2->tv_usec);
}

static void
display_samples(struct samples *samples)
{
    int i;

    if(samples == NULL)
        return;

    display_samples(samples->next);

    for(i = 0; i < samples->n; i++)
        printf("%d\t%d%06d\t%d\t%d\t%d\n",
               samples->samples[i].code,
               (int)samples->samples[i].b1.tv_sec,
               (int)samples->samples[i].b1.tv_usec,
               timeval_minus(&samples->samples[i].b2, &samples->samples[i].b1),
               timeval_minus(&samples->samples[i].b3, &samples->samples[i].b1),
               timeval_minus(&samples->samples[i].end,
                             &samples->samples[i].b1));
}

static void
accumulate_samples(struct samples *samples)
{
    int rc;
    rc = pthread_mutex_lock(&samples_lock);
    if(rc) abort();
    samples->next = global_samples;
    global_samples = samples;
    rc = pthread_mutex_unlock(&samples_lock);
    if(rc) abort();
}

static void *
client(void *arg)
{
    int fd, rc, i;
    char buf[BUFFER_SIZE];
    int len, eof;
    int one = 1;
    char *message = "???";
    int myrequests = 0;
    struct samples *samples = NULL;

    samples = malloc(sizeof(struct samples));
    if(samples == NULL)
        abort();
    samples->n = 0;

    pthread_barrier_wait(&start);

    while(1) {
        char *space;

        if(myrequests <= 0) {
            rc = pthread_mutex_lock(&requests_lock);
            if(rc) abort();

            if(requests < numrequests) {
                int n;
                if(requests + numthreads * slice <= numrequests)
                    n = slice;
                else if(requests + 2 * numthreads <= numrequests)
                    n = (numrequests - requests) / numthreads / 2;
                else
                    n = 1;

                requests += n;
                myrequests += n;
            }
            rc = pthread_mutex_unlock(&requests_lock);
            if(rc) abort();
        }

        if(myrequests <= 0)
            break;
        else
            myrequests--;

        if(samples->n >= NUMSAMPLES) {
            accumulate_samples(samples);

            samples = malloc(sizeof(struct samples));
            if(samples == NULL)
                abort();
            samples->n = 0;
        }

        memset(&samples->samples[samples->n], 0, sizeof(struct sample));
        samples->samples[samples->n].code = -1;
        gettimeofday(&samples->samples[samples->n].b1, NULL);

        fd = socket(PF_INET, SOCK_STREAM, 0);
        if(fd < 0) {
            if(debug)
                perror("socket");
            goto error_noclose;
        }

        rc = connect(fd, (struct sockaddr*)&addr, addrlen);
        if(rc < 0) {
            message = "connect";
            goto error;
        }

        rc = setsockopt(fd, SOL_TCP, TCP_NODELAY, (char*)&one, sizeof(one));
        if(rc < 0) {
            message = "nodelay";
            goto error;
        }

        rc = write(fd, request, request_len);
        if(rc < request_len) {
            message = "write";
            goto error;
        }
        gettimeofday(&samples->samples[samples->n].b2, NULL);

        eof = 0; len = 0;

        while(1) {
            rc = read(fd, buf, BUFFER_SIZE);
            if(rc < 0) {
                message = "read";
                goto error;
            }
            if(rc > 0 && len == 0)
                gettimeofday(&samples->samples[samples->n].b3, NULL);
            len += rc;
            if(rc == 0)
                eof = 1;
                
            for(i = 0; i < len - 3; i++) {
                if(buf[i] == '\r' && buf[i + 1] == '\n' &&
                   buf[i + 2] == '\r' && buf[i + 3] == '\n')
                    goto out;
            }
            if(eof) {
                message="read";
                goto error;
            }
        }


    out:
        if(len < 6) {
            message = "read";
            errno = 0;
            goto error;
        }

        if(memcmp(buf, "HTTP/1", 6) != 0) {
            message = "read";
            errno = 0;
            goto error;
        }

        space = memchr(buf, ' ', len);
        if(space == NULL || len - (space - buf) < 3 ||
           (space[1] < '0' && space[1] >= '9')) {
            message = "server";
            errno = 0;
            goto error;
        }

        samples->samples[samples->n].code = atoi(space + 1);

        while(!eof) {
            rc = read(fd, buf, BUFFER_SIZE);
            if(rc < 0) {
                message = "read";
                goto error;
            }
            if(rc == 0)
                eof = 1;
        }

        rc = close(fd);
        if(rc < 0) {
            message = "close";
            goto error;
        }

        gettimeofday(&samples->samples[samples->n].end, NULL);
        samples->n++;

        continue;

    error:
        if(debug) {
            if(errno)
                perror(message);
            else
                fprintf(stderr, "%s failed.\n", message);
        }
        close(fd);
    error_noclose:
        errors++;
        gettimeofday(&samples->samples[samples->n].end, NULL);
        samples->n++;
        continue;
    }

    accumulate_samples(samples);

    rc = pthread_mutex_lock(&threads_lock);
    if(rc) abort();
    threads--;
    if(threads <= 0)
        pthread_cond_signal(&done);
    rc = pthread_mutex_unlock(&threads_lock);
    if(rc) abort();

    return NULL;
}
