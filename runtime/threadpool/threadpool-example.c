#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>
#include "threadpool.h"

threadpool_t *threadpool;

void
main_func(void *s)
{
    printf("Main thread continuation: %s\n", (char*)s);
    free(s);
}

void
thread_func(void *s)
{
    struct timeval tv = {0, 1000};
    select(0, NULL, NULL, NULL, &tv);
    printf("Thread: %s\n", (char*)s);
    threadpool_schedule_back(threadpool, main_func, s);
}

void
wakeup(void *pp)
{
    int *p = (int*)pp;
    write(p[1], "a", 1);
}

int
main()
{
    int i, rc;
    int p[2];
    int done;

    rc = pipe(p);
    if(rc < 0) {
        perror("pipe");
        exit(1);
    }

    threadpool = threadpool_create(4, wakeup, (void*)p);
    if(threadpool == NULL) {
        perror("threadpool_create");
        exit(1);
    }

    for(i = 0; i < 20; i++) {
        char *s;
        fd_set fdset;
        char buf[10];
        struct timeval tv = {0, 100};
        FD_ZERO(&fdset);
        FD_SET(p[0], &fdset);

        asprintf(&s, "%d", i);
        rc = threadpool_schedule(threadpool, thread_func, (void*)s);
        if(rc < 0) {
            perror("threadpool_schedule");
            exit(1);
        }

        printf("Going through main loop.\n");
        rc = select(p[0] + 1, &fdset, NULL, NULL, &tv);
        if(rc < 0) {
            perror("select");
            exit(1);
        }
            
        if(rc > 0) {
            read(p[0], buf, 10);
            threadpool_items_run(threadpool_get_back(threadpool));
        }
    }

    printf("Main loop done, preparing to die\n");
    do {
        done = threadpool_die(threadpool, 1);
        threadpool_items_run(threadpool_get_back(threadpool));
    } while(!done);
    printf("Done dying.\n");
    rc = threadpool_destroy(threadpool);
    if(rc < 0)
        abort();
        
    return 0;
}
