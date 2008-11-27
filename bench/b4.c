#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>

volatile int arg;
volatile int res;

void *
thread_routine(void *dummy)
{
    while(arg < 0)
        sched_yield();
    res = arg;
}

int
main()
{
    pthread_t thread;
    int rc;
    int i, j, s;

    arg = -1;
    res = -1;

    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            rc = pthread_create(&thread, NULL, thread_routine, NULL);
            if(rc < 0)
                abort();
            while(res < 0)
                sched_yield();
            s += res;
            pthread_join(thread, NULL);
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
