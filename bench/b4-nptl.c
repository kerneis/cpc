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
    pthread_attr_t attr;
    pthread_attr_init(&attr);
#ifdef JOIN
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
#else
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
#endif
    int rc;
    int i, j, s;

    arg = -1;
    res = -1;

    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            rc = pthread_create(&thread, &attr, thread_routine, NULL);
            if(rc < 0)
                abort();
            while(res < 0)
                sched_yield();
            s += res;
#ifdef JOIN
            pthread_join(thread, NULL);
#endif
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
