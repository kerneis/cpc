#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>

volatile int arg;
volatile int res;

void *
thread_routine(void *dummy)
{
    while(1) {
        while(arg < 0)
            sched_yield();
        res = arg;
        sched_yield();
    }
}

int
main()
{
    pthread_t thread;
    int rc;
    int i, j, s;

    arg = -1;
    res = -1;
    rc = pthread_create(&thread, NULL, thread_routine, NULL);
    if(rc != 0)
        abort();

    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            sched_yield();
            while(res < 0)
                sched_yield();
            s += res;
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
