#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
volatile int n;

static int
inc_n()
{
    int k;
    pthread_mutex_lock(&mutex);
    n++;
    k = n;
    pthread_mutex_unlock(&mutex);
    return k;
}
 
void *
thread_routine(void *dummy)
{
    pthread_t thread;
    int rc, k;
    while(1) {
        rc = pthread_create(&thread, NULL, thread_routine, NULL);
        k = inc_n();
        if(rc != 0) {
            printf("%d\n", k);
            while(1) sched_yield();
        }
        if(k % 10000 == 0) {
            printf("%d\n", k);
        }
    }
}

int
main()
{
    pthread_t thread;
    int rc;
    int n;

    n = 0;
    rc = pthread_create(&thread, NULL, thread_routine, NULL);
    inc_n();
    if(rc != 0) {
        abort();
    }
    while(1)
        sched_yield();
    return 0;
}







