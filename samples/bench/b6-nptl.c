#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_attr_t attr;
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
        rc = pthread_create(&thread, &attr, thread_routine, NULL);
        k = inc_n();
        if(rc != 0) {
            printf("%d\n", k);
            abort();
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
    pthread_attr_init(&attr);
#ifdef JOIN
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
#else
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
#endif
    int rc;
    int n;

    setbuf(stdout, NULL);

    n = 0;
    rc = pthread_create(&thread, &attr, thread_routine, NULL);
    inc_n();
    if(rc != 0) {
        abort();
    }
    while(1)
        sched_yield();
    return 0;
}







