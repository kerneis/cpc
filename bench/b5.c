#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>

volatile int arg;
volatile int res;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t c1 = PTHREAD_COND_INITIALIZER,
    c2 = PTHREAD_COND_INITIALIZER;

void *
thread_routine(void *dummy)
{
    while(1) {
        pthread_mutex_lock(&mutex);
        if(arg < 0)
            pthread_cond_wait(&c1, &mutex);
        res = arg;
        arg = -1;
        pthread_mutex_unlock(&mutex);
        pthread_cond_signal(&c2);
    }
}

int
main()
{
    pthread_t thread;
    int rc;
    int i, j, s;

    arg = -1;
    res = 0;
    rc = pthread_create(&thread, NULL, thread_routine, NULL);
    if(rc != 0)
        abort();
    
    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            pthread_mutex_lock(&mutex);
            res = -1;
            arg = j;
            pthread_mutex_unlock(&mutex);
            pthread_cond_signal(&c1);
            pthread_mutex_lock(&mutex);
            if(res < 0)
                pthread_cond_wait(&c2, &mutex);
            s += res;
            arg = -1;
            pthread_mutex_unlock(&mutex);
        }
    }
    printf("%d\n", s);
    return 0;
}
