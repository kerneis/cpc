#include <stdio.h>
#include <stdlib.h>
#include <pth.h>

volatile int arg;
volatile int res;

pth_mutex_t mutex = PTH_MUTEX_INIT;
pth_cond_t c1 = PTH_COND_INIT,
    c2 = PTH_COND_INIT;

void *
thread_routine(void *dummy)
{
    while(1) {
        pth_mutex_acquire(&mutex, FALSE, NULL);
        if(arg < 0)
            pth_cond_await(&c1, &mutex, NULL);
        res = arg;
        arg = -1;
        pth_mutex_release(&mutex);
        pth_cond_notify(&c2, FALSE);
    }
}

int
main()
{
    pth_t thread;
    int rc;
    int i, j, s;

    arg = -1;
    res = 0;
    pth_init();
    thread = pth_spawn(PTH_ATTR_DEFAULT, thread_routine, NULL);
    
    for(i = 0; i < 100; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            pth_mutex_acquire(&mutex, FALSE, NULL);
            res = -1;
            arg = j;
            pth_mutex_release(&mutex);
            pth_cond_notify(&c1, FALSE);
            pth_mutex_acquire(&mutex, FALSE, NULL);
            if(res < 0)
                pth_cond_await(&c2, &mutex, NULL);
            s += res;
            arg = -1;
            pth_mutex_release(&mutex);
        }
    }
    printf("%d\n", s);
    return 0;
}
