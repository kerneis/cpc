#include <stdio.h>
#include <stdlib.h>
#include <pth.h>

volatile int arg;
volatile int res;

void *
thread_routine(void *dummy)
{
    while(1) {
        while(arg < 0) {
            pth_yield(NULL);
        }
        res = arg;
        pth_yield(NULL);
    }
}

int
main()
{
    pth_t thread;
    int rc;
    int i, j, s;

    arg = -1;
    res = -1;
    pth_init();
    thread = pth_spawn(pth_attr_new(), thread_routine, NULL);

    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            pth_yield(NULL);
            while(res < 0)
                pth_yield(NULL);
            s += res;
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
