#include <stdio.h>
#include <stdlib.h>
#include <pth.h>

volatile int arg;
volatile int res;

void *
thread_routine(void *dummy)
{
        while(arg < 0) {
            pth_yield(NULL);
        }
        res = arg;
        pth_yield(NULL);
}

int
main()
{
    pth_t thread;
    pth_attr_t attr = pth_attr_new();
    pth_attr_init(attr);
#ifdef JOIN
    pth_attr_set(attr, PTH_ATTR_JOINABLE, 1);
#else
    pth_attr_set(attr, PTH_ATTR_JOINABLE, 0);
#endif
    int rc;
    int i, j, s;

    arg = -1;
    res = -1;
    pth_init();
    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            thread = pth_spawn(attr, thread_routine, NULL);
            pth_yield(NULL);
            while(res < 0)
                pth_yield(NULL);
            s += res;
#ifdef JOIN
            pth_join(thread, NULL);
#endif
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
