#include <stdio.h>
#include <stdlib.h>
#include <pth.h>

int n;

static int
inc_n()
{
    int k;
    n++;
    k = n;
    return k;
}
 
void *
thread_routine(void *dummy)
{
    pth_t thread;
    int k;
    while(1) {
        pth_yield(NULL);
        thread = pth_spawn(PTH_ATTR_DEFAULT, thread_routine, NULL);
        if(thread == NULL) {
            printf("%d\n", n);
            abort();
        }
        k = inc_n();
        if(k % 100 == 0) {
            printf("%d\n", k);
        }
    }
}

int
main()
{
    int n;
    pth_t thread;

    pth_init();
    n = 0;
    thread = pth_spawn(PTH_ATTR_DEFAULT, thread_routine, NULL);
    inc_n();

    while(1)
        pth_yield(NULL);
    return 0;
}







