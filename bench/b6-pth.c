#include <stdio.h>
#include <stdlib.h>
#include <pth.h>

int n;
pth_attr_t attr;

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
        thread = pth_spawn(attr, thread_routine, NULL);
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
    attr = pth_attr_new();
    pth_attr_init(attr);
#ifdef JOIN
    pth_attr_set(attr, PTH_ATTR_JOINABLE, 1);
#else
    pth_attr_set(attr, PTH_ATTR_JOINABLE, 0);
#endif

    setbuf(stdout, NULL);

    pth_init();
    n = 0;
    thread = pth_spawn(attr, thread_routine, NULL);
    inc_n();

    while(1)
        pth_yield(NULL);
    return 0;
}







