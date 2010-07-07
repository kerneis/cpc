#include <stdio.h>
#include <stdlib.h>
#include <st.h>

#ifndef STACK
#define STACK 0
#endif

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
    st_thread_t thread;
    int k;
    while(1) {
        st_sleep(0);
        thread = st_thread_create(thread_routine, NULL, 0, STACK);
        if(thread == NULL) {
            printf("%d\n", n);
            abort();
        }
        k = inc_n();
        if(k % 100 == 0) {
            printf("%d\n", k);
            fflush(stdout); /* setbuf(stdout, NULL); causes a segfault! */
        }
    }
}

int
main()
{
    int n;
    st_thread_t thread;

    st_init();
    n = 0;
    thread = st_thread_create(thread_routine, NULL, 0, STACK);
    inc_n();

    while(1)
        st_sleep(1);
    return 0;
}







