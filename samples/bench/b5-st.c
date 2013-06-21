#include <stdio.h>
#include <stdlib.h>
#include <st.h>

volatile int arg;
volatile int res;

st_cond_t c1, c2;

void *
thread_routine(void *dummy)
{
    while(1) {
        st_cond_wait(c1);
        res = arg;
        arg = -1;
        st_cond_signal(c2);
    }
}

int
main()
{
    st_thread_t thread;
    int rc;
    int i, j, s;

    st_init();
    c1 = st_cond_new();
    c2 = st_cond_new();

    arg = -1;
    res = 0;
    thread = st_thread_create(thread_routine, NULL, 0, 0);
    st_sleep(0); /* yield to avoid dead-lock */
    
    for(i = 0; i < 100; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            st_cond_signal(c1);
            st_cond_wait(c2);
            s += res;
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
