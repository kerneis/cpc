#include <stdio.h>
#include <stdlib.h>
#include <st.h>

volatile int arg;
volatile int res;

void *
thread_routine(void *dummy)
{
    while(1) {
        while(arg < 0) {
            st_sleep(0);
        }
        res = arg;
        st_sleep(0);
    }
}

int
main()
{
    st_thread_t thread;
    int rc;
    int i, j, s;

    arg = -1;
    res = -1;
    st_init();
    thread = st_thread_create(thread_routine, NULL, 0, 0);

    for(i = 0; i < 10; i++) {
        s = 0;
        for(j = 0; j < 10000; j++) {
            res = -1;
            arg = j;
            st_sleep(0);
            while(res < 0)
                st_sleep(0);
            s += res;
            arg = -1;
        }
    }
    printf("%d\n", s);
    return 0;
}
