#include <stdio.h>

volatile int ret;

extern void f(int);

int
main()
{
    int i, j;
    volatile int s;
    for(i = 0; i < 100000; i++) {
        s = 0;
        for(j = 0; j < 1000; j++) {
            f(j);
            s += ret;
        }
    }
    printf("%d\n", s);
    return 0;
}
