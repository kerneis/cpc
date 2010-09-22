#include <stdio.h>

typedef int (*ft)(int);
extern int f(int x);

int
main()
{
    int i, j;
    volatile int s;
    volatile ft p = &f;
    for(i = 0; i < 100000; i++) {
        s = 0;
        for(j = 0; j < 1000; j++) {
            s += (*p)(j);
        }
    }
    printf("%d\n", s);
    return 0;
}
