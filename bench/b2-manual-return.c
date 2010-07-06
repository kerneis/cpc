#include <stdio.h>

int ret;

extern int f(int, int*);

int
main()
{
    int i, j, s;
    for(i = 0; i < 100000; i++) {
        s = 0;
        for(j = 0; j < 1000; j++) {
            f(j, &ret);
            s += ret;
        }
    }
    printf("%d\n", s);
    return 0;
}
