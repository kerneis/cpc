#include <stdio.h>

int
main()
{
    int i, j, s;
    for(i = 0; i < 100000; i++) {
        s = 0;
        for(j = 0; j < 10000; j++)
            s += j;
    }
    printf("%d\n", s);
    return 0;
}
