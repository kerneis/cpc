PREFIX = /usr/local

CDEBUGFLAGS = -O2 -g -Wall

DEFINES = $(PLATFORM_DEFINES)

CFLAGS = $(CDEBUGFLAGS) $(DEFINES) $(EXTRA_DEFINES)

LDLIBS = -lpthread

SRCS = threadpool.c threadpool-example.c

OBJS = threadpool.o threadpool-example.o

all: threadpool.o threadpool-example

threadpool-example: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o threadpool-example $(OBJS) $(LDLIBS)

.PHONY: clean

clean:
	-rm -f threadpool-example threadpool-torture a.out *.o *~ core TAGS gmon.out

