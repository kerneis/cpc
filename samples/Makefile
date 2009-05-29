CPC='../bin/cpc.asm.exe'

CLIBS=-I ../include
CFLAGS=-O3 -Wall -g $(CLIBS)

LDLIBS=-L../runtime -lm -pthread -lcpcfull

.SUFFIXES: .cpc .cpi

.PHONY: all clean

all: barrier conditions detached done loops patch sleep sleep_cond \
    tcat trivial-httpd

servers: trivial-httpd trivial-httpd-fork trivial-httpd-nptl \
    trivial-httpd-st static-httpd static-httpd-fork static-httpd-nptl \
    static-httpd-st trivial-httpd-pth static-httpd-pth

barrier: barrier.o

conditions: conditions.o

detached: detached.o

done: done.o

loops: loops.o

patch: patch.o

sleep: sleep.o

sleep_cond: sleep_cond.o

tcat: tcat.o

trivial-httpd: trivial-httpd.o
trivial-httpd.o: trivial-httpd.c
static-httpd.cpi: trivial-httpd.cpc
	$(CPP) -P $(CFLAGS) -include cpc_runtime.h \
	-D__extension__= -D'__attribute__(x)=' \
	-DSTATIC - <$< >$@
static-httpd: static-httpd.o

trivial-httpd-fork: trivial-httpd-fork.o
static-httpd-fork: static-httpd-fork.o
static-httpd-fork.o: trivial-httpd-fork.c
	$(CC) $(CFLAGS) -c -DSTATIC $< -o $@

trivial-httpd-st: LDLIBS+=-lst
static-httpd-st: LDLIBS+=-lst
static-httpd-st.o: trivial-httpd-st.c
	$(CC) $(CFLAGS) -c -DSTATIC $< -o $@

trivial-httpd-nptl: LDLIBS+=-pthread
static-httpd-nptl: LDLIBS+=-pthread
static-httpd-nptl.o: trivial-httpd-nptl.c
	$(CC) $(CFLAGS) -c -DSTATIC $< -o $@

trivial-httpd-pth: LDLIBS+=`pth-config --libs --ldflags`
static-httpd-pth: LDLIBS+=`pth-config --libs --ldflags`
static-httpd-pth.o: trivial-httpd-pth.c
	$(CC) $(CFLAGS) -c -DSTATIC $< -o $@

clean:
	rm -f *.o *~ *.cpi trivial-httpd.c
	rm -f barrier conditions detached done loops patch sleep sleep_cond tcat \
	 trivial-httpd trivial-httpd-fork trivial-httpd-nptl trivial-httpd-st\
	 static-httpd static-httpd-fork static-httpd-nptl static-httpd-st\
	 trivial-httpd-pth static-httpd-pth

.cpc.cpi:
	$(CPP) -x c $(CFLAGS) -include cpc_runtime.h \
	  -o $@ $<
.cpi.c:
	$(CPC) $< --out $@

