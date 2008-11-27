CPC='../obj/x86_LINUX/cilly.asm.exe'

CFLAGS=-O3 -Wall -g -I../runtime

LDLIBS=-lcpc /usr/local/lib/cpc_runtime.o

.SUFFIXES: .cpc .cpi

.PHONY: all

all: barrier conditions done fork loops sleep sleep_cond tcat \
     trivial-httpd trivial-httpd-fork trivial-httpd-nptl trivial-httpd-st\
     static-httpd static-httpd-fork static-httpd-nptl static-httpd-st\
	 trivial-httpd-pth static-httpd-pth

clean:
	rm -f *.o *~ *.cpi
	rm -f barrier conditions done fork loops sleep sleep_cond tcat \
	 trivial-httpd trivial-httpd-fork trivial-httpd-nptl trivial-httpd-st\
 	 static-httpd static-httpd-fork static-httpd-nptl static-httpd-st\
	 trivial-httpd-pth static-httpd-pth

barrier: barrier.o

conditions: conditions.o

done: done.o

fork: fork.o

loops: loops.o

sleep: sleep.o

sleep_cond: sleep_cond.o

tcat: tcat.o

trivial-httpd: trivial-httpd.o
trivial-httpd.o: trivial-httpd.c
static-httpd.cpi: trivial-httpd.cpc
	$(CPP) -P $(CFLAGS) -D__extension__= -D'__attribute__(x)=' \
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

.PHONY: clean
	-rm -f *.cpi

.cpc.cpi:
	$(CPP) -P $(CFLAGS)  -D__extension__= -D'__attribute__(x)=' - \
	    <$< >$@

.cpi.c:
	$(CPC) $< --out $@

