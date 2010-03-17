CPC='../bin/cpc.asm.exe'

CDEBUGFLAGS=-O3 -Wall -fno-strict-aliasing
CLIBS=-I ../include/cpc -pthread
CFLAGS=$(CDEBUGFLAGS) $(CLIBS) $(EXTRA_DEFINES)

PREFIX=/usr/local

.SUFFIXES: .cpc .cpi

.PHONY: all install clean

all: libcpc.a libcpcfull.a

cpc_runtime.o: cpc_runtime.c
	$(CC) $(CFLAGS) -c $<

libcpc.a: cpc_runtime.o threadpool/threadpool.o
	rm -f $@
	ar rc $@ $^
	ranlib $@

libcpcfull.a: cpc_runtime.o threadpool/threadpool.o cpc_barrier.o cpc_io.o
	rm -f $@
	ar rc $@ $^
	ranlib $@

install: all
	cp -f cpc_runtime.a libcpc.a $(PREFIX)/lib/

clean:
	rm -f *~
	rm -f cpc_runtime.o threadpool/threadpool.o
	rm -f libcpc.a libcpcfull.a
	rm -f cpc_barrier.o cpc_io.o
	rm -f cpc_barrier.cpi cpc_io.cpi
	rm -f cpc_barrier.c cpc_io.c

.cpc.cpi:
	$(CC) -E -x c $(CFLAGS) -include cpc_runtime.h \
	  -o $@ $<

.cpi.c:
	$(CPC) $< --out $@

