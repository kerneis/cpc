CPC='../bin/cpc.asm.exe'

CDEBUGFLAGS=-O3 -Wall -fno-strict-aliasing
CLIBS=-I ../libev -I ../Nifty -pthread
CFLAGS=$(CDEBUGFLAGS) $(CLIBS) $(EXTRA_DEFINES)

PREFIX=/usr/local

.SUFFIXES: .cpc .cpi

.PHONY: all install clean

all: cpc_runtime.a libcpc.a

cpc_runtime.o: cpc_runtime.c
	$(CC) $(CFLAGS) -c $<

cpc_runtime.a: cpc_runtime.o ../Nifty/nft_pool.o
	rm -f $@
	ar rc $@ $^
	ranlib $@

libcpc.a: cpc_barrier.o cpc_io.o
	rm -f $@
	ar rc $@ $^
	ranlib $@

install: all
	cp -f cpc_runtime.a libcpc.a $(PREFIX)/lib/

clean:
	rm -f *~
	rm -f cpc_runtime.o ../Nifty/nft_pool.o
	rm -f libcpc.a cpc_runtime.a
	rm -f cpc_barrier.o cpc_io.o
	rm -f cpc_barrier.cpi cpc_io.cpi
	rm -f cpc_barrier.c cpc_io.c

.cpc.cpi:
	$(CC) -E -x c $(CFLAGS) -include cpc_runtime.h \
	  -o $@ $<

.cpi.c:
	$(CPC) $< --out $@

