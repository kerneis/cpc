CPC='../bin/cpc.asm.exe'

CDEBUGFLAGS=-Wall -g -O2 -Wno-unknown-pragmas
CLIBS=-I ../include/cpc
CFLAGS += $(CDEBUGFLAGS) $(EXTRA_DEFINES)

# To build with compact continuations , disable the following line
# CFLAGS += -DCPC_COMPACT_CONTINUATIONS

# Import $(RANLIB)
include ../config.mk

LDLIBS=-pthread

PREFIX=/usr/local

.SUFFIXES: .cpc .cpi

.PHONY: all clean compact

all: libcpc.a libcpcfull.a

cpc_runtime.o: cpc_runtime.c
	$(CC) $(CFLAGS) $(CLIBS) -c $<

threadpool/threadpool.o: threadpool/threadpool.c
	cd threadpool && $(MAKE) threadpool.o CFLAGS="$(CFLAGS)"

libcpc.a: cpc_runtime.o threadpool/threadpool.o
	rm -f $@
	$(AR) rc $@ $^
	$(RANLIB) $@

libcpcfull.a: cpc_runtime.o threadpool/threadpool.o cpc_barrier.o cpc_io.o
	rm -f $@
	$(AR) rc $@ $^
	$(RANLIB) $@

clean:
	rm -f *~
	rm -f cpc_runtime.o threadpool/threadpool.o
	rm -f libcpc.a libcpcfull.a
	rm -f cpc_barrier.o cpc_io.o
	rm -f cpc_barrier.cpi cpc_io.cpi
	rm -f cpc_barrier.c cpc_io.c

.cpc.cpi:
	$(CC) -E -x c $(CFLAGS) $(CLIBS) -o $@ $<

.cpi.c:
	$(CPC) $(CPCOPTS) $< --out $@

