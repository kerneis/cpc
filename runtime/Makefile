CPC='../bin/cpc'

CDEBUGFLAGS=-Wall -g -O2 -Wno-unknown-pragmas
CLIBS=-I ../include/cpc
CFLAGS += $(CDEBUGFLAGS) $(EXTRA_DEFINES)

# To build with compact continuations , disable the following lines
# CFLAGS += -DCPC_COMPACT_CONTINUATIONS
# CPCOPTS += --packed
# To build with environments, disable the following lines
# CFLAGS += -DCPC_INDIRECT_PATCH
# CPCOPTS += --ecpc

# Import $(RANLIB)
include ../config.mk

LDLIBS=-pthread

PREFIX=/usr/local

.SUFFIXES: .cpc .o

.PHONY: all clean compact

all: libcpc.a libcpcfull.a

cpc_runtime.o: cpc_runtime.c
	$(CC) $(CFLAGS) $(CLIBS) -c $<

cpc_libev.o: cpc_libev.c
	$(CC) $(CFLAGS) $(CLIBS) -I libev -c $<

threadpool/threadpool.o: threadpool/threadpool.c
	cd threadpool && $(MAKE) threadpool.o CFLAGS="$(CFLAGS)"

libcpc.a: cpc_libev.o threadpool/threadpool.o
	rm -f $@
	$(AR) rc $@ $^
	$(RANLIB) $@

libcpcfull.a: cpc_libev.o threadpool/threadpool.o  cpc_barrier.o cpc_io.o
	rm -f $@
	$(AR) rc $@ $^
	$(RANLIB) $@

clean:
	rm -f *~
	$(MAKE) -C threadpool clean
	rm -f libcpc.a libcpcfull.a
	rm -f *.o *.cpc.*

.cpc.o:
	$(CPC) $(CPCOPTS) $(CFLAGS) $(CLIBS) -o $@ -c $<

