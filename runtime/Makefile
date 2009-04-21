CPC='../bin/cpc.asm.exe'

CDEBUGFLAGS=-O3 -Wall
CFLAGS=$(CDEBUGFLAGS) $(EXTRA_DEFINES)

PREFIX=/usr/local

.SUFFIXES: .cpc .cpi

.PHONY: all install clean

all: cpc_runtime.o libcpc.a

cpc_runtime.o: cpc_runtime.c
	$(CC) $(CFLAGS) -c $<

libcpc.a: cpc_barrier.o cpc_io.o
	rm -f $@
	ar rc $@ $^
	ranlib $@

install: all
	cp -f cpc_runtime.o libcpc.a $(PREFIX)/lib/

clean:
	rm -f *~
	rm -f cpc_runtime.o
	rm -f libcpc.a
	rm -f cpc_barrier.o cpc_io.o
	rm -f cpc_barrier.cpi cpc_io.cpi
	rm -f cpc_barrier.c cpc_io.c

.cpc.cpi:
	$(CC) -E $(CFLAGS) -include cpc_runtime.h \
	  - <$< >$@

.cpi.c:
	$(CPC) --check $< --out $@

