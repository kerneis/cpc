CPC='../bin/cpc.asm.exe'

CDEBUGFLAGS=-Wall -g -O2

CLIBS=-I ../include
CFLAGS=$(CDEBUGFLAGS) $(EXTRA_DEFINES)

.PHONY: all clean

BENCH=b1 b2  b2-indirect b2-manual-return b2-cpc b2-manual-return-cpc \
			b2-aligned-cpc \
			b3-nptl  b3-cpc b3-pth  b3-st \
			b4-nptl  b4-cpc  b4-pth  b4-st \
			b5-nptl b5-cpc b5-pth b5-st b6-nptl  b6-cpc  b6-pth  b6-4kb-st b6-st \
#			b2-misaligned-cpc 

all: $(BENCH)

b2: b2.o b2b.o
b2-indirect: b2-indirect.o b2b.o
b2-manual-return: b2-manual-return.o b2b-manual-return.o

b2-misaligned-cpc.c: CPCOPTS+=--packed
# You must build cpc with compact continuations, and rename libcpcfull.a
# into libcpcfullcompact.a for this to work.
# If you have no idea how to do this, then you should not run this
# benchmark.
b2-misaligned-cpc: LDLIBS=-L../runtime -lm -pthread -lcpcfullcompact

b6-4kb-st.o: b6-st.c
	$(CC) $(CFLAGS) $(CLIBS) -DSTACK=4096 -c $< -o $@

%-nptl: LDLIBS+=-pthread
%-cpc: LDLIBS+=-L../runtime -lm -pthread -lcpcfull
%-st: LDLIBS+=-lst
%-pth: LDLIBS+=`pth-config --libs --ldflags`


clean:
	rm -f *.o *~ *.cpi core
	for x in *.cpc; do rm -f $${x%.cpc}.c; done;
	rm -f $(BENCH)

%.cpi: %.cpc
	$(CPP) -x c $(CFLAGS) $(CLIBS) -include cpc/cpc_runtime.h \
	  -o $@ $<
%.c: %.cpi
	$(CPC) $(CPCOPTS) $< --out $@
