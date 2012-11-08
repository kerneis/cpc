CPC='../bin/cpc'

CDEBUGFLAGS=-Wall -g
CLIBS=-I ../include

CFLAGS+=$(CDEBUGFLAGS) $(EXTRA_DEFINES) -Os

LDLIBS=-L../runtime -lcpc -lm -lWs2_32 -lMswsock

.PHONY: all clean servers test

BASICS=barrier boxing bug conditions done loops patch padding sleep\
    sleep_cond detached-torture sleep-torture conditions-torture pointer\
	detached-spawn cps_funptr
EXTENDED=ampersand nontailcall copy trivial-httpd
SERVERS=trivial-httpd trivial-httpd-fork trivial-httpd-nptl trivial-httpd-st\
	 static-httpd static-httpd-fork static-httpd-nptl static-httpd-st\
	 trivial-httpd-pth static-httpd-pth

all: $(BASICS) $(EXTENDED)

servers: $(SERVERS)

test: $(patsubst %,test/%,$(BASICS))

$(BASICS) $(EXTENDED): %: %.o

test/%: % %.output 
	@((./$* || echo FAIL) | diff -q $*.output - > /dev/null)\
        || (echo $@: FAIL; false)
	@ echo $@: SUCCESS

static-httpd.o: trivial-httpd.cpc
	$(CPC) $(CPCOPTS) $(CFLAGS) $(CLIBS) -c -DSTATIC $< -o $@

static-httpd-fork.o: trivial-httpd-fork.c
	$(CC) $(CFLAGS) $(CLIBS) -c -DSTATIC $< -o $@

trivial-httpd-st: LDLIBS+=-lst
static-httpd-st: LDLIBS+=-lst
static-httpd-st.o: trivial-httpd-st.c
	$(CC) $(CFLAGS) $(CLIBS) -c -DSTATIC $< -o $@

trivial-httpd-nptl: LDLIBS+=-pthread
static-httpd-nptl: LDLIBS+=-pthread
static-httpd-nptl.o: trivial-httpd-nptl.c
	$(CC) $(CFLAGS) $(CLIBS) -c -DSTATIC $< -o $@

trivial-httpd-pth: LDLIBS+=`pth-config --libs --ldflags`
static-httpd-pth: LDLIBS+=`pth-config --libs --ldflags`
static-httpd-pth.o: trivial-httpd-pth.c
	$(CC) $(CFLAGS) $(CLIBS) -c -DSTATIC $< -o $@

clean:
	rm -f *.o *~ *.cpc.* *.output core
	rm -f $(BASICS) $(EXTENDED) $(SERVERS)

%.o: %.cpc
	$(CPC) $(CPCOPTS) $(CFLAGS) $(CLIBS) -o $@ -c $<

%.output: %.cpc
	@grep -q "EXPECTED OUTPUT" $< || (echo $<: missing expected output; false)
	@sed "1,/EXPECTED OUTPUT/ d ; /END OF OUTPUT/,$$ d" $< > $@
