CPC='../bin/cpc.asm.exe'

CDEBUGFLAGS=-Wall -g
CLIBS=-I ../include

CFLAGS+=CDEBUGFLAGS

LDLIBS=-L../runtime -lm -pthread -lcpcfull

.PHONY: all clean servers test

BASICS=barrier boxing bug conditions done loops patch padding sleep\
    sleep_cond detached-torture sleep-torture conditions-torture pointer
EXTENDED=ampersand nontailcall tcat tcat_fd trivial-httpd
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

static-httpd.cpi: trivial-httpd.cpc
	$(CPP) -x c $(CFLAGS) $(CLIBS) -DSTATIC -o $@ $<

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
	rm -f *.o *~ *.cpi *.output core
	for x in *.cpc; do rm -f $${x%.cpc}.c; done;
	rm -f $(BASICS) $(EXTENDED) $(SERVERS)

%.cpi: %.cpc
	$(CPP) -x c $(CFLAGS) $(CLIBS) -o $@ $<
%.c: %.cpi
	$(CPC) $(CPCOPTS) $< --out $@

%.output: %.cpc
	@grep -q "EXPECTED OUTPUT" $< || (echo $<: missing expected output; false)
	@sed "0,/EXPECTED OUTPUT/ d ; /END OF OUTPUT/,$$ d" $< > $@
