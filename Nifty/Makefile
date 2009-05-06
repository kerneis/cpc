################################################################################
#
# Build settings. You will need to adjust these depending on your platform.
#
CC		= gcc
CPPFLAGS	= -I . -D_REENTRANT

#
# By default, use gettimeofday() to get accurate time.
# See ../include/nft_gettime.h for other choices.
#
CPPFLAGS	+= -DUSE_GETTIMEOFDAY

# CPPFLAGS	+= -D_ALL_SOURCE		# Uncomment for Windows SFU builds.

#
# Select debug or optimized build.
#
CFLAGS		= -Wall -g
#CFLAGS		= -Wall -O2 -DNDEBUG		# optimized

#
# Choose appropriate link libraries.
#
LDLIBS		= -lpthread			# Solaris, Linux, AIX
#LDLIBS		= -L/usr/lib/nptl -lpthread	# Linux/NPTL
#LDLIBS		= -lkse				# FreeBSD 5


################################################################################
#
# You should not need to alter the definitions below.
#
SRCS		= nft_pool.c
OBJS		= ${SRCS:.c=.o}
EXES		= ${SRCS:.c=}


all:	$(OBJS)


test:	$(EXES)
	./nft_pool

.c.o:
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $<

#
# Build the unit test programs
#
nft_pool: nft_pool.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -DMAIN $@.c $(LDLIBS) -o $@

clean:
	- rm -f *.o $(EXES)
