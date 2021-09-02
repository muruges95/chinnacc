CC=gcc
CFLAGS=-std=c11 -g -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

chinnacc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chinnacc.h

test: chinnacc
	./test.sh

clean:
	rm -f chinnacc *.o *~ tmp*

.PHONY: test clean
