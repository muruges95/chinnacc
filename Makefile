CC=gcc
CFLAGS=-std=c11 -g -fno-common

chinnacc: main.o
	$(CC) -o chinnacc main.o $(LDFLAGS)

test: chinnacc
	./test.sh

clean:
	rm -f chinnacc *.o *~ tmp*

.PHONY: test clean
