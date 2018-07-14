CC = gcc
## Use the C99 standard, compile with warnings, super optimize
OPTS = --std=c99 -Wall -O3
OUT = -o walker

objects:
	$(CC) $(OPTS) -fPIC -c walk.c -o libwalk.o
	$(CC) libwalk.o -shared -o libwalk.so
	ar rcs libwalk.a libwalk.o

all: install objects

install:
	$(CC) $(OPTS) $(OUT) walk.c main.c

clean:
	rm -f *.o *.so walker
