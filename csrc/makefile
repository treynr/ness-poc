CC = gcc
## Use the new C++11 standard, compile with warnings, super optimize
OPTS = --std=c99 -Wall -O3
OUT = -o walker

all: clean install objects

install:
	$(CC) $(OPTS) $(OUT) walk.c main.c

objects:
	$(CC) $(OPTS) -fPIC -c walk.c -o libwalk.o
	$(CC) libwalk.o -shared -o libwalk.so

clean:
	rm -f *.o *.so walker
