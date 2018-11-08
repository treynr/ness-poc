CC = g++
## Use the C++11 standard, compile with warnings, super optimize
OPTS = --std=c++11 -Wall -O3
OUT = -o nessw

install:
	$(CC) $(OPTS) -fPIC $(OUT) walk.c main.cpp

clean:
	rm -f *.o *.so nessw
