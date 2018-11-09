CC = g++
## Use the C++11 standard, compile with warnings, super optimize
OPTS = --std=c++11 -Wall -O3
OUT = nessw
INSTALL = ${HOME}/.local/bin

nessw: walk.c main.cpp
	$(CC) $(OPTS) -fPIC -o $(OUT) $^

install: nessw
	cp $(OUT) $(INSTALL)

clean:
	rm -f *.o *.so nessw
