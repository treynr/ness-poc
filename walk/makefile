CC = g++
## Use the C++11 standard, compile with warnings, super optimize
OPTS = --std=c++11 -Wall -O3
OUT = nessw
INSTALL = ${HOME}/.local/bin

nessw: walk.cpp main.cpp
	$(CC) $(OPTS) -fPIC -o $(OUT) $^

install: nessw
	cp $(OUT) $(INSTALL)

test: nessw
	cd tests && ./test-walk.sh

clean:
	rm -f *.o *.so nessw
