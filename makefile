CC = gcc
CSRC = csrc
## Use the C99 standard, compile with warnings, super optimize
OPTS = --std=c99 -Wall -O3
OUT = -o walker
DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))/csrc

build: cbuild hbuild

cbuild:
	$(MAKE) -C $(CSRC)
	$(MAKE) install -C $(CSRC)
	$(MAKE) objects -C $(CSRC)

hbuild: 
	stack build --copy-bins --extra-include-dirs $(DIR) --extra-lib-dirs $(DIR)

clean:
	stack clean
	$(MAKE) clean -C $(CSRC)
