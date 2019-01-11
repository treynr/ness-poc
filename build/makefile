stackopts = --copy-bins
staticopts = --flag "*:static"

dynamic:
	stack build

static:
	stack build $(staticopts)

install:
	stack build $(stackopts)

test: FORCE
	stack test

clean:
	stack clean --full

FORCE:
