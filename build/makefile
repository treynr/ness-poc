stackopts = --copy-bins
staticopts = --flag "*:static"

dynamic:
	stack build $(stackopts)

static:
	stack build $(staticopts) $(stackopts)

install:
	stack install

clean:
	stack clean --full

