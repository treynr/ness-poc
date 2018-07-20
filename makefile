cc = gcc
csrc = csrc
dir := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))/$(csrc)
stackopts = --copy-bins --extra-lib-dirs $(dir) --extra-include-dirs $(dir)
staticopts = --flag "*:static"

error:
	@echo "Choose a compilation target: dynamic, static, clean"
	@exit 1

static: $(dir)/libwalk.a
	export LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(dir); \
	stack build $(staticopts) $(stackopts)

dynamic: $(dir)/libwalk.so
	export LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(dir); \
	stack build $(stackopts)

install:
	stack install

$(dir)/libwalk.so:
	$(MAKE) objects -C $(csrc)

$(dir)/libwalk.a:
	$(MAKE) objects -C $(csrc)

clean:
	stack clean
	$(MAKE) clean -C $(CSRC)

