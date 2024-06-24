# [make all] compiles this benchmark.
# Using --profile release ensures that debug assertions are turned off.

.PHONY: all
all:
	@ dune build --profile release


MAIN := ../_build/default/unsafe_set/main.exe
.PHONY: test
test: all
	@ hyperfine --min-runs 30 --warmup 10 \
	  "$(MAIN)" \
	  "$(MAIN) --unsafe"

.PHONY: assembly
assembly: all
	@ open -a /Applications/Emacs.app ../_build/default/unsafe_set/.main.eobjs/native/dune__exe__Main.s
