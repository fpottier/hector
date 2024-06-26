# [make all] compiles this benchmark.
# Using --profile release ensures that debug assertions are turned off.

.PHONY: all
all:
	@ dune build --profile release


MAIN := ../_build/default/array_unsafe_set/main.exe

.PHONY: test
test: all
	@ hyperfine -N --min-runs 30 --warmup 10 \
	  "$(MAIN)" \
	  "$(MAIN) --unsafe" \
	  "$(MAIN) --unrolled" \
	  "$(MAIN) --unsafe --unrolled" \

.PHONY: once
once: all
	@ echo
	@ echo Running safe:
	@ echo
	@ $(MAIN)
	@ echo
	@ echo Running unsafe:
	@ echo
	@ $(MAIN) --unsafe
	@ echo
	@ echo Running unrolled:
	@ echo
	@ $(MAIN) --unrolled
	@ echo
	@ echo Running unsafe unrolled:
	@ echo
	@ $(MAIN) --unsafe --unrolled

.PHONY: assembly
assembly:
	@ dune clean
	@ make all
	@ open -a /Applications/Emacs.app ../_build/default/array_unsafe_set/.main.eobjs/native/dune__exe__Main.s
