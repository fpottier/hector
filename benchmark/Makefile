# [make all] compiles this benchmark.

.PHONY: all
all:
	@ dune build --profile release

# [make bench] runs the benchmarks.

# Using --profile release ensures that debug assertions are turned off.

MAIN := \
  dune exec --profile release \
  --display=quiet --no-print-directory \
  ./main.exe -- \

.PHONY: bench
bench:
	@ mkdir -p log
	@ echo
	@ $(MAIN) --push 10000 2>&1 | tee log/push.log
	@ echo
	@ $(MAIN) --pop 10000 2>&1 | tee log/pop.log
	@ echo
	@ $(MAIN) --get  10000 2>&1 | tee log/get.log
	@ echo
	@ $(MAIN) --set  10000 2>&1 | tee log/set.log
	@ echo
	@ $(MAIN) --iter 10000 2>&1 | tee log/iter.log
	@ echo
	@ $(MAIN) --iteri 100000 2>&1 | tee log/iteri.log
	@ echo
	@ $(MAIN) --pusha 10000 2>&1 | tee log/pusha.log
	@ echo
	@ $(MAIN) --pushl 10000 2>&1 | tee log/pushl.log
	@ echo
	@ $(MAIN) --sort 10000 2>&1 | tee log/sort.log

ASSEMBLY=$(shell find ../_build -name '*Main.s')
.PHONY: assembly
assembly:
	@ dune clean
	@ make all
	@ open -a /Applications/Emacs.app $(ASSEMBLY)
