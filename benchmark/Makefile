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
	mkdir -p log
	@ echo
	@ $(MAIN) --push 10000 2>&1 | tee log/push.log
	@ echo
	@ $(MAIN) --get 10000  2>&1 | tee log/get.log
	@ echo
	@ $(MAIN) --set 10000  2>&1 | tee log/set.log
#	@ echo
#	@ $(MAIN) --scan 10000  2>&1 | tee log/set.log
