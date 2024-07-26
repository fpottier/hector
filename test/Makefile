# [make test] runs 7 tests in parallel, namely:

# 4 tests of the Vector API:
#   Hector.Int
#   Hector.Mono.Make(Int)
#   Hector.Poly
#   Dynarray
# 3 tests of the Stack API:
#   Hector.Int.Stack
#   Hector.Mono.Make(Int).Stack
#   Hector.Poly.Stack

# The subdirectories of the 4 tests of the Vector API are:
#   int       (synonymous with hector/int)
#   mono      (synonymous with hector/mono)
#   poly      (synonymous with hector/poly)
#   dynarray
# The subdirectories of the 3 tests of the Stack API are:
#   int/stack
#   mono/stack
#   poly/stack

# The subdirectory hector serves no deep purpose;
# it allows the 7 tests to have names of comparable lengths,
# so that the output produced by GNU parallel is visually aligned.

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  hector/int \
	  hector/mono \
	  hector/poly \
	  dynarray \
	  int/stack \
	  mono/stack \
	  poly/stack \

.PHONY: clean
clean:
	@ git clean -fdX
