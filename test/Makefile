# [make test] runs 9 tests in parallel, namely:

# 5 tests of the Vector API:
#   Hector.Int
#   Hector.Mono.Make(Int)
#   Hector.Poly
#   Dynarray
#   Hector_simplified.Vector
# 4 tests of the Stack API:
#   Hector.Int.Stack
#   Hector.Mono.Make(Int).Stack
#   Hector.Poly.Stack
#   Hector_simplified.Vector.Stack

# The subdirectories of the 5 tests of the Vector API are:
#   int          (synonymous with hector/int)
#   mono         (synonymous with hector/mono)
#   poly         (synonymous with hector/poly)
#   dynarray
#   simplified
# The subdirectories of the 3 tests of the Stack API are:
#   int/stack
#   mono/stack
#   poly/stack
#   simple/stack (synonymous with simplified/stack)

# The subdirectory hector serves no deep purpose;
# it allows the tests to have names of comparable lengths,
# so that the output produced by GNU parallel is visually aligned.

SUBDIRS := \
  hector/int \
  hector/mono \
  hector/poly \
  dynarray \
  int/stack \
  mono/stack \
  poly/stack \
  simplified \
  simple/stack \

.PHONY: test
test:
	@ make clean
	@ for d in $(SUBDIRS) ; do make -C $$d all ; done
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  $(SUBDIRS)

.PHONY: clean
clean:
	@ git clean -fdX
