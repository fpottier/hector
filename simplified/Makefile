.PHONY: all
all:
	@ dune build

CLIENT := $(HOME)/dev/menhir

.PHONY: vendor
vendor:
	@ cp -f Vector.{ml,mli} $(CLIENT)/src/
	@ cd $(CLIENT) && headache -h $(CLIENT)/headers/regular-header src/Vector.{ml,mli}

.PHONY: clean
clean:
	@ git clean -fX
