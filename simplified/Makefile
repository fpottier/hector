.PHONY: all
all:
	@ dune build

CLIENT := $(HOME)/dev/menhir

.PHONY: vendor
vendor:
# Check if this is the menhir branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "menhir" ] ; then \
	  echo "Error: this is not the menhir branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Copy the files of interest into Menhir's working directory.
	@ cp -f Vector.{ml,mli} $(CLIENT)/src/
	@ cd $(CLIENT) && headache -h $(CLIENT)/headers/regular-header src/Vector.{ml,mli}

.PHONY: clean
clean:
	@ git clean -fX
