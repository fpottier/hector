# ------------------------------------------------------------------------------

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# This is used in [make release] to search CHANGES.md.
# Ideally, it should be derived from $(DATE).
DATE_WITH_SLASHES := $(shell /bin/date +%Y/%m/%d)

# The project's name.
THIS     := hector

# The archive's URL (https).
ARCHIVE  := https://github.com/fpottier/$(THIS)/archive/$(DATE).tar.gz

# ------------------------------------------------------------------------------

.PHONY: all
all:
	@ dune build @all

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: test
test:
	@ make -C test test

.PHONY: bench
bench:
	@ dune build @all
	@ make -C benchmark bench

.PHONY: install
install:
	@ dune clean
	@ dune build -p $(THIS)
	@ dune install -p $(THIS)

.PHONY: uninstall
uninstall:
	@ ocamlfind remove $(THIS) || true

.PHONY: reinstall
reinstall: uninstall
	@ make install

.PHONY: pin
pin:
	opam pin --yes add $(THIS) .

.PHONY: unpin
unpin:
	opam pin --yes remove $(THIS)

.PHONY: show
show: reinstall
	@ echo "#require \"hector\";;\n#show Hector;;" | ocaml

ASSEMBLY=$(shell find . -name "hector__Int.s")
.PHONY: assembly
assembly:
	@ dune clean && dune build --profile=release
	@ cat $(ASSEMBLY) \
	| grep -vw "\.loc" \
	| grep -vw "\.long" \
	| grep -vw "\.short" \
	| grep -vw "\.asciz" \
	| grep -vw "\.globl" \
	| grep -vw "\.quad" \
	| gsed "s/L[[:digit:]]\+/L/g" \
	> simplified.s
	@ open -a /Applications/Emacs.app/ simplified.s

# ------------------------------------------------------------------------------

# Documentation.

# Unfortunately, odoc seems unable to produce proper (hyperlinked)
# references to standard library types, such as [Seq.t]. So, we search
# for unresolved references in the HTML output and suppress them.

DOCDIR = _build/default/_doc/_html
DOC    = $(DOCDIR)/index.html

.PHONY: doc
doc:
	@ rm -rf _build/default/_doc
	@ dune clean
	@ dune build @doc
	@ find $(DOCDIR) -name "*.html" \
	  | xargs sed -i.bak 's|<span class="xref-unresolved">Stdlib</span>.||g'
	@ echo "You can view the documentation by typing 'make view'".

.PHONY: view
view: doc
	@ echo Attempting to open $(DOC)...
	@ if command -v firefox > /dev/null ; then \
	  firefox $(DOC) ; \
	else \
	  open -a /Applications/Firefox.app/ $(DOC) ; \
	fi

.PHONY: export
export: doc
	ssh yquem.inria.fr rm -rf public_html/$(THIS)/doc
	scp -r $(DOCDIR) yquem.inria.fr:public_html/$(THIS)/doc

# [make count] counts how many operations this library exports.

.PHONY: count
count:
	@ echo "Number of operations in the signature (not counting synonyms):"
	@ grep -w val src/Vector.frag.mli | grep -v synonym | grep -vw check | wc -l

# ------------------------------------------------------------------------------

# [make versions] compiles the package under many versions of OCaml,
# whose list is specified below.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.14.2 \
  5.0.0 \
  5.1.0 \
  5.2.0 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions

.PHONY: handiwork
handiwork:
	@ for v in $(VERSIONS) ; do \
	    opam install --switch $$v monolith ocamlfind cppo core_unix ; \
	  done

# ------------------------------------------------------------------------------

# [make headache] updates the headers.

HEADACHE := headache
HEADER   := header.txt
FIND      = $(shell if command -v gfind >/dev/null ; then echo gfind ; else echo find ; fi)
SOURCES   = $(shell $(FIND) src test benchmark -type f -regex ".*\.\(mli?\|c\)" \
                    | grep -v dynarray)

.PHONY: headache
headache:
	@ for f in $(SOURCES) ; do \
	  $(HEADACHE) -c headache.config -h $(HEADER) $$f ; \
	done

# -------------------------------------------------------------------------

# Publishing a release.

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make uninstall
	@ make clean
	@ make install
# Check the current package description.
	@ opam lint
# Make sure a CHANGES entry with the current date seems to exist.
	@ if ! grep $(DATE_WITH_SLASHES) CHANGES.md ; then \
	    echo "Error: CHANGES.md has no entry with date $(DATE_WITH_SLASHES)." ; \
	    exit 1 ; \
	  fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Upload. (This automatically makes a .tar.gz archive available on gitlab.)
	@ git push
	@ git push --tags
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  \"make publish\"   to publish a new opam package"
	@ echo "  \"make export\"    to upload the documentation to yquem.inria.fr"

.PHONY: publish
publish:
# Publish an opam description.
	@ opam publish -v $(DATE) $(THIS) $(ARCHIVE) .

.PHONY: undo
undo:
# Undo the last release (assuming it was done on the same date).
	@ git tag -d $(DATE)
	@ git push -u origin :$(DATE)
