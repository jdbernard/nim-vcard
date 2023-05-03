# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SOURCES=$(call rwildcard,src/,*.nim)
TEST_SOURCES=$(wildcard tests/*.nim)
TESTS=$(patsubst %.nim,bin/%,$(TEST_SOURCES))

.PHONY: build
build: test docs

doc/vcard/vcard.html: $(SOURCES)
	nim doc --project --outdir:doc/vcard src/vcard.nim

.PHONY: doc
docs: doc/vcard/vcard.html

.PHONY: test
test:
	#@for t in $(TESTS); do $$t; done
	nimble --warning:BareExcept:off test

.PHONY: install
install: test
	nimble install

diagrams: doc/vcard3.mmd
	mmdc -i doc/vcard3.mmd -o doc/vcard3.png

# Target allowing for running individual tests.
bin/tests/%: tests/%.nim $(SOURCES)
	nim --outdir:bin/tests --hints:off --warning:BareExcept:off c -r $(patsubst bin/%,%.nim,$@)
