# Make does not offer a recursive wildcard function, so here's one:
 rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SOURCES=$(call rwildcard,src/,*.nim)
TEST_SOURCES=$(wildcard tests/*.nim)
TESTS=$(patsubst %.nim,bin/%,$(TEST_SOURCES))

.PHONY: test
test: $(TESTS) $(SOURCES)
	@for t in $(TESTS); do $$t; done

.PHONY: watch-tests
watch-tests:
	watch 'make test' src tests --wait=10

.PHONY: build
build: test
	nimble build

.PHONY: install
install: test
	nimble install

diagrams: doc/vcard3.mmd doc/vcard4.mmd
	mmdc -i doc/vcard3.mmd -o doc/vcard3.png
	mmdc -i doc/vcard4.mmd -o doc/vcard4.png

bin/tests/%: tests/%.nim $(SOURCES)
	nim --outdir:bin/tests c $(patsubst bin/%,%.nim,$@)
