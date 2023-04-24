.PHONY.: test
test:
	nimble test

.PHONY.: build
build: test
	nimble build

.PHONY.: install
install: test
	nimble install

diagrams: doc/vcard3.mmd doc/vcard4.mmd
	mmdc -i doc/vcard3.mmd -o doc/vcard3.png
	mmdc -i doc/vcard4.mmd -o doc/vcard4.png
