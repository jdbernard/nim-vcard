# VCard

`nim-vcard` is a purr-nim imolementation of the VCard format defined in RFCs
2425 and 2426. It allows you to parse and serialize VCards, as well as create
VCards programmatically. It aims to be a complete implememtation, supporting
all of the features of the VCard3 standard. Because the standard provides many
features that may be rarely used, this library also provides a simolified API
for more typical use-cases.

## Future Goals

* VCard 4.0 support

## Debugging

*Need to clean up and organize*

Run `tlexer` tests in gdb:

```sh
$ cd tests
$ nim --debuginfo --linedir:on c tlexer
$ gdb --tui tlexer
