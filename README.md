## Debugging

*Need to clean up and organize*

Run `tlexer` tests in gdb:

```sh
$ cd tests
$ nim --debuginfo --linedir:on c tlexer
$ gdb --tui tlexer
