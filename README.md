# VCard

`nim-vcard` is a pure nim implementation of the VCard format defined in RFCs
2425, 2426, and 6350. It allows you to parse and serialize VCards, as well as
create VCards programmatically. It aims to be a complete implememtation,
supporting all of the features of the VCard3 standard. Because the standard
provides many features that may be rarely used, this library also provides a
simplified API for more typical use-cases.

## Example Usage

```vcard
BEGIN:VCARD
VERSION:3.0
UID: 5db6f100-e2d6-4e8d-951f-d920586bc069
N:Foster;Jack;Allen;;
FN:Allen Foster
REV:20230408T122102Z
EMAIL;TYPE=home;TYPE=pref:allen@fosters.test
EMAIL;TYPE=work:jack.foster@company.test
TEL;TYPE=CELL:+1 (555) 123-4567
END:VCARD
```

https://github.com/jdbernard/nim-vcard/blob/4839ff64a8e6da1ad4803adbd71c0a53cae81c4e/examples/simple.nim#L1-L22

## Future Goals

* VCard 4.0 support

## Debugging

*Need to clean up and organize*

Run `tvcard3` tests in gdb:

```sh
$ cd tests
$ nim --debuginfo --linedir:on c tvcard3
$ gdb --tui tvcard3
