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

```nim
import vcard

# Reading in an existing vcard
let vcards = parseVCard3File("jack.vcf")
assert vcards.len == 1
let vcAllen = vcards[0]

assert vcAllen.email.len == 2
assert vcAllen.email[0].value == "allen@fosters.test"
assert vcAllen.n.first == "Jack"


# Creating a new VCard
var vcSusan: VCard3
vcSusan.add(
  newVC3_N(given = "Susan", family = "Foster"),
  newVC3_Email(value = "susan@fosters.test", emailType = @["PREF", $etInternet),
  newVC3_Tel(
    value = "+1 (555) 444-3889",
    telType = @[$ttHome, $ttCell, $ttVoice, $ttMsg])
)
writeFile("susan.vcf", $vcSusan)
```

## Future Goals

* VCard 4.0 support

## Debugging

*Need to clean up and organize*

Run `tlexer` tests in gdb:

```sh
$ cd tests
$ nim --debuginfo --linedir:on c tlexer
$ gdb --tui tlexer
