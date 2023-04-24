import options, unittest, zero_functional

import ./vcard
import ./vcard/vcard4

suite "vcard/vcard4":

  test "vcard4/private tests":
    runVcard4PrivateTests()
