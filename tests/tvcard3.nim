import options, unittest, zero_functional

import ./vcard

suite "vcard/vcard3":

  test "vcard3/private tests":
    runVcard3PrivateTests()

  let jdbVCard = readFile("tests/jdb.vcf")
  let jdb = parseVCard3(jdbVCard)[0]

  test "parseVCard3":
    check:
      jdb.n.family == @["Bernard"]
      jdb.n.given == @["Jonathan"]
      jdb.fn.value == "Jonathan Bernard"

  test "parseVCard3File":
    let jdb = parseVCard3File("tests/jdb.vcf")[0]
    check:
      jdb.email.len == 7
      jdb.email[0].value == "jonathan@jdbernard.com"
      jdb.email[0].emailType.contains("pref")
      jdb.fn.value == "Jonathan Bernard"

  test "email is parsed correctly":
    check:
      jdb.email.len == 7
      jdb.email[0].value == "jonathan@jdbernard.com"
      jdb.email[0].emailType.contains("pref")
      jdb.email[0].emailType.contains("home")
      jdb.email[1].value == "jdb@jdb-software.com"
      jdb.email[1].emailType.contains("work")
      jdb.email[2].group.isSome
      jdb.email[2].group.get == "email2"
      jdb.email[6].value == "jbernard@vectra.ai"
      jdb.email[6].emailType.contains("work")

  test "tel is parsed correctly":
    check:
      jdb.tel.len == 2
      jdb.tel[0].value == "(512) 777-1602"
      jdb.tel[0].telType.contains("CELL")

  test "RFC2426 Author's VCards":
    let vcardsStr =
      "BEGIN:vCard\r\n" &
      "VERSION:3.0\r\n" &
      "FN:Frank Dawson\r\n" &
      "ORG:Lotus Development Corporation\r\n" &
      "ADR;TYPE=WORK,POSTAL,PARCEL:;;6544 Battleford Drive\r\n" &
      " ;Raleigh;NC;27613-3502;U.S.A.\r\n" &
      "TEL;TYPE=VOICE,MSG,WORK:+1-919-676-9515\r\n" &
      "TEL;TYPE=FAX,WORK:+1-919-676-9564\r\n" &
      "EMAIL;TYPE=INTERNET,PREF:Frank_Dawson@Lotus.com\r\n" &
      "EMAIL;TYPE=INTERNET:fdawson@earthlink.net\r\n" &
      "URL:http://home.earthlink.net/~fdawson\r\n" &
      "END:vCard\r\n" &
      "\r\n" &
      "\r\n" &
      "BEGIN:vCard\r\n" &
      "VERSION:3.0\r\n" &
      "FN:Tim Howes\r\n" &
      "ORG:Netscape Communications Corp.\r\n" &
      "ADR;TYPE=WORK:;;501 E. Middlefield Rd.;Mountain View;\r\n" &
      " CA; 94043;U.S.A.\r\n" &
      "TEL;TYPE=VOICE,MSG,WORK:+1-415-937-3419\r\n" &
      "TEL;TYPE=FAX,WORK:+1-415-528-4164\r\n" &
      "EMAIL;TYPE=INTERNET:howes@netscape.com\r\n" &
      "END:vCard\r\n"

    let vcards = parseVCard3(vcardsStr)
    check:
      vcards.len == 2
      vcards[0].fn.value == "Frank Dawson"
      vcards[0].email.len == 2
      (vcards[0].email --> find(it.emailType.contains("PREF"))).isSome
