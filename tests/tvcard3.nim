import options, unittest, vcard3, zero_functional

suite "vcard/vcard3":

  let testVCard =
    "BEGIN:VCARD\r\n" &
    "VERSION:3.0\r\n" &
    "FN:Mr. John Q. Public\\, Esq.\r\n" &
    "N:Public;John;Quinlan;Mr.;Esq.\r\n" &
    "END:VCARD\r\n"

  test "minimal VCard":
    let vc = parseVCard3(testVCard)[0]
    check:
      vc.n.family[0] == "Public"
      vc.n.given[0] == "John"
      vc.fn.value == "Mr. John Q. Public\\, Esq."

  test "serialize minimal VCard":
    let vc = parseVCard3(testVCard)[0]
    check $vc == testVCard

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

  test "Jonathan Bernard VCard":
    #const jdbVcard = readFile("tests/jdb.vcf")
    let jdb = parseVCard3File("tests/jdb.vcf")[0]
    check:
      jdb.email.len == 7
      jdb.email[0].value == "jonathan@jdbernard.com"
      jdb.fn.value == "Jonathan Bernard"
