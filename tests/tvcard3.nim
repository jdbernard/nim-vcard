import std/[options, strutils, times, unittest]
import zero_functional

import vcard
import vcard/vcard3

template vcard3Doc(lines: varargs[string]): string =
  "BEGIN:VCARD\r\n" &
  lines.join("\r\n") &
  "\r\nEND:VCARD\r\n"

proc parseSingleVCard3(content: string): VCard3 =
  cast[VCard3](parseVCards(content)[0])

proc newMinimalVCard3(): VCard3 =
  result = VCard3(parsedVersion: VCardV3)
  result.add(newVC3_Fn("John Smith"))
  result.add(newVC3_N(family = @["Smith"], given = @["John"]))

suite "vcard/vcard3":

  test "vcard3/private tests":
    runVcard3PrivateTests()

  let jdbVCard = readFile("tests/jdb.vcf")

  test "parseVCard3":
    check parseVCards(jdbVCard).len == 1

  test "parseVCard3File":
    check parseVCardsFromFile("tests/jdb.vcf").len == 1

  # TODO: remove cast after finishing VCard4 implementation
  let jdb = cast[VCard3](parseVCards(jdbVCard)[0])

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

    let vcards = parseVCards(vcardsStr)
    check:
      vcards.len == 2
      cast[VCard3](vcards[0]).fn.value == "Frank Dawson"
      cast[VCard3](vcards[0]).email.len == 2
      (cast[VCard3](vcards[0]).email --> find(it.emailType.contains("PREF"))).isSome

  test "spec: parser rejects cards missing VERSION":
    expect(VCardParsingError):
      discard parseVCards(vcard3Doc(
        "FN:John Smith",
        "N:Smith;John;;;"))

  test "spec: parser rejects cards missing FN":
    expect(VCardParsingError):
      discard parseVCards(vcard3Doc(
        "VERSION:3.0",
        "N:Smith;John;;;"))

  test "spec: parser rejects cards missing N":
    expect(VCardParsingError):
      discard parseVCards(vcard3Doc(
        "VERSION:3.0",
        "FN:John Smith"))

  test "spec: simple text values decode RFC 2426 escapes when parsing":
    let parsed = parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      r"FN:Jane\, Smith\; Esq.\\Office\nSecond line",
      "N:Smith;Jane;;;"))

    check parsed.fn.value == "Jane, Smith; Esq.\\Office\nSecond line"

  test "spec: simple text values escape special characters when serializing":
    let vc = newMinimalVCard3()
    vc.set(newVC3_Fn("Jane, Smith; Esq.\\Office\nSecond line"))

    check ($vc).contains(r"FN:Jane\, Smith\; Esq.\\Office\nSecond line")

  test "spec: structured text values escape special characters when serializing":
    let vc = VCard3(parsedVersion: VCardV3)
    vc.add(newVC3_Fn("John Smith"))
    vc.add(newVC3_N(
      family = @["Smith, Sr."],
      given = @["John;Jack"],
      additional = @["Back\\Slash"],
      prefixes = @["Dr.\nProf."],
      suffixes = @["III"]))

    check ($vc).contains(r"N:Smith\, Sr.;John\;Jack;Back\\Slash;Dr.\nProf.;III")

  test "spec: list text values escape special characters when serializing":
    let vc = newMinimalVCard3()
    vc.add(newVC3_Categories(@["alpha,beta", "semi;colon", "slash\\value"]))

    check ($vc).contains(r"CATEGORIES:alpha\,beta,semi\;colon,slash\\value")

  test "spec: inline binary values round-trip without double encoding":
    let payload = "aGVsbG8="
    let serialized = $parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      "FN:John Smith",
      "N:Smith;John;;;",
      "PHOTO;ENCODING=b;TYPE=JPEG:" & payload,
      "LOGO;ENCODING=b;TYPE=GIF:" & payload,
      "SOUND;ENCODING=b;TYPE=WAVE:" & payload,
      "KEY;ENCODING=b;TYPE=PGP:" & payload))

    check:
      serialized.contains("PHOTO;ENCODING=b;TYPE=JPEG:" & payload)
      serialized.contains("LOGO;ENCODING=b;TYPE=GIF:" & payload)
      serialized.contains("SOUND;ENCODING=b;TYPE=WAVE:" & payload)
      serialized.contains("KEY;ENCODING=b;TYPE=PGP:" & payload)

  test "spec: uri-backed binary properties round-trip as uris":
    let serialized = $parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      "FN:John Smith",
      "N:Smith;John;;;",
      "PHOTO;VALUE=uri:http://example.test/photo.jpg",
      "LOGO;VALUE=uri:http://example.test/logo.gif",
      "SOUND;VALUE=uri:http://example.test/sound.wav",
      "KEY;VALUE=uri:http://example.test/key.asc"))

    check:
      serialized.contains("PHOTO;VALUE=uri:http://example.test/photo.jpg")
      serialized.contains("LOGO;VALUE=uri:http://example.test/logo.gif")
      serialized.contains("SOUND;VALUE=uri:http://example.test/sound.wav")
      serialized.contains("KEY;VALUE=uri:http://example.test/key.asc")

  test "spec: quoted parameter values are accepted":
    let parsed = parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      "FN;LANGUAGE=\"en\":John Smith",
      "N:Smith;John;;;"))

    check parsed.fn.language == some("en")

  test "spec: PROFILE is exposed as the standard property type":
    let parsed = parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      "PROFILE:VCARD",
      "FN:John Smith",
      "N:Smith;John;;;"))

    check parsed.profile.isSome

  test "spec: AGENT uri values survive parse and serialize":
    let serialized = $parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      "FN:John Smith",
      "N:Smith;John;;;",
      "AGENT;VALUE=uri:mailto:assistant@example.com"))

    check serialized.contains("AGENT;VALUE=uri:mailto:assistant@example.com")

  test "spec: folded lines may continue with horizontal tab":
    let parsed = parseSingleVCard3(
      "BEGIN:VCARD\r\n" &
      "VERSION:3.0\r\n" &
      "FN:John Smith\r\n" &
      "N:Smith;John;;;\r\n" &
      "NOTE:one two \r\n" &
      "\tthree four\r\n" &
      "END:VCARD\r\n")

    check:
      parsed.note.len == 1
      parsed.note[0].value == "one two three four"

  test "spec: group names may contain hyphens":
    let parsed = parseSingleVCard3(vcard3Doc(
      "VERSION:3.0",
      "FN:John Smith",
      "N:Smith;John;;;",
      "item-1.EMAIL:test@example.com"))

    check:
      parsed.email.len == 1
      parsed.email[0].group == some("item-1")

  test "spec: REV with VALUE=date serializes as a date":
    let vc = newMinimalVCard3()
    vc.add(newVC3_Rev(
      value = parse("2000-01-02", "yyyy-MM-dd"),
      valueType = some("date")))

    check ($vc).contains("REV;VALUE=date:2000-01-02")

  test "spec: KEY defaults to text rather than uri":
    let vc = newMinimalVCard3()
    vc.add(newVC3_Key("ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC"))

    check:
      ($vc).contains("KEY:ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC")
      not ($vc).contains("KEY;VALUE=uri:")
