import std/[options, sequtils, strutils, tables, times, unittest]
import zero_functional

import vcard
import vcard/vcard4

template vcard4Doc(lines: varargs[string]): string =
  "BEGIN:VCARD\r\n" &
  lines.join("\r\n") &
  "\r\nEND:VCARD\r\n"

proc parseSingleVCard4(content: string): VCard4 =
  cast[VCard4](parseVCards(content)[0])

suite "vcard/vcard4":

  test "vcard4/private tests":
    runVcard4PrivateTests()

  let v4ExampleStr = readFile("tests/allen.foster.v4.vcf")

  let testVCardTemplate =
    "BEGIN:VCARD\r\n" &
    "VERSION:4.0\r\n" &
    "$#" &
    "END:VCARD\r\n"

  test "parseVCard4":
    check parseVCards(v4ExampleStr).len == 1

  test "parseVCard4File":
    check parseVCardsFromFile("tests/allen.foster.v4.vcf").len == 1

  # TODO: remove cast after finishing VCard4 implementation
  let v4Ex = cast[VCard4](parseVCards(v4ExampleStr)[0])

  test "RFC 6350 author's VCard":
    let vcardStr =
      "BEGIN:VCARD\r\n" &
      "VERSION:4.0\r\n" &
      "FN:Simon Perreault\r\n" &
      "N:Perreault;Simon;;;ing. jr,M.Sc.\r\n" &
      "BDAY:--0203\r\n" &
      "ANNIVERSARY:20090808T1430-0500\r\n" &
      "GENDER:M\r\n" &
      "LANG;PREF=1:fr\r\n" &
      "LANG;PREF=2:en\r\n" &
      "ORG;TYPE=work:Viagenie\r\n" &
      "ADR;TYPE=work:;Suite D2-630;2875 Laurier;\r\n" &
      " Quebec;QC;G1V 2M2;Canada\r\n" &
      "TEL;VALUE=uri;TYPE=\"work,voice\";PREF=1:tel:+1-418-656-9254;ext=102\r\n" &
      "TEL;VALUE=uri;TYPE=\"work,cell,voice,video,text\":tel:+1-418-262-6501\r\n" &
      "EMAIL;TYPE=work:simon.perreault@viagenie.ca\r\n" &
      "GEO;TYPE=work:geo:46.772673,-71.282945\r\n" &
      "KEY;TYPE=work;VALUE=uri:\r\n" &
      " http://www.viagenie.ca/simon.perreault/simon.asc\r\n" &
      "TZ:-0500\r\n" &
      "URL;TYPE=home:http://nomis80.org\r\n" &
      "END:VCARD\r\n"

    let vcards = parseVCards(vcardStr)
    check vcards.len == 1
    let sp = cast[VCard4](vcards[0])
    check:
      sp.fn.len == 1
      sp.fn[0].value == "Simon Perreault"
      sp.gender.isSome
      sp.gender.get.sex == some(VC4_Sex.Male)
      sp.gender.get.genderIdentity.isNone
      sp.lang.len == 2
      sp.lang --> map(it.value) == @["fr", "en"]

  test "custom properties are serialized":
    let email = newVC4_Email(
      value ="john.smith@testco.test",
      types = @["work", "internet"],
      params = @[("PREF", @["1"]), ("X-ATTACHMENT-LIMIT", @["25MB"])])

    let serialized = serialize(email)
    check:
      serialized.startsWith("EMAIL;")
      serialized.endsWith(":john.smith@testco.test")
      serialized.contains(";PREF=1")
      serialized.contains(";TYPE=work,internet")
      serialized.contains(";X-ATTACHMENT-LIMIT=25MB")
      serialized.count(';') == 3

  test "spec: text-or-uri constructors serialize non-empty values":
    check:
      serialize(newVC4_Tel(value = "tel:+1-555-555-5555")) ==
        "TEL:tel:+1-555-555-5555"
      serialize(newVC4_Related(value = "urn:uuid:person-1")) ==
        "RELATED:urn:uuid:person-1"
      serialize(newVC4_Uid(value = "urn:uuid:card-1")) ==
        "UID:urn:uuid:card-1"
      serialize(newVC4_Key(value = "https://example.com/keys/john.asc")) ==
        "KEY:https://example.com/keys/john.asc"

  test "spec: text-list properties serialize parameters exactly once":
    let nickname = newVC4_Nickname(value = @["Doc"], types = @["work"])
    check serialize(nickname) == "NICKNAME;TYPE=work:Doc"

  test "spec: handwritten serializers preserve group prefixes":
    let rev = newVC4_Rev(value = now(), group = some("item1"))
    check:
      serialize(newVC4_N(family = @["Doe"], group = some("item1"))) ==
        "item1.N:Doe;;;;"
      serialize(newVC4_Adr(street = "Main", group = some("item1"))) ==
        "item1.ADR:;;Main;;;;"
      serialize(newVC4_Gender(sex = some(VC4_Sex.Male), group = some("item1"))) ==
        "item1.GENDER:M"
      serialize(newVC4_ClientPidMap(
        id = 1,
        uri = "urn:uuid:client-map",
        group = some("item1"))) ==
          "item1.CLIENTPIDMAP:1;urn:uuid:client-map"
      serialize(rev).startsWith("item1.REV:")

  test "spec: text-valued BDAY and ANNIVERSARY serialize with VALUE=text":
    check:
      serialize(newVC4_Bday(value = "circa 1800", valueType = some("text"))) ==
        "BDAY;VALUE=text:circa 1800"
      serialize(newVC4_Anniversary(value = "childhood", valueType = some("text"))) ==
        "ANNIVERSARY;VALUE=text:childhood"

  test "spec: URI properties round-trip MEDIATYPE":
    let photo = newVC4_Photo(
      value = "https://example.com/photo.jpg",
      mediaType = some("image/jpeg"))
    check serialize(photo) == "PHOTO;MEDIATYPE=image/jpeg:https://example.com/photo.jpg"

    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "PHOTO;MEDIATYPE=image/jpeg:https://example.com/photo.jpg"))
    check:
      parsed.photo.len == 1
      parsed.photo[0].mediaType == some("image/jpeg")

  test "spec: typed PID accessors expose parsed PID values":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "EMAIL;PID=1.7:test@example.com",
      "CLIENTPIDMAP:7;urn:uuid:device-7"))
    check:
      parsed.email.len == 1
      parsed.email[0].pid == @[PidValue(propertyId: 1, sourceId: 7)]

  test "spec: ADR supports structured list components":
    check compiles(newVC4_Adr(street = @["123 Main St", "Unit 5"]))

    when compiles(newVC4_Adr(street = @["123 Main St", "Unit 5"])):
      let adr = newVC4_Adr(
        street = @["123 Main St", "Unit 5"],
        locality = @["Springfield"],
        region = @["IL"],
        postalCode = @["01111"],
        country = @["USA"])

      check serialize(adr) == "ADR:;;123 Main St,Unit 5;Springfield;IL;01111;USA"

      let parsed = parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "ADR:;;123 Main St,Unit 5;Springfield;IL;01111;USA"))
      check:
        parsed.adr.len == 1
        parsed.adr[0].street == @["123 Main St", "Unit 5"]
        parsed.adr[0].locality == @["Springfield"]
        parsed.adr[0].region == @["IL"]
        parsed.adr[0].postalCode == @["01111"]
        parsed.adr[0].country == @["USA"]
        serialize(parsed.adr[0]) == "ADR:;;123 Main St,Unit 5;Springfield;IL;01111;USA"

  test "spec: ADR escapes special characters in component values":
    let adr = newVC4_Adr(
      poBox = "Box, 7",
      ext = "Suite; 9",
      street = "123 Main St",
      locality = "Montreal\nWest",
      region = "QC\\CA",
      postalCode = "H2Y 1C6",
      country = "Canada")
    check:
      serialize(adr) ==
        r"ADR:Box\, 7;Suite\; 9;123 Main St;Montreal\nWest;QC\\CA;H2Y 1C6;Canada"

  test "spec: ADR constructors serialize GEO, TZ, and LABEL parameters":
    let adr = newVC4_Adr(
      street = "123 Main St",
      geo = some("geo:46.772673,-71.282945"),
      label = some("123 Main St., Suite 100"),
      tz = some("America/Chicago"))
    let serialized = serialize(adr)
    check:
      serialized.startsWith("ADR;")
      serialized.contains("GEO=\"geo:46.772673,-71.282945\"")
      serialized.contains("LABEL=\"123 Main St., Suite 100\"")
      serialized.contains("TZ=America/Chicago")
      serialized.endsWith(":;;123 Main St;;;;")

  test "spec: ADR exposes GEO, TZ, and LABEL through typed accessors":
    check compiles((block:
      let parsed = parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "ADR;GEO=\"geo:46.772673,-71.282945\";" &
          "LABEL=\"123 Main St., Suite 100\";TZ=America/Chicago:" &
          ";;123 Main St;Springfield;IL;01111;USA"))
      parsed.adr[0].geo))

    when compiles((block:
        let parsed = parseSingleVCard4(vcard4Doc(
          "VERSION:4.0",
          "FN:John Smith",
          "ADR;GEO=\"geo:46.772673,-71.282945\";" &
            "LABEL=\"123 Main St., Suite 100\";TZ=America/Chicago:" &
            ";;123 Main St;Springfield;IL;01111;USA"))
        parsed.adr[0].geo)):
      let parsed = parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "ADR;GEO=\"geo:46.772673,-71.282945\";" &
          "LABEL=\"123 Main St., Suite 100\";TZ=America/Chicago:" &
          ";;123 Main St;Springfield;IL;01111;USA"))
      check:
        parsed.adr.len == 1
        parsed.adr[0].geo == some("geo:46.772673,-71.282945")
        parsed.adr[0].label == some("123 Main St., Suite 100")
        parsed.adr[0].tz == some("America/Chicago")
        serialize(parsed.adr[0]) ==
          "ADR;GEO=\"geo:46.772673,-71.282945\";" &
          "LABEL=\"123 Main St., Suite 100\";TZ=America/Chicago:" &
          ";;123 Main St;Springfield;IL;01111;USA"

  test "spec: ORG supports multiple organization units":
    check compiles(newVC4_Org(
      value = @["ABC, Inc.", "North American Division", "Marketing"]))

    when compiles(newVC4_Org(
        value = @["ABC, Inc.", "North American Division", "Marketing"])):
      let org = newVC4_Org(
        value = @["ABC, Inc.", "North American Division", "Marketing"])
      check serialize(org) == "ORG:ABC\\, Inc.;North American Division;Marketing"

      let parsed = parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "ORG:ABC\\, Inc.;North American Division;Marketing"))
      check:
        parsed.org.len == 1
        parsed.org[0].value == @["ABC, Inc.", "North American Division", "Marketing"]
        serialize(parsed.org[0]) == "ORG:ABC\\, Inc.;North American Division;Marketing"

  test "spec: ORG round-trips structured input without escaping separators":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "ORG:ABC\\, Inc.;North American Division;Marketing"))
    check serialize(parsed.org[0]) == "ORG:ABC\\, Inc.;North American Division;Marketing"

  test "spec: N and ORG support SORT-AS through the typed API":
    check:
      compiles(newVC4_N(
        family = @["van der Harten"],
        given = @["Rene"],
        sortAs = @["Harten", "Rene"]))
      compiles(newVC4_Org(
        value = @["ABC, Inc.", "Marketing"],
        sortAs = @["ABC Inc.", "Marketing"]))

    when compiles(newVC4_N(
        family = @["van der Harten"],
        given = @["Rene"],
        sortAs = @["Harten", "Rene"])) and
         compiles(newVC4_Org(
           value = @["ABC, Inc.", "Marketing"],
           sortAs = @["ABC Inc.", "Marketing"])):
      check:
        serialize(newVC4_N(
          family = @["van der Harten"],
          given = @["Rene"],
          sortAs = @["Harten", "Rene"])) ==
            "N;SORT-AS=Harten,Rene:van der Harten;Rene;;;"
        serialize(newVC4_Org(
          value = @["ABC, Inc.", "Marketing"],
          sortAs = @["ABC Inc.", "Marketing"])) ==
            "ORG;SORT-AS=ABC Inc.,Marketing:ABC\\, Inc.;Marketing"

      let parsed = parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:Rene van der Harten",
        "N;SORT-AS=Harten,Rene:van der Harten;Rene;;;",
        "ORG;SORT-AS=ABC Inc.,Marketing:ABC\\, Inc.;Marketing"))
      check:
        parsed.n.isSome
        parsed.n.get.sortAs == @["Harten", "Rene"]
        parsed.org.len == 1
        parsed.org[0].sortAs == @["ABC Inc.", "Marketing"]

  test "spec: BDAY and ANNIVERSARY support CALSCALE through the typed API":
    check:
      compiles(newVC4_Bday(value = "19960415", calscale = some("gregorian")))
      compiles(newVC4_Anniversary(value = "20140612", calscale = some("gregorian")))

    when compiles(newVC4_Bday(value = "19960415", calscale = some("gregorian"))) and
         compiles(newVC4_Anniversary(value = "20140612", calscale = some("gregorian"))):
      check:
        serialize(newVC4_Bday(value = "19960415", calscale = some("gregorian"))) ==
          "BDAY;CALSCALE=gregorian:19960415"
        serialize(newVC4_Anniversary(
          value = "20140612",
          calscale = some("gregorian"))) ==
            "ANNIVERSARY;CALSCALE=gregorian:20140612"

      let parsed = parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "BDAY;CALSCALE=gregorian:19960415",
        "ANNIVERSARY;CALSCALE=gregorian:20140612"))
      check:
        parsed.bday.isSome
        parsed.bday.get.calscale == some("gregorian")
        parsed.anniversary.isSome
        parsed.anniversary.get.calscale == some("gregorian")

  test "spec: unsupported standard parameters are rejected on known properties":
    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN;SORT-AS=Smith:John Smith"))

    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "EMAIL;LABEL=Inbox:test@example.com"))

    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "ORG;CALSCALE=gregorian:Example Corp"))

    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "CLIENTPIDMAP;PID=1.1:1;urn:uuid:client-map"))

  test "spec: CALSCALE is rejected when BDAY or ANNIVERSARY use VALUE=text":
    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "BDAY;VALUE=text;CALSCALE=gregorian:circa 1800"))

    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "ANNIVERSARY;VALUE=text;CALSCALE=gregorian:childhood"))

  test "spec: MEMBER requires KIND=group":
    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "MEMBER:urn:uuid:person-1"))

    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "KIND:group",
      "FN:The Doe Family",
      "MEMBER:urn:uuid:person-1"))
    check parsed.member.len == 1

  test "spec: PID identifiers require positive values and matching CLIENTPIDMAP":
    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "EMAIL;PID=1.1:test@example.com"))

    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "EMAIL;PID=0.1:test@example.com",
        "CLIENTPIDMAP:1;urn:uuid:device-1"))

    expect(VCardParsingError):
      discard parseSingleVCard4(vcard4Doc(
        "VERSION:4.0",
        "FN:John Smith",
        "EMAIL;PID=1.0:test@example.com",
        "CLIENTPIDMAP:0;urn:uuid:device-1"))

    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "EMAIL;PID=1.1:test@example.com",
      "CLIENTPIDMAP:1;urn:uuid:device-1"))
    check:
      parsed.email.len == 1
      parsed.clientpidmap.len == 1
      parsed.email[0].pid == @[PidValue(propertyId: 1, sourceId: 1)]

  test "can parse properties with escaped characters":
    check v4Ex.note.len == 1
    let note = v4Ex.note[0]

    check note.value ==
      "This is an example, for clarity; in text value cases the parser " &
      "will recognize escape values for ',', '\\', and newlines. For " &
      "example:" &
      "\n\t123 Flagstaff Road" &
      "\n\tPlaceville, MA"

  test "can parse parameters with escaped characters":
    let prop = v4Ex.customProp("X-CUSTOM-EXAMPLE")[0]
    check prop.value ==
      "This is an example, for clarity; in straight value cases, the parser " &
      "does not recognize any escape values, as the meaning of the content " &
      "is implementation-specific."
    let param1 = prop.params --> filter(it.name == "PARAM")
    let label = prop.params --> filter(it.name == "LABEL")
    check:
      param1.len == 1
      param1[0].values == @["How one says, \"Hello.\""]
      label.len == 1
      label[0].values == @["^top\nsecond line"]

  test "spec: RFC 6868 unknown escapes pass through in unquoted parameter values":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN;X-TEST=alpha^xbeta:John Smith"))
    let param = parsed.fn[0].params --> find(it.name == "X-TEST")
    check:
      param.isSome
      param.get.values == @["alpha^xbeta"]

  test "spec: RFC 6868 unknown escapes pass through in quoted parameter values":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN;X-TEST=\"alpha^xbeta\":John Smith"))
    let param = parsed.fn[0].params --> find(it.name == "X-TEST")
    check:
      param.isSome
      param.get.values == @["alpha^xbeta"]

  test "Data URIs are parsed correctly":
    let expectedB64 = readFile("tests/allen.foster.jpg.uri")

    check:
      v4Ex.photo.len == 2
      v4Ex.photo[0].altId == some("1")
      v4Ex.photo[0].value ==
        "https://tile.loc.gov/storage-services/service/pnp/bellcm/02200/02297r.jpg"
      v4Ex.photo[0].valueType == some("uri")
      v4Ex.photo[1].altId == some("1")
      v4Ex.photo[1].value == expectedB64
      v4Ex.photo[1].valueType.isNone

  test "URI-type properties are parsed correctly":
    # Covers SOURCE, PHOTO, IMPP, GEO, LOGO, MEMBER, SOUND, URL, FBURL,
    # CALADRURI, and CALURI
    check:
      v4Ex.source.len == 1
      v4Ex.source[0].value == "https://carddav.fosters.test/allen.vcf"
      v4Ex.source[0].valueType == some("uri")
      v4Ex.url.len == 1
      v4Ex.url[0].value == "https://allen.fosters.test/"

  test "URI-type properties are serialized correctly":
    # Covers SOURCE, PHOTO, IMPP, GEO, LOGO, MEMBER, SOUND, URL, FBURL,
    # CALADRURI, and CALURI
    let src = newVC4_Source(value="https://carddav.example.test/john-smith.vcf")
    check serialize(src) == "SOURCE:https://carddav.example.test/john-smith.vcf"

  test "spec: LANG supports explicit VALUE=language-tag":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "LANG;VALUE=language-tag;PREF=1:en-US"))
    check:
      parsed.lang.len == 1
      parsed.lang[0].value == "en-US"
      parsed.lang[0].valueType == some("language-tag")
      serialize(parsed.lang[0]) == "LANG;VALUE=language-tag;PREF=1:en-US"

  test "spec: TZ supports VALUE=utc-offset":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "TZ;VALUE=utc-offset:-0500"))
    check:
      parsed.tz.len == 1
      parsed.tz[0].value == "-0500"
      parsed.tz[0].valueType == some("utc-offset")
      serialize(parsed.tz[0]) == "TZ;VALUE=utc-offset:-0500"

  test "spec: TZ supports VALUE=uri":
    let parsed = parseSingleVCard4(vcard4Doc(
      "VERSION:4.0",
      "FN:John Smith",
      "TZ;VALUE=uri:https://example.com/tz/America-Chicago"))
    check:
      parsed.tz.len == 1
      parsed.tz[0].value == "https://example.com/tz/America-Chicago"
      parsed.tz[0].valueType == some("uri")
      serialize(parsed.tz[0]) == "TZ;VALUE=uri:https://example.com/tz/America-Chicago"

  test "Single-text properties are parsed correctly":
    # Covers KIND, XML, FN, NICKNAME, EMAIL, LANG, TZ, TITLE, ROLE, ORG, NOTE,
    # PRODID, and VERSION
    check:
      v4Ex.kind.isSome
      v4Ex.kind.get.value == "individual"
      v4Ex.nickname.len == 2
      v4Ex.nickname[0].value == @["Jack Jr."]
      v4Ex.nickname[1].value == @["Doc A"]
      v4Ex.fn.len == 1
      v4Ex.fn[0].value == "Dr. Allen Foster"
      v4Ex.email.len == 2
      v4Ex.email[0].value == "jack.foster@company.test"
      v4Ex.email[0].types == @["work"]

  test "URI or Text properties are parsed correctly":
    # Covers TEL, RELATED, UID, KEY
    check:
      v4Ex.tel.len == 3
      v4ex.tel[0].types == @[$VC4_TelType.ttCell]
      v4Ex.tel[0].value == "+1 555-123-4567"
      v4Ex.tel[2].types == @[$VC4_TelType.ttWork,$VC4_TelType.ttVoice]
      v4Ex.tel[2].valueType == some($vtUri)
      v4Ex.tel[2].value == "tel:+1-555-874-1234"

  test "N is parsed correctly":
    check:
      v4Ex.n.isSome
      v4Ex.n.get.given == @["Jack"]
      v4Ex.n.get.family == @["Foster"]
      v4Ex.n.get.additional == @["John", "Allen"]
      v4Ex.n.get.prefixes == @["Dr."]
      v4Ex.n.get.suffixes == @["II"]

  test "BDAY is parsed correctly":
    check:
      v4Ex.bday.isSome
      v4Ex.bday.get.value == "--1224"
      v4Ex.bday.get.year.isNone
      v4Ex.bday.get.month == some(12)
      v4Ex.bday.get.day == some(24)

  test "ANNIVERSARY is parsed correctly":
    check:
      v4Ex.anniversary.isSome
      v4Ex.anniversary.get.value == "20140612T163000-0500"
      v4Ex.anniversary.get.year == some(2014)
      v4Ex.anniversary.get.hour == some(16)
      v4Ex.anniversary.get.minute == some(30)
      v4Ex.anniversary.get.timezone == some("-0500")

  test "GENDER is parsed correctly":
    check:
      v4Ex.gender.isSome
      v4Ex.gender.get.sex == some(VC4_Sex.Male)
      v4Ex.gender.get.genderIdentity == some("male")

#[
  test "CATEGORIES is parsed correctly":
  test "REV is parsed correctly":
  test "CLIENTPIDMAP is parsed correctly":
]#

  test "unknown properties are parsed correctly":

    check v4Ex.customProp("MADE-UP-PROP").len == 1
    let madeUpProp = v4Ex.customProp("MADE-UP-PROP")[0]
    check:
      madeUpProp.name == "MADE-UP-PROP"
      madeUpProp.value == "Sample value for my made-up prop."

  let cardWithAltBdayStr = testVCardTemplate % [(
    "FN:Simon Perreault\r\n" &
    "BDAY;VALUE=text;ALTID=1:20th century\r\n" &
    "BDAY;VALUE=date-and-or-time;ALTID=1:19650321\r\n"
  )]

  test "single-cardinality properties allow multiples with ALTID":
    check parseVCards(cardWithAltBdayStr).len == 1

  test "single-cardinality properties reject multiples without ALTID":
    expect(VCardParsingError):
      discard parseVCards(testVCardTemplate % [(
        "FN:Simon Perreault\r\n" &
        "BDAY;VALUE=text:20th century\r\n" &
        "BDAY;VALUE=date-and-or-time:19650321\r\n"
      )])

  let hasAltBdays = cast[VCard4](parseVCards(cardWithAltBdayStr)[0])

  test "properties with cardinality 1 and altids return the first found by default":
    check:
      hasAltBdays.bday.isSome
      hasAltBdays.bday.get.value == "20th century"
      hasAltBdays.bday.get.year.isNone

  test "allAlternatives":
    check:
      hasAltBdays.content.len == 3
      hasAltBdays.bday.isSome
      hasAltBdays.content.countIt(it of VC4_Version) == 0

    let allBdays = allAlternatives[VC4_Bday](hasAltBdays)
    check:
      allBdays.len == 1
      allBdays.contains("1")
      allBdays["1"].len == 2

    let bday0 = allBdays["1"][0]
    check:
      bday0.value == "20th century"
      bday0.year.isNone
      bday0.month.isNone
      bday0.day.isNone
      bday0.hour.isNone
      bday0.minute.isNone
      bday0.second.isNone
      bday0.timezone.isNone

    let bday1 = allBDays["1"][1]
    check:
      bday1.value == "19650321"
      bday1.year == some(1965)
      bday1.month == some(3)
      bday1.day == some(21)
      bday1.hour.isNone
      bday1.minute.isNone
      bday1.second.isNone

  test "PREF ordering":
    check:
      v4Ex.nickname --> map(it.value) == @[@["Jack Jr."], @["Doc A"]]
      v4Ex.nickname.inPrefOrder --> map(it.value) == @[@["Doc A"], @["Jack Jr."]]
