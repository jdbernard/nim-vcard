# vCard 3.0 and 4.0 Nm implementation
# © 2022 Jonathan Bernard

## The `vcard` module implements a high-performance vCard parser for both
## versions 3.0 (defined by RFCs [2425][rfc2425] and  [2426][rfc2426]) and 4.0
## (defined by RFC [6350][rfc6350])
##
## [rfc2425]: https://tools.ietf.org/html/rfc2425
## [rfc2426]: https://tools.ietf.org/html/rfc2426
## [rfc6350]: https://tools.ietf.org/html/rfc6350

import std/[base64, macros, options, sequtils, streams, strutils, times,
            unicode]

import zero_functional

import vcard/private/[util, lexer]

type
  VC3_ValueTypes = enum
    vtUri = "URI",
    vtText = "TEXT",
    vtPText = "PTEXT",
    vtDate = "DATE",
    vtTime = "TIME",
    vtDateTime = "DATE-TIME",
    vtInteger = "INTEGER",
    vtBoolean = "BOOLEAN",
    vtFloat = "FLOAT",
    vtBinary = "BINARY",
    vtVCard = "VCARD"
    vtPhoneNumber = "PHONE-NUMBER"
    vtUtcOffset = "UTC-OFFSET"

  VC3_ContentNames = enum
    cnName = "NAME"
    cnProfile = "PROFILE"
    cnSource = "SOURCE"
    cnFn = "FN"
    cnN = "N"
    cnNickname = "NICKNAME"
    cnPhoto = "PHOTO"
    cnBday = "BDAY"
    cnAdr = "ADR"
    cnLabel = "LABEL"
    cnTel = "TEL"
    cnEmail = "EMAIL"
    cnMailer = "MAILER"
    cnTz = "TZ"
    cnGeo = "GEO"
    cnTitle = "TITLE"
    cnRole = "ROLE"
    cnLogo = "LOGO"
    cnAgent = "AGENT"
    cnOrg = "ORG"
    cnCategories = "CATEGORIES"
    cnNote = "NOTE"
    cnProdid = "PRODID"
    cnRev = "REV"
    cnSortString = "SORT-STRING"
    cnSound = "SOUND"
    cnUid = "UID"
    cnUrl = "URL"
    cnVersion = "VERSION"
    cnClass = "CLASS"
    cnKey = "KEY"

  VC3_XParam* = tuple[name, value: string]

  VC3_Content* = ref object of RootObj
    contentId*: int
    group*: Option[string]
    name*: string

  VC3_ContentList* = openarray[VC3_Content]

  VC3_SimpleTextContent* = ref object of VC3_Content
    value*: string
    isPText*: bool # true if VALUE=ptext, false by default
    language*: Option[string]
    xParams: seq[VC3_XParam]

  VC3_BinaryContent* = ref object of VC3_Content
    valueType*: Option[string] # binary / uri. Stored separately from ENCODING
                               # (captured in the isInline field) because the
                               # VALUE parameter is not set by default, but is
                               # allowed to be set.
    value*: string  # either a URI or bit sequence, both stored as string
    binaryType*: Option[string]# if using ENCODING=b, there may also be a TYPE
                               # parameter specifying the MIME type of the
                               # binary-encoded object, which is stored here.
    isInline*: bool # true if ENCODING=b, false by default

  VC3_Name* = ref object of VC3_Content
    value*: string

  VC3_Profile* = ref object of VC3_Content

  VC3_Source* = ref object of VC3_Content
    valueType*: Option[string]  # uri
    value*: string # URI
    context*: Option[string]
    xParams*: seq[VC3_XParam]

  VC3_Fn* = ref object of VC3_SimpleTextContent

  VC3_N* = ref object of VC3_Content
    family*: seq[string]
    given*: seq[string]
    additional*: seq[string]
    prefixes*: seq[string]
    suffixes*: seq[string]
    language*: Option[string]
    isPText*: bool # true if VALUE=ptext, false by default
    xParams*: seq[VC3_XParam]

  VC3_Nickname* = ref object of VC3_SimpleTextContent

  VC3_Photo* = ref object of VC3_BinaryContent

  VC3_Bday* = ref object of VC3_Content
    valueType*: Option[string] # date / date-time
    value*: DateTime

  VC3_AdrTypes* = enum
    # Standard types defined in RFC2426
    atDom = "DOM"
    atIntl = "INTL"
    atPostal = "POSTAL"
    atParcel = "PARCEL"
    atHome = "HOME"
    atWork = "WORK"
    atPref = "PREF"

  VC3_Adr* = ref object of VC3_Content
    adrType*: seq[string]
    poBox*: string
    extendedAdr*: string
    streetAdr*: string
    locality*: string
    region*: string
    postalCode*: string
    country*: string
    isPText*: bool # true if VALUE=ptext, false by default
    language*: Option[string]
    xParams*: seq[VC3_XParam]

  VC3_Label* = ref object of VC3_SimpleTextContent
    adrType*: seq[string]

  VC3_TelTypes* = enum
    ttHome = "HOME",
    ttWork = "WORK",
    ttPref = "PREF",
    ttVoice = "VOICE",
    ttFax = "FAX",
    ttMsg = "MSG",
    ttCell = "CELL",
    ttPager = "PAGER",
    ttBbs = "BBS",
    ttModem = "MODEM",
    ttCar = "CAR",
    ttIsdn = "ISDN",
    ttVideo = "VIDEO",
    ttPcs = "PCS"

  VC3_Tel* = ref object of VC3_Content
    telType*: seq[string]
    value*: string

  VC3_EmailType* = enum
    etInternet = "INTERNET",
    etX400 = "X400"

  VC3_Email* = ref object of VC3_Content
    emailType*: seq[string]
    value*: string

  VC3_Mailer* = ref object of VC3_SimpleTextContent

  VC3_TZ* = ref object of VC3_Content
    value*: string
    isText*: bool # true if VALUE=text, false by default

  VC3_Geo* = ref object of VC3_Content
    lat*, long*: float

  VC3_Title* = ref object of VC3_SimpleTextContent

  VC3_Role* = ref object of VC3_SimpleTextContent

  VC3_Logo* = ref object of VC3_BinaryContent

  VC3_Agent* = ref object of VC3_Content
    value*: string  # either an escaped vCard object, or a URI
    isInline*: bool # false if VALUE=uri, true by default

  VC3_Org* = ref object of VC3_Content
    value*: seq[string]
    isPText*: bool # true if VALUE=ptext, false by default
    language*: Option[string]
    xParams*: seq[VC3_XParam]

  VC3_Categories* = ref object of VC3_Content
    value*: seq[string]
    isPText*: bool # true if VALUE=ptext, false by default
    language*: Option[string]
    xParams*: seq[VC3_XParam]

  VC3_Note* = ref object of VC3_SimpleTextContent

  VC3_Prodid* = ref object of VC3_SimpleTextContent

  VC3_Rev* = ref object of VC3_Content
    valueType*: Option[string] # date / date-time
    value*: DateTime

  VC3_SortString* = ref object of VC3_SimpleTextContent

  VC3_Sound* = ref object of VC3_BinaryContent

  VC3_UID* = ref object of VC3_Content
    value*: string

  VC3_URL* = ref object of VC3_Content
    value*: string

  VC3_Version* = ref object of VC3_Content
    value*: string # 3.0

  VC3_Class* = ref object of VC3_Content
    value*: string

  VC3_Key* = ref object of VC3_BinaryContent
    keyType*: Option[string] # x509 / pgp

  VC3_XType* = ref object of VC3_SimpleTextContent

  VCard3* = object
    nextContentId: int
    content*: seq[VC3_Content]

const CRLF = "\r\n"
const DATE_FMT = "yyyy-MM-dd"
const DATETIME_FMT = "yyyy-MM-dd'T'HH:mm:sszz"

# Internal Utility/Implementation
# =============================================================================

template findAll[T](c: VC3_ContentList): seq[T] =
  c.filterIt(it of typeof(T)).mapIt(cast[T](it))

template findFirst[T](c: VC3_ContentList): Option[T] =
  let found = c.filterIt(it of typeof(T)).mapIt(cast[T](it))
  if found.len > 0: some(found[0])
  else: none[T]()

template takeContentId(vc3: var VCard3): int =
  vc3.nextContentId += 1
  vc3.nextContentId - 1

macro assignFields(assign: untyped, fields: varargs[untyped]): untyped =
  result = assign

  for f in fields:
    let exp = newNimNode(nnkExprColonExpr)
    exp.add(f, f)
    result.add(exp)


# Initializers
# =============================================================================

func newVC3_Name*(value: string, group = none[string]()): VC3_Name =
  return VC3_Name(name: "NAME", value: value, group: group)

func newVC3_Source*(
  value: string,
  inclContext = false,
  inclValue = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Source =

  return assignFields(
    VC3_Source(
      name: "SOURCE",
      valueType: if inclValue: some("uri")
                 else: none[string](),
      context: if inclContext: some("word")
               else: none[string]()),
    value, group, xParams)

func newVC3_Fn*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Fn =

  return assignFields(
    VC3_Fn(name: "FN"),
    value, language, isPText, group, xParams)

func newVC3_N*(
  family: seq[string] = @[],
  given: seq[string] = @[],
  additional: seq[string] = @[],
  prefixes: seq[string] = @[],
  suffixes: seq[string] = @[],
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_N =

  return assignFields(
    VC3_N(name: "N"),
    family, given, additional, prefixes, suffixes, language, xParams)

func newVC3_Nickname*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Nickname =

  return assignFields(
    VC3_Nickname(name: "NICKNAME"),
    value, language, isPText, group, xParams)

func newVC3_Photo*(
  value: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Photo =

  return assignFields(
    VC3_Photo(name: "PHOTO"),
    value, valueType, binaryType, isInline, group)

func newVC3_Bday*(
  value: DateTime,
  valueType = none[string](),
  group = none[string]()): VC3_Bday =

    return assignFields(VC3_Bday(name: "BDAY"), value, valueType, group)

func newVC3_Adr*(
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  poBox = "",
  extendedAdr = "",
  streetAdr = "",
  locality = "",
  region = "",
  postalCode = "",
  country = "",
  language = none[string](),
  isPText = false,
  group = none[string](),
  xParams: seq[VC3_XParam] = @[]): VC3_Adr =

  return assignFields(
    VC3_Adr(name: "ADR"),
    adrType, poBox, extendedAdr, streetAdr, locality, region, postalCode,
    country, isPText, language, group, xParams)

func newVC3_Label*(
  value: string,
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Label =

  return assignFields(
    VC3_Label(name: "LABEL"),
    value, adrType, language, isPText, group, xParams)

func newVC3_Tel*(
  value: string,
  telType = @[$ttVoice],
  group = none[string]()): VC3_Tel =

  return VC3_Tel(name: "TEL", telType: telType, group: group)

func newVC3_Email*(
  value: string,
  emailType = @[$etInternet],
  group = none[string]()): VC3_Email =

  return VC3_Email(name: "EMAIL", emailType: emailType, group: group)

func newVC3_Mailer*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Mailer =

  return assignFields(
    VC3_Mailer(name: "MAILER"),
    value, language, isPText, xParams, group)

func newVC3_TZ*(value: string, isText = false, group = none[string]()): VC3_TZ =
  return assignFields(VC3_TZ(name: "TZ"), value, isText, group)

func newVC3_Geo*(lat, long: float, group = none[string]()): VC3_Geo =
  return assignFields(VC3_Geo(name: "GEO"), lat, long, group)

func newVC3_Title*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Title =

  return assignFields(
    VC3_Title(name: "TITLE"),
    value, language, isPText, xParams, group)

func newVC3_Role*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Role =

  return assignFields(
    VC3_Role(name: "ROLE"),
    value, language, isPText, xParams, group)

func newVC3_Logo*(
  value: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Logo =

  return assignFields(
    VC3_Logo(name: "LOGO"),
    value, valueType, binaryType, isInline, group)

func newVC3_Agent*(
  value: string,
  isInline = true,
  group = none[string]()): VC3_Agent =

  return VC3_Agent(name: "AGENT", isInline: isInline, group: group)

func newVC3_Org*(
  value: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Org =

  return assignFields(
    VC3_Org(name: "ORG"),
    value, isPText, language, xParams, group)

func newVC3_Categories*(
  value: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Categories =

  return assignFields(
    VC3_Categories(name: "CATEGORIES"),
    value, isPText, language, xParams, group)

func newVC3_Note*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Note =

  return assignFields(
    VC3_Note(name: "NOTE"),
    value, language, isPText, xParams, group)

func newVC3_Prodid*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Prodid =

  return assignFields(
    VC3_Prodid(name: "PRODID"),
    value, language, isPText, xParams, group)

func newVC3_Rev*(
  value: DateTime,
  valueType = none[string](),
  group = none[string]()): VC3_Rev =

  return assignFields(VC3_Rev(name: "REV"), value, valueType, group)

func newVC3_SortString*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_SortString =

  return assignFields(
    VC3_SortString(name: "SORTSTRING"),
    value, language, isPText, xParams, group)

func newVC3_Sound*(
  value: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Sound =

  return assignFields(
    VC3_Sound(name: "SOUND"),
    value, valueType, binaryType, isInline, group)

func newVC3_UID*(value: string, group = none[string]()): VC3_UID =
  return VC3_UID(name: "UID", value: value, group: group)

func newVC3_URL*(value: string, group = none[string]()): VC3_Url =
  return VC3_Url(name: "URL", value: value, group: group)

func newVC3_Version*(group = none[string]()): VC3_Version =
  return VC3_Version(name: "VERSION", value: "3.0", group: group)

func newVC3_Class*(value: string, group = none[string]()): VC3_Class =
  return VC3_Class(name: "CLASS", value: value, group: group)

func newVC3_Key*(
  value: string,
  valueType = some("uri"),
  keyType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Key =

  return assignFields(
    VC3_Key(name: "KEY", binaryType: keyType),
    value, valueType, keyType, isInline, group)

func newVC3_XType*(
  name: string,
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_XType =

  if not name.startsWith("x-"):
    raise newException(ValueError, "Extended types must begin with 'x-'.")

  return assignFields(
    VC3_XType(name: name),
    value, language, isPText, xParams, group)

# Accessors
# =============================================================================

func forGroup*(vc: VC3_ContentList, group: string): seq[VC3_Content] =
  return vc.filterIt(it.group.isSome and it.group.get == group)

func groups*(vc: VC3_ContentList): seq[string] =
  result = @[]
  for c in vc:
    if c.group.isSome:
      let grp = c.group.get
      if not result.contains(grp): result.add(grp)

func name*(c: VC3_ContentList): Option[VC3_Name] = findFirst[VC3_Name](c)
func name*(vc3: VCard3): Option[VC3_Name] = vc3.content.name

func profile*(c: VC3_ContentList): Option[VC3_Profile] =
  findFirst[VC3_Profile](c)
func profile*(vc3: VCard3): Option[VC3_Profile] = vc3.content.profile

func source*(c: VC3_ContentList): seq[VC3_Source] = findAll[VC3_Source](c)
func source*(vc3: VCard3): seq[VC3_Source] = vc3.content.source

func fn*(c: VC3_ContentList): VC3_Fn = findFirst[VC3_Fn](c).get
func fn*(vc3: VCard3): VC3_Fn = vc3.content.fn

func n*(c: VC3_ContentList): VC3_N = findFirst[VC3_N](c).get
func n*(vc3: VCard3): VC3_N = vc3.content.n

func nickname*(c: VC3_ContentList): Option[VC3_Nickname] = findFirst[VC3_Nickname](c)
func nickname*(vc3: VCard3): Option[VC3_Nickname] = vc3.content.nickname

func photo*(c: VC3_ContentList): seq[VC3_Photo] = findAll[VC3_Photo](c)
func photo*(vc3: VCard3): seq[VC3_Photo] = vc3.content.photo

func bday*(c: VC3_ContentList): Option[VC3_Bday] = findFirst[VC3_Bday](c)
func bday*(vc3: VCard3): Option[VC3_Bday] = vc3.content.bday

func adr*(c: VC3_ContentList): seq[VC3_Adr] = findAll[VC3_Adr](c)
func adr*(vc3: VCard3): seq[VC3_Adr] = vc3.content.adr

func label*(c: VC3_ContentList): seq[VC3_Label] = findAll[VC3_Label](c)
func label*(vc3: VCard3): seq[VC3_Label] = vc3.content.label

func tel*(c: VC3_ContentList): seq[VC3_Tel] = findAll[VC3_Tel](c)
func tel*(vc3: VCard3): seq[VC3_Tel] = vc3.content.tel

func email*(c: VC3_ContentList): seq[VC3_Email] = findAll[VC3_Email](c)
func email*(vc3: VCard3): seq[VC3_Email] = vc3.content.email

func mailer*(c: VC3_ContentList): Option[VC3_Mailer] = findFirst[VC3_Mailer](c)
func mailer*(vc3: VCard3): Option[VC3_Mailer] = vc3.content.mailer

func tz*(c: VC3_ContentList): Option[VC3_Tz] = findFirst[VC3_Tz](c)
func tz*(vc3: VCard3): Option[VC3_Tz] = vc3.content.tz

func geo*(c: VC3_ContentList): Option[VC3_Geo] = findFirst[VC3_Geo](c)
func geo*(vc3: VCard3): Option[VC3_Geo] = vc3.content.geo

func title*(c: VC3_ContentList): seq[VC3_Title] = findAll[VC3_Title](c)
func title*(vc3: VCard3): seq[VC3_Title] = vc3.content.title

func role*(c: VC3_ContentList): seq[VC3_Role] = findAll[VC3_Role](c)
func role*(vc3: VCard3): seq[VC3_Role] = vc3.content.role

func logo*(c: VC3_ContentList): seq[VC3_Logo] = findAll[VC3_Logo](c)
func logo*(vc3: VCard3): seq[VC3_Logo] = vc3.content.logo

func agent*(c: VC3_ContentList): Option[VC3_Agent] = findFirst[VC3_Agent](c)
func agent*(vc3: VCard3): Option[VC3_Agent] = vc3.content.agent

func org*(c: VC3_ContentList): seq[VC3_Org] = findAll[VC3_Org](c)
func org*(vc3: VCard3): seq[VC3_Org] = vc3.content.org

func categories*(c: VC3_ContentList): Option[VC3_Categories] =
  findFirst[VC3_Categories](c)
func categories*(vc3: VCard3): Option[VC3_Categories] = vc3.content.categories

func note*(c: VC3_ContentList): Option[VC3_Note] = findFirst[VC3_Note](c)
func note*(vc3: VCard3): Option[VC3_Note] = vc3.content.note

func prodid*(c: VC3_ContentList): Option[VC3_Prodid] = findFirst[VC3_Prodid](c)
func prodid*(vc3: VCard3): Option[VC3_Prodid] = vc3.content.prodid

func rev*(c: VC3_ContentList): Option[VC3_Rev] = findFirst[VC3_Rev](c)
func rev*(vc3: VCard3): Option[VC3_Rev] = vc3.content.rev

func sortstring*(c: VC3_ContentList): Option[VC3_SortString] =
  findFirst[VC3_SortString](c)
func sortstring*(vc3: VCard3): Option[VC3_SortString] = vc3.content.sortstring

func sound*(c: VC3_ContentList): seq[VC3_Sound] = findAll[VC3_Sound](c)
func sound*(vc3: VCard3): seq[VC3_Sound] = vc3.content.sound

func uid*(c: VC3_ContentList): Option[VC3_Uid] = findFirst[VC3_Uid](c)
func uid*(vc3: VCard3): Option[VC3_Uid] = vc3.content.uid

func url*(c: VC3_ContentList): Option[VC3_Url] = findFirst[VC3_Url](c)
func url*(vc3: VCard3): Option[VC3_Url] = vc3.content.url

func version*(c: VC3_ContentList): VC3_Version =
  let found = findFirst[VC3_Version](c)
  if found.isSome: return found.get
  else: return VC3_Version(
    contentId: c.len + 1,
    group: none[string](),
    name: "VERSION",
    value: "3.0")
func version*(vc3: VCard3): VC3_Version = vc3.content.version

func class*(c: VC3_ContentList): Option[VC3_Class] = findFirst[VC3_Class](c)
func class*(vc3: VCard3): Option[VC3_Class] = vc3.content.class

func key*(c: VC3_ContentList): seq[VC3_Key] = findAll[VC3_Key](c)
func key*(vc3: VCard3): seq[VC3_Key] = vc3.content.key

func xTypes*(c: VC3_ContentList): seq[VC3_XType] = findAll[VC3_XType](c)
func xTypes*(vc3: VCard3): seq[VC3_XType] = vc3.content.xTypes

# Setters
# =============================================================================

func set*[T](vc3: var VCard3, newContent: var T): void =
  let existingIdx = vc3.content.indexOfIt(it of T)
  if existingIdx < 0:
    newContent.contentId = vc3.takeContentId
    vc3.content.add(newContent)
  else:
    newContent.contentId = vc3.content[existingIdx].contentId
    vc3.content[existingIdx] = newContent

func set*[T](vc3: VCard3, newContent: var T): VCard3 =
  result = vc3
  result.set(newContent)

func add*[T](vc3: var VCard3, newContent: T): void =
  newContent.contentId = vc3.takeContentId
  vc3.content.add(newContent)

func add*[T](vc3: VCard3, newContent: T): VCard3 =
  result = vc3
  result.add(newContent)

func updateOrAdd*[T](vc3: var VCard3, content: seq[T]): VCard3 =
  for c in content:
    let existingIdx = vc3.content.indexOfIt(it.contentId == c.contentId)
    if existingIdx < 0: vc3.content.add(c)
    else: c.content[existingIdx] = c

# TODO: simplify with macros?
# macro generateImmutableVersion()...
# generateImmutableVersion("set", "add", "setName", "addSource")

#[
func setName*(vc3: var VCard3, name: string, group = none[string]()): void =
  var name = newVC3_Name(name, group)
  vc3.set(name)

func setName*(vc3: VCard3, name: string, group = none[string]()): VCard3 =
  result = vc3
  result.setName(name, group)

func addSource*(
  vc3: var VCard3,
  source: string,
  context = none[string](),
  setValue = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Source(source, context, setValue, xParams, group)
  vc3.add(c)

func addSource*(
  vc3: VCard3,
  source: string,
  context = none[string](),
  setValue = false,
  xParams: seq[VC3_XParam] = @[]): VCard3 =

  result = vc3
  result.addSource(source, context, setValue, xParams)

func setFn*(
  vc3: var VCard3,
  fn: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_FN(fn, language, isPText, xParams, group)
  vc3.set(c)

func setFn*(
  vc3: VCard3,
  fn: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.setFn(fn, language, isPText, xParams, group)


func addFn*(
  vc3: var VCard3,
  fn: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_FN(fn, language, isPText, xParams, group)
  vc3.add(c)

func addFn*(
  vc3: VCard3,
  fn: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.addFn(fn, language, isPText, xParams, group)

func setN*(
  vc3: var VCard3,
  family: seq[string] = @[],
  given: seq[string] = @[],
  additional: seq[string] = @[],
  prefixes: seq[string] = @[],
  suffixes: seq[string] = @[],
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_N(family, given, additional, prefixes, suffixes, language,
    isPText, xParams, group)
  vc3.set(c)

func setN*(
  vc3: VCard3,
  family: seq[string] = @[],
  given: seq[string] = @[],
  additional: seq[string] = @[],
  prefixes: seq[string] = @[],
  suffixes: seq[string] = @[],
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.setN(family, given, additional, prefixes, suffixes, language, isPText,
    xParams, group)

func addNickname*(
  vc3: var VCard3,
  nickname: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Nickname(nickname, language, isPText, xParams, group)
  vc3.add(c)

func addNickname*(
  vc3: VCard3,
  nickname: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.addNickname(nickname, language, isPText, xParams, group)

func addPhoto*(
  vc3: var VCard3,
  photo: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): void =

  var c = newVC3_Photo(photo, valueType, binaryType, isInline, group)
  vc3.add(c)

func addPhoto*(
  vc3: VCard3,
  photo: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VCard3 =

  result = vc3
  result.addPhoto(photo, valueType, binaryType, isInline, group)

func setBday*(
  vc3: var VCard3,
  bday: DateTime,
  valueType = none[string](),
  group = none[string]()): void =

  var c = newVC3_Bday(bday, valueType, group)
  vc3.set(c)

func setBday*(
  vc3: VCard3,
  bday: DateTime,
  valueType = none[string](),
  group = none[string]()): VCard3 =

  result = vc3
  result.setBday(bday, valueType, group)

func addAdr*(
  vc3: var VCard3,
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  poBox = "",
  extendedAdr = "",
  streetAdr = "",
  locality = "",
  region = "",
  postalCode = "",
  country = "",
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[]): void =

  var c = newVC3_Adr(adrType, poBox, extendedAdr, streetAdr, locality, region,
    postalCode, country, language, isPText, xParams)
  vc3.add(c)

func addAdr*(
  vc3: VCard3,
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  poBox = "",
  extendedAdr = "",
  streetAdr = "",
  locality = "",
  region = "",
  postalCode = "",
  country = "",
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[]): VCard3 =

  result = vc3
  result.addAdr(adrType, poBox, extendedAdr, streetAdr, locality, region,
    postalCode, country, language, isPText, xParams)

func addLabel*(
  vc3: var VCard3,
  label: string,
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Label(label, adrType, language, isPText, xParams, group)
  vc3.add(c)

func addLabel*(
  vc3: VCard3,
  label: string,
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.addLabel(label, adrType, language, isPText, xParams, group)

func addTel*(
  vc3: var VCard3,
  tel: string,
  telType = @[$ttVoice],
  group = none[string]()): void =

  var c = newVC3_Tel(tel, telType, group)
  vc3.add(c)

func addTel*(
  vc3: VCard3,
  tel: string,
  telType = @[$ttVoice],
  group = none[string]()): VCard3 =

  result = vc3
  result.addTel(tel, telType, group)

func addEmail*(
  vc3: var VCard3,
  email: string,
  emailType = @[$etInternet],
  group = none[string]()): void =

  var c = newVC3_Email(email, emailType, group)
  vc3.add(c)

func addEmail*(
  vc3: VCard3,
  email: string,
  emailType = @[$etInternet],
  group = none[string]()): VCard3 =

  result = vc3
  result.addEmail(email, emailType, group)

func setMailer*(
  vc3: var VCard3,
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Mailer(value, language, isPText, xParams, group)
  vc3.set(c)

func setMailer*(
  vc3: VCard3,
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.setMailer(value, language, isPText, xParams, group)

func setTZ*(
  vc3: var VCard3,
  value: string,
  isText = false,
  group = none[string]()): void =

  var c = newVC3_TZ(value, isText, group)
  vc3.set(c)

func setTZ*(
  vc3: VCard3,
  value: string,
  isText = false,
  group = none[string]()): VCard3 =

  result = vc3
  result.setTZ(value, isText, group)

func setGeo*(
  vc3: var VCard3,
  lat, long: float,
  group = none[string]()): void =

  var c = newVC3_Geo(lat, long, group)
  vc3.set(c)

func setGeo*(
  vc3: VCard3,
  lat, long: float,
  group = none[string]()): VCard3 =

  result = vc3
  result.setGeo(lat, long, group)

func addTitle*(
  vc3: var VCard3,
  title: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Title(title, language, isPText, xParams, group)
  vc3.add(c)

func addTitle*(
  vc3: VCard3,
  title: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.addTitle(title, language, isPText, xParams, group)

func addRole*(
  vc3: var VCard3,
  role: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Role(role, language, isPText, xParams, group)
  vc3.add(c)

func addRole*(
  vc3: VCard3,
  role: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.addRole(role, language, isPText, xParams, group)

func addLogo*(
  vc3: var VCard3,
  logo: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): void =

  var c = newVC3_Logo(logo, valueType, binaryType, isInline, group)
  vc3.add(c)

func addLogo*(
  vc3: VCard3,
  logo: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VCard3 =

  result = vc3
  result.addLogo(logo, valueType, binaryType, isInline, group)

func setAgent*(
  vc3: var VCard3,
  agent: string,
  isInline = true,
  group = none[string]()): void =

  var c = newVC3_Agent(agent, isInline, group)
  vc3.add(c)

func setAgent*(
  vc3: VCard3,
  agent: string,
  isInline = true,
  group = none[string]()): VCard3 =

  result = vc3
  result.setAgent(agent, isInline, group)

func setOrg*(
  vc3: var VCard3,
  org: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Org(org, isPText, language, xParams, group)
  vc3.set(c)

func setOrg*(
  vc3: VCard3,
  org: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.setOrg(org, isPText, language, xParams, group)

func setCategories*(
  vc3: var VCard3,
  categories: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): void =

  var c = newVC3_Categories(categories, isPText, language, xParams, group)
  vc3.set(c)

func setCategories*(
  vc3: VCard3,
  categories: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  result = vc3
  result.setCategories(categories, isPText, language, xParams, group)

func addNote(
  vc3: VCard3,
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VCard3 =

  var c = newVC3_Note(value, language, isPText, xParams, group)
  vc3.add(c)
]#
#[
# TODO
note
prodid
rev
sortstring
sound
uid
url
version
class
key
]#

# Output
# =============================================================================

func nameWithGroup(s: VC3_Content): string =
  if s.group.isSome: s.group.get & "." & s.name
  else: s.name

func serialize(s: seq[VC3_XParam]): string =
  result = ""
  for x in s: result &= ";" & x.name & "=" & x.value

func serialize(s: VC3_Source): string =
  result = s.nameWithGroup
  if s.valueType.isSome: result &= ";VALUE=" & s.valueType.get
  if s.context.isSome: result &= ";CONTEXT=" & s.context.get
  result &= serialize(s.xParams)
  result &= ":" & s.value

func serialize(n: VC3_N): string =
  result = n.nameWithGroup
  if n.isPText: result &= ";VALUE=ptext"
  if n.language.isSome: result &= ";LANGUAGE=" & n.language.get
  result &= serialize(n.xParams)
  result &= ":" &
    n.family.join(",") & ";" &
    n.given.join(",") & ";" &
    n.additional.join(",") & ";" &
    n.prefixes.join(",") & ";" &
    n.suffixes.join(",")

func serialize(b: VC3_Bday): string =
  result = b.nameWithGroup
  if b.valueType.isSome and b.valueType.get == "date-time":
    result &= ";VALUE=date-time:" & b.value.format(DATETIME_FMT)
  else:
    result &= ";VALUE=date:" & b.value.format(DATE_FMT)

func serialize(a: VC3_Adr): string =
  result = a.nameWithGroup
  if a.adrType.len > 0: result &= ";TYPE=" & a.adrType.join(",")
  if a.isPText: result &= ";VALUE=ptext"
  if a.language.isSome: result &= ";LANGUAGE=" & a.language.get
  result &= serialize(a.xParams)
  result &= ":" &
    a.poBox & ";" &
    a.extendedAdr & ";" &
    a.streetAdr & ";" &
    a.locality & ";" &
    a.region & ";" &
    a.postalCode & ";" &
    a.country

proc serialize(t: VC3_Tel): string =
  result = t.nameWithGroup
  if t.telType.len > 0: result &= ";TYPE=" & t.telType.join(",")
  result &= ":" & t.value

proc serialize(t: VC3_Email): string =
  result = t.nameWithGroup
  if t.emailType.len > 0: result &= ";TYPE=" & t.emailType.join(",")
  result &= ":" & t.value

func serialize(s: VC3_SimpleTextContent): string =
  result = s.nameWithGroup
  if s.isPText: result &= ";VALUE=ptext"
  if s.language.isSome: result &= ";LANGUAGE=" & s.language.get
  result &= serialize(s.xParams)
  result &= ":" & s.value

proc serialize(b: VC3_BinaryContent): string =
  result = b.nameWithGroup
  if b.valueType.isSome: result &= ";VALUE=" & b.valueType.get
  if b.isInline: result &= ";ENCODING=b"
  if b.binaryType.isSome: result &= ";TYPE=" & b.binaryType.get
  result &= ":"
  if b.isInline: result &= base64.encode(b.value)
  else: result &= b.value

proc serialize(z: VC3_TZ): string =
  result = z.nameWithGroup
  if z.isText: result &= ";VALUE=text"
  result &= ":" & z.value

proc serialize(g: VC3_Geo): string =
  result = g.nameWithGroup & ":" & $g.lat & ";" & $g.long

proc serialize(a: VC3_Agent): string =
  result = a.nameWithGroup
  if not a.isInline: result &= ";VALUE=uri"
  result &= ":" & a.value

proc serialize(o: VC3_Org): string =
  result = o.nameWithGroup
  if o.isPText: result &= ";VALUE=ptext"
  if o.language.isSome: result &= ";LANGUAGE=" & o.language.get
  result &= serialize(o.xParams)
  result &= ":" & o.value.join(",")

proc serialize(c: VC3_Categories): string =
  result = c.nameWithGroup
  if c.isPText: result &= ";VALUE=ptext"
  if c.language.isSome: result &= ";LANGUAGE=" & c.language.get
  result &= serialize(c.xParams)
  result &= ":" & c.value.join(",")

proc serialize(r: VC3_Rev): string =
  result = r.nameWithGroup
  if r.valueType.isSome and r.valueType.get == "date-time":
    result &= ";VALUE=date-time:" & r.value.format(DATETIME_FMT)
  else:
    result &= ";VALUE=date:" & r.value.format(DATE_FMT)

proc serialize(u: VC3_UID | VC3_URL | VC3_VERSION | VC3_Class): string =
  result = u.nameWithGroup & ":" & u.value

proc serialize(c: VC3_Content): string =
  if c of VC3_Name: return c.nameWithGroup & ":" & cast[VC3_Name](c).value
  elif c of VC3_Profile: return c.nameWithGroup & ":VCARD"
  elif c of VC3_Source: return serialize(cast[VC3_Source](c))
  elif c of VC3_N: return serialize(cast[VC3_N](c))
  elif c of VC3_Bday: return serialize(cast[VC3_Bday](c))
  elif c of VC3_Adr: return serialize(cast[VC3_Adr](c))
  elif c of VC3_Tel: return serialize(cast[VC3_Tel](c))
  elif c of VC3_Email: return serialize(cast[VC3_Email](c))
  elif c of VC3_TZ: return serialize(cast[VC3_TZ](c))
  elif c of VC3_Geo: return serialize(cast[VC3_Geo](c))
  elif c of VC3_Agent: return serialize(cast[VC3_Agent](c))
  elif c of VC3_Org: return serialize(cast[VC3_Org](c))
  elif c of VC3_Categories: return serialize(cast[VC3_Categories](c))
  elif c of VC3_Rev: return serialize(cast[VC3_Rev](c))
  elif c of VC3_UID: return serialize(cast[VC3_UID](c))
  elif c of VC3_URL: return serialize(cast[VC3_URL](c))
  elif c of VC3_Version: return serialize(cast[VC3_Version](c))
  elif c of VC3_Class: return serialize(cast[VC3_Class](c))
  elif c of VC3_SimpleTextContent:
    return serialize(cast[VC3_SimpleTextContent](c))
  elif c of VC3_BinaryContent:
    return serialize(cast[VC3_BinaryContent](c))

proc `$`*(vc3: VCard3): string =
  result = "BEGIN:VCARD" & CRLF
  result &= "VERSION:3.0" & CRLF
  for c in vc3.content.filterIt(not (it of VC3_Version)):
    result &= foldContentLine(serialize(c)) & CRLF
  result &= "END:VCARD" & CRLF


# Parsing
# =============================================================================
import vcard/private/parsercommon

type
  VC3Parser = object of VCardLexer
    filename: string

  VC3Param = object
    name*: string
    values*: seq[string]
    isPText*: bool

  VCard3ParsingError = object of ValueError

const NON_ASCII = { '\x80'..'\xFF' }
const SAFE_CHARS = WSP + { '\x21', '\x23'..'\x2B', '\x2D'..'\x39', '\x3C'..'\x7E' } + NON_ASCII
const QSAFE_CHARS = WSP + { '\x21', '\x23'..'\x7E' } + NON_ASCII
const VALUE_CHAR = WSP + { '\x21'..'\x7E' } + NON_ASCII

proc error(p: VC3Parser, msg: string) =
  raise newException(VCard3ParsingError, "$1($2, $3) Error: $4] " %
    [ p.filename, $p.lineNumber, $p.getColNumber(p.pos), msg ])

proc readName(p: var VC3Parser): string =
  ## Read a name from the current read position or error. As both content types
  ## and paramaters use the same definition for valid names, this method is
  ## used to read in both.
  p.setBookmark
  let validChars = ALPHA_NUM + {'-'}
  while validChars.contains(p.peek): discard p.read
  result = p.readSinceBookmark.toUpper()
  if result.len == 0:
    p.error("expected to read a name but found '$1'" % [$p.peek])
  p.unsetBookmark

proc readParamValue(p: var VC3Parser): string =
  ## Read a single parameter value at the current read position or error.
  p.setBookmark
  if p.peek == '"':
    while QSAFE_CHARS.contains(p.peek): discard p.read
    if p.read != '"':
      p.error("quoted parameter value expected to end with a " &
        "double quote (\")")
    result = p.readSinceBookmark[0 ..< ^1]
  else:
    while SAFE_CHARS.contains(p.peek): discard p.read
    result = p.readSinceBookmark

  p.unsetBookmark

  if result.len == 0:
    p.error("expected to read a parameter value")

proc readParams(p: var VC3Parser): seq[VC3Param] =
  ## Read all parameters for the current content line at the current read head.
  result = @[]
  while p.peek == ';':
    discard p.read
    var param = VC3Param(name: p.readName, values: @[])
    p.expect("=", true)
    param.values.add(p.readParamValue)
    while p.peek == ',':
      discard p.read
      param.values.add(p.readParamValue)
    result.add(param)

proc readValue(p: var VC3Parser): string =
  ## Read a content value at the current read position.
  p.setBookmark
  while VALUE_CHAR.contains(p.peek): discard p.read
  result = p.readSinceBookmark
  p.unsetBookmark

proc readTextValue(p: var VC3Parser, ignorePrefix: set[char] = {}): string =
  ## Read a text-value (defined by RFC2426) from the current read position.
  ## text-value is a more constrained definition of possible value characters
  ## used in content types like N and ADR.

  result = newStringOfCap(32)

  let validChars = SAFE_CHARS + {'"', ':', '\\'}
  while ignorePrefix.contains(p.peek): discard p.read
  while validChars.contains(p.peek):
    let c = p.read

    if c == '\\':
      case p.peek
      of '\\', ';', ',': result.add(p.read)
      of 'n', 'N':
        result.add('\n')
        discard p.read
      else:
        p.error("invalid character escape: '\\$1'" % [$p.read])
    else: result.add(c)

proc readTextValueList(
    p: var VC3Parser,
    seps: set[char] = {','},
    ifPrefix = none[char]()
  ): seq[string] =
  ## Read in a list of multiple text-value (defined by RFC2426) values from the
  ## current read position. This is used, for example, but the N content type.

  if ifPrefix.isSome:
    if p.peek != ifPrefix.get: return @[]
    discard p.read

  result = @[p.readTextValue]
  while seps.contains(p.peek): result.add(p.readTextValue(ignorePrefix = seps))

proc skip(p: var VC3Parser, expected: string, caseSensitive = false): bool =
  p.setBookmark
  if caseSensitive:
    for ch in expected:
      if p.read != ch:
        p.returnToBookmark
        return false

  else:
    for rune in expected.runes:
      if p.readRune.toLower != rune.toLower:
        p.returnToBookmark
        return false

  p.unsetBookmark
  return true

proc existsWithValue(
    params: openarray[VC3Param],
    name, value: string,
    caseSensitive = false
  ): bool =

  ## Determine if the given parameter exists and has the expected value. By
  ## default, value checks are not case-sensitive, as most VCard values are not
  ## defined as being case-sensitive.

  let ps = params.toSeq

  if caseSensitive:
    ps --> exists(
      it.name == name and
      it.values.len == 1 and
      it.values[0] == value)
  else:
    ps --> exists(
      it.name == name and
      it.values.len == 1 and
      it.values[0].toLower == value.toLower)

proc getMultipleValues(
    params: openarray[VC3Param],
    name: string
  ): seq[string] =

  ## Get all of the values for a given parameter in a single list. There are
  ## two patterns for multi-valued parameters defined in the VCard RFCs:
  ##
  ##   - TYPE=work,cell,voice
  ##   - TYPE=work;TYPE=cell;TYPE=voice
  ##
  ## Parameter values can often be specific using both patterns. This method
  ## joins all defined values regardless of the pattern used to define them.

  let ps = params.toSeq
  ps -->
    filter(it.name == name).
    map(it.values).
    flatten()

proc getSingleValue(params: openarray[VC3Param], name: string): Option[string] =
  ## Get the first single value defined for a parameter.
  #
  # Many parameters only support a single value, depending on the content type.
  # In order to support multi-valued parameters our implementation stores all
  # parameters as seq[string]. This function is a convenience around that.

  let ps = params.toSeq
  let foundParam = ps --> find(it.name == name)

  if foundParam.isSome and foundParam.get.values.len > 0:
    return some(foundParam.get.values[0])
  else:
    return none[string]()

proc validateNoParameters(
    p: VC3Parser,
    params: openarray[VC3Param],
    name: string
  ) =

  ## Error unless there are no defined parameters
  if params.len > 0:
    p.error("no parameters allowed on the $1 content type" % [name])

proc validateRequiredParameters(
    p: VC3Parser,
    params: openarray[VC3Param],
    expectations: openarray[tuple[name: string, value: string]]
  ) =

  ## Some content types have specific allowed parameters. For example, the
  ## SOURCE content type requires that the VALUE parameter be set to "uri" if
  ## it is present. This will error if given parameters are present with
  ## different values that expected.

  for (n, v) in expectations:
    let pv = params.getSingleValue(n)
    if pv.isSome and pv.get != v:
      p.error("parameter '$1' must have the value '$2'" % [n, v])

proc getXParams(params: openarray[VC3Param]): seq[VC3_XParam] =
  ## Filter out and return only the non-standard parameters starting with "x-"

  let ps = params.toSeq
  return ps -->
    filter(it.name.startsWith("x-")).
    map((name: it.name, value: it.values.join(",")))

proc parseContentLines(p: var VC3Parser): seq[VC3_Content] =
  result = @[]

  macro assignCommon(assign: untyped): untyped =
    result = assign
    result.add(newTree(nnkExprEqExpr, ident("group"), ident("group")))

    result.add(newTree(nnkExprEqExpr,
      ident("language"),
      newCall(ident("getSingleValue"),
        ident("params"),
        newStrLitNode("LANGUAGE"))))

    result.add(newTree(nnkExprEqExpr,
      ident("isPText"),
      newCall(ident("existsWithValue"),
        ident("params"),
        newStrLitNode("VALUE"),
        newTree(nnkPrefix, ident("$"), ident("vtPText")))))

    result.add(newTree(nnkExprEqExpr,
      ident("xParams"),
      newCall(ident("getXParams"), ident("params"))))

  while true:
    let group = p.readGroup
    let name = p.readName
    if name == "END":
      p.expect(":VCARD\r\n")
      break
    let params = p.readParams
    p.expect(":")

    case name

    of $cnName:
      p.validateNoParameters(params, "NAME")
      result.add(newVC3_Name(p.readValue, group))

    of $cnProfile:
      if p.readValue.toUpper != "VCARD":
        p.error("the value of the PROFILE content type must be \"$1\"" %
          ["vcard"])
      p.validateNoParameters(params, "NAME")
      result.add(VC3_Content(group: group, name: name))

    of $cnSource:
      p.validateRequiredParameters(params,
        [("CONTEXT", "word"), ("VALUE", "uri")])

      result.add(newVC3_Source(
        group = group,
        value = p.readValue,
        inclContext = params.existsWithValue("CONTEXT", "WORD"),
        inclValue = params.existsWithValue("VALUE", $vtUri),
        xParams = params.getXParams))

    of $cnFn:
      result.add(assignCommon(newVC3_Fn(value = p.readValue)))

    of $cnN:
      result.add(assignCommon(newVC3_N(
        family = p.readTextValueList,
        given = p.readTextValueList(ifPrefix = some(';')),
        additional = p.readTextValueList(ifPrefix = some(';')),
        prefixes = p.readTextValueList(ifPrefix = some(';')),
        suffixes = p.readTextValueList(ifPrefix = some(';')))))

    of $cnNickname:
      result.add(assignCommon(newVC3_Nickname(value = p.readValue)))

    of $cnPhoto:
      result.add(newVC3_Photo(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        binaryType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    of $cnBday:
      let valueType = params.getSingleValue("VALUE")
      let valueStr = p.readValue
      var value: DateTime

      try:
        if valueType.isSome and valueType.get == $vtDate:
          value = parseDate(valueStr)
        elif valueType.isSome and valueType.get == $vtDateTime:
          value = parseDateTime(valueStr)
        elif valueType.isSome:
          p.error("invalid VALUE for BDAY content. " &
                  "Expected '" & $vtDate & "' or '" & $vtDateTime & "'")
        else:
          value = parseDateOrDateTime(valueStr)
      except ValueError:
        p.error("invalid date or date-time value: $1" % [valueStr])

      result.add(newVC3_Bday(
        group = group,
        valueType = valueType,
        value = value))

    of $cnAdr:
      result.add(assignCommon(newVC3_Adr(
        adrType = params.getMultipleValues("TYPE"),
        poBox = p.readTextValue,
        extendedAdr = p.readTextValue(ignorePrefix = {';'}),
        streetAdr = p.readTextValue(ignorePrefix = {';'}),
        locality = p.readTextValue(ignorePrefix = {';'}),
        region = p.readTextValue(ignorePrefix = {';'}),
        postalCode = p.readTextValue(ignorePrefix = {';'}),
        country = p.readTextValue(ignorePrefix = {';'}))))

    of $cnLabel:
      result.add(assignCommon(newVC3_Label(
        value = p.readValue,
        adrType = params.getMultipleValues("TYPE"))))

    of $cnTel:
      result.add(newVC3_Tel(
        group = group,
        value = p.readValue,
        telType = params.getMultipleValues("TYPE")))

    of $cnEmail:
      result.add(newVC3_Email(
        group = group,
        value = p.readValue,
        emailType = params.getMultipleValues("TYPE")))

    of $cnMailer:
      result.add(assignCommon(newVC3_Mailer(value = p.readValue)))

    of $cnTz:
      result.add(newVC3_Tz(
        value = p.readValue,
        isText = params.existsWithValue("VALUE", "TEXT")))

    of $cnGeo:
      let rawValue = p.readValue
      try:
        let partsStr = rawValue.split(';')
        result.add(newVC3_Geo(
          group = group,
          lat = parseFloat(partsStr[0]),
          long = parseFloat(partsStr[1])
        ))
      except:
        p.error("expected two float values separated by ';' for the GEO " &
          "content type but received '" & rawValue & "'")

    of $cnTitle:
      result.add(assignCommon(newVC3_Title(value = p.readValue)))

    of $cnRole:
      result.add(assignCommon(newVC3_Role(value = p.readValue)))

    of $cnLogo:
      result.add(newVC3_Logo(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        binaryType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    of $cnAgent:
      let valueParam = params.getSingleValue("VALUE")
      if valueParam.isSome and valueParam.get != $vtUri:
        p.error("the VALUE parameter must be set to '" & $vtUri &
                "' if present on the AGENT content type, but it was '" &
                valueParam.get & "'")

      result.add(newVC3_Agent(
        group = group,
        value = p.readValue,
        isInline = valueParam.isNone))

    of $cnOrg:
      result.add(assignCommon(newVC3_Org(
        value = p.readTextValueList(seps = {';'}))))

    of $cnCategories:
      result.add(assignCommon(newVC3_Categories(
        value = p.readTextValueList())))

    of $cnNote:
      result.add(assignCommon(newVC3_Note(value = p.readTextValue)))

    of $cnProdid:
      result.add(assignCommon(newVC3_Prodid(value = p.readValue)))

    of $cnRev:
      let valueType = params.getSingleValue("VALUE")
      let valueStr = p.readValue
      var value: DateTime

      try:
        if valueType.isSome and valueType.get == $vtDate:
          value = parseDate(valueStr)
        elif valueType.isSome and valueType.get == $vtDateTime:
          value = parseDateTime(valueStr)
        elif valueType.isSome:
          p.error("invalid VALUE for BDAY content. " &
                  "Expected '" & $vtDate & "' or '" & $vtDateTime & "'")
        else:
          value = parseDateOrDateTime(valueStr)
      except ValueError:
        p.error("invalid date or date-time value: $1" % [valueStr])

      result.add(newVC3_Rev(
        group = group,
        value = value,
        valueType = valueType
      ))

    of $cnSortString:
      result.add(assignCommon(newVC3_SortString(value = p.readValue)))

    of $cnSound:
      result.add(newVC3_Sound(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        binaryType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    of $cnUid:
      result.add(newVC3_Uid(group = group, value = p.readValue))

    of $cnUrl:
      result.add(newVC3_Url(group = group, value = p.readValue))

    of $cnVersion:
      p.expect("3.0")
      p.validateNoParameters(params, "VERSION")
      result.add(newVC3_Version(group = group))

    of $cnClass:
      result.add(newVC3_Class(group = group, value = p.readValue))

    of $cnKey:
      result.add(newVC3_Key(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        keyType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    else:
      if not name.startsWith("x-"):
        p.error("unrecognized content type: '$1'" % [name])

      result.add(newVC3_XType(
        name = name,
        value = p.readValue,
        language = params.getSingleValue("LANGUAGE"),
        isPText = params.existsWithValue("VALUE", "PTEXT"),
        group = group,
        xParams = params -->
          filter(not ["value", "language"].contains(it.name)).
          map((name: it.name, value: it.values.join(",")))))

    p.expect("\r\n")

proc parseVCard3*(input: Stream, filename = "input"): seq[VCard3] =
  var p: VC3Parser
  p.filename = filename
  lexer.open(p, input)

  while p.peek != '\0': # until EOF?
    var vcard = VCard3()
    discard p.readGroup
    p.expect("begin:vcard")
    while (p.skip("\r\n", true)): discard
    for content in p.parseContentLines: vcard.add(content)
    while (p.skip("\r\n", true)): discard
    result.add(vcard)

proc parseVCard3*(content: string, filename = "input"): seq[VCard3] =
  parseVCard3(newStringStream(content), filename)

proc parseVCard3File*(filepath: string): seq[VCard3] =
  parseVCard3(newFileStream(filepath, fmRead), filepath)

#[
Simplified Parsing Diagram

```mermaid
stateDiagram-v2
  [*] --> StartVCard
  StartVCard --> ContentLine: "BEGIN VCARD" CRLF
  ContentLine --> EndVCard: "END VCARD" CRLF
  ContentLine --> Name
  Name --> Name: 0-9/a-z/-/.
  Name --> Param: SEMICOLON
  Name --> Value: COLON
  Param --> Value: COLON
  Value --> ContentLine: CRLF

  state Param {
    [*] --> ParamName
    ParamName --> ParamName: 0-9/a-z/-/.
    ParamName --> ParamValue: "="
    ParamValue --> ParamValue: ","
    ParamValue --> PText
    ParamValue --> Quoted
    PText --> PText: SAFE-CHAR
    PText --> [*]
    Quoted --> Quoted: QSAFE-CHAR
    Quoted --> [*]
  }
```
]#

## Private Function Unit Tests
## ============================================================================

import std/unittest

suite "vcard/vcard3/private":

  proc initParser(input: string): VC3Parser =
    result = VC3Parser(filename: "private unittests")
    lexer.open(result, newStringStream(input))

  test "readGroup with group":
    var p = initParser("mygroup.BEGIN:VCARD")
    let g = p.readGroup

    check:
      g.isSome
      g.get == "mygroup"

  test "readGroup without group":
    var p = initParser("BEGIN:VCARD")
    check p.readGroup.isNone

  test "expect (case-sensitive)":
    var p = initParser("BEGIN:VCARD")
    p.expect("BEGIN", true)

    try:
      p.expect(":vcard", true)
      check "" == "expect should have raised an error"
    except: discard

  test "expect (case-insensitive)":
    var p = initParser("BEGIN:VCARD")
    p.expect("begin")

    try:
      p.expect("begin")
      check "" == "expect should have raised an error"
    except: discard

  test "readName":
    var p = initParser("TEL;tel;x-Example;x-Are1+Name")
    check:
      p.readName == "TEL"
      p.read == ';'
      p.readName == "TEL"
      p.read == ';'
      p.readName == "X-EXAMPLE"
      p.read == ';'
      p.readName == "X-ARE1"

    try:
      discard p.readName
      check "" == "readName should have raised an error"
    except: discard

  test "readParamValue":
    var p = initParser("TEL;TYPE=WORK;TYPE=Fun&Games%:+15551234567")
    check:
      p.readName == "TEL"
      p.read == ';'
      p.readName == "TYPE"
      p.read == '='
      p.readParamValue == "WORK"
      p.read == ';'
      p.readName == "TYPE"
      p.read == '='
      p.readParamValue == "Fun&Games%"

  test "readParams":
    var p = initParser("TEL;TYPE=WORK;TYPE=Fun&Games%,Extra:+15551234567")
    check p.readName == "TEL"
    let params = p.readParams
    check:
      params.len == 2
      params[0].name == "TYPE"
      params[0].values.len == 1
      params[0].values[0] == "WORK"
      params[1].name == "TYPE"
      params[1].values.len == 2
      params[1].values[0] == "Fun&Games%"
      params[1].values[1] == "Extra"

  test "readValue":
    var p = initParser("TEL;TYPE=WORK:+15551234567\r\nFN:John Smith\r\n")
    check p.skip("TEL")
    discard p.readParams
    check p.read == ':'
    check p.readValue == "+15551234567"
    p.expect("\r\n")
    check p.readName == "FN"
    discard p.readParams
    check p.read == ':'
    check p.readValue == "John Smith"

  test "readTextValueList":
    var p = initParser("Public;John;Quincey,Adams;Rev.;Esq:limited\r\n")
    check:
      p.readTextValueList == @["Public"]
      p.readTextValueList(ifPrefix = some(';')) == @["John"]
      p.readTextValueList(ifPrefix = some(';')) == @["Quincey", "Adams"]
      p.readTextValueList(ifPrefix = some(';')) == @["Rev."]
      p.readTextValueList(ifPrefix = some(';')) == @["Esq:limited"]
      p.readTextValueList(ifPrefix = some(';')) == newSeq[string]()

  test "existsWithValue":
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    check:
      params.existsWithValue("TYPE", "WORK")
      params.existsWithValue("TYPE", "CELL")
      not params.existsWithValue("TYPE", "ISDN")

  test "getSingleValue":
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    let val = params.getSingleValue("TYPE")
    check:
      val.isSome
      val.get == "WORK"
      params.getSingleValue("VALUE").isNone

  test "getMultipleValues":
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    check:
      params.getMultipleValues("TYPE") == @["WORK", "VOICE", "CELL"]
      params.getMultipleValues("VALUE") == newSeq[string]()

  test "validateNoParameters":
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    p.validateNoParameters(@[], "TEST")
    try:
      p.validateNoParameters(params, "TEST")
      check "" == "validateNoParameters should have errored"
    except: discard

  test "validateRequredParameters":
    var p = initParser(";CONTEXT=word;VALUE=uri;TYPE=CELL")
    let params = p.readParams
    p.validateRequiredParameters(params,
      [("VALUE", "uri"), ("CONTEXT", "word")])

    try:
      p.validateRequiredParameters(params, [("TYPE", "VOICE")])
      check "" == "validateRequiredParameters should have errored"
    except: discard
