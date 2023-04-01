# vCard 3.0 and 4.0 Nm implementation
# © 2022 Jonathan Bernard

## The `vcard` module implements a high-performance vCard parser for both
## versions 3.0 (defined by RFCs [2425][rfc2425] and  [2426][rfc2426]) and 4.0
## (defined by RFC [6350][rfc6350])
##
## [rfc2425]: https://tools.ietf.org/html/rfc2425
## [rfc2426]: https://tools.ietf.org/html/rfc2426
## [rfc6350]: https://tools.ietf.org/html/rfc6350

import std/[base64, lexbase, macros, options, sequtils, streams, strutils,
            times, unicode]

import vcard/private/[util, lexer]

type
#[
  TokKind = enum
    tkInvalid,
    tkEof,
    tkQuotedString,
    tkValue,
    tkPtext,
    tkColon,
    tkComma,
    tk
]#

#[
  VC3_Content*[T] = tuple[
    name: string,
    group: Option[string],
    value: T]
]#

  VC3_ValueTypes = enum
    vtUri = "uri",
    vtText = "text",
    vtDate = "date",
    vtTime = "time",
    vtDateTime = "date-time",
    vtInteger = "integer",
    vtBoolean = "boolean",
    vtFloat = "float",
    vtBinary = "binary",
    vtVCard = "vcard"
    vtPhoneNumber = "phone-number"
    vtUtcOffset = "utc-offset"

  VC3_XParam* = tuple[name, value: string]

  VC3_Content* = ref object of RootObj
    contentId: int
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
    binaryType*: Option[string]
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
    atDom = "dom"
    atIntl = "intl"
    atPostal = "postal"
    atParcel = "parcel"
    atHome = "home"
    atWork = "work"
    atPref = "pref"

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
    ttHome = "home",
    ttWork = "work",
    ttPref = "pref",
    ttVoice = "voice",
    ttFax = "fax",
    ttMsg = "msg",
    ttCell = "cell",
    ttPager = "pager",
    ttBbs = "bbs",
    ttModem = "modem",
    ttCar = "car",
    ttIsdn = "isdn",
    ttVideo = "video",
    ttPcs = "pcs"

  VC3_Tel* = ref object of VC3_Content
    telType*: seq[string]
    value*: string

  VC3_EmailType* = enum
    etInternet = "internet",
    etX400 = "x400"

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
    exp.add(f)
    exp.add(f)
    result.add(exp)


# Initializers
# =============================================================================

func newVC3_Name*(value: string, group = none[string]()): VC3_Name =
  return VC3_Name(name: "NAME", value: value, group: group)

func newVC3_Source*(
  value: string,
  context = none[string](),
  inclValue = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_Source =

  return assignFields(
    VC3_Source(
      name: "SOURCE",
      valueType: if inclValue: some("uri")
                 else: none[string]()),
    value, context, group, xParams)

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
  xParams: seq[VC3_XParam] = @[]): VC3_Adr =

  return assignFields(
    VC3_Adr(name: "ADR"),
    adrType, poBox, extendedAdr, streetAdr, locality, region, postalCode,
    country, isPText, language, xParams)

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
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Key =

  return assignFields(
    VC3_Key(name: "KEY"),
    value, valueType, binaryType, isInline, group)

func newVC3_XType*(
  name: string,
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC3_XParam] = @[],
  group = none[string]()): VC3_XType =

  if not name.toLower.startsWith("x-"):
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

func fn*(c: VC3_ContentList): seq[VC3_Fn] = findAll[VC3_Fn](c)
func fn*(vc3: VCard3): seq[VC3_Fn] = vc3.content.fn

func n*(c: VC3_ContentList): seq[VC3_N] = findAll[VC3_N](c)
func n*(vc3: VCard3): seq[VC3_N] = vc3.content.n

func nickname*(c: VC3_ContentList): seq[VC3_Nickname] = findAll[VC3_Nickname](c)
func nickname*(vc3: VCard3): seq[VC3_Nickname] = vc3.content.nickname

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

func org*(c: VC3_ContentList): Option[VC3_Org] = findFirst[VC3_Org](c)
func org*(vc3: VCard3): Option[VC3_Org] = vc3.content.org

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

func setContent[T](vc3: var VCard3, newContent: var T): void =
  let existingIdx = vc3.content.indexOfIt(it of T)
  if existingIdx < 0:
    newContent.contentId = vc3.takeContentId
    vc3.content.add(newContent)
  else:
    newContent.contentId = vc3.content[existingIdx].contentId
    vc3.content[existingIdx] = newContent

func setContent[T](vc3: VCard3, newContent: var T): VCard3 =
  result = vc3
  result.setContent(newContent)

func add[T](vc3: var VCard3, newContent: var T): void =
  newContent.contentId = vc3.takeContentId
  vc3.content.add(newContent)

func add[T](vc3: VCard3, newContent: var T): VCard3 =
  result = vc3
  result.add(newContent)

func updateOrAdd*[T](vc3: var VCard3, content: seq[T]): VCard3 =
  for c in content:
    let existingIdx = vc3.content.indexOfIt(it.contentId == c.contentId)
    if existingIdx < 0: vc3.content.add(c)
    else: c.content[existingIdx] = c

#[
func setName*(vc3: var VCard3, name: string, group = none[string]()): void =
  var name = newVC3_Name(name, group)
  vc3.setContent(name)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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
  vc3.setContent(c)

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

proc serialize(u: VC3_UID): string =
  result = u.nameWithGroup & ":" & u.value

proc serialize(u: VC3_URL): string =
  result = u.nameWithGroup & ":" & u.value

proc serialize(u: VC3_Version): string =
  result = u.nameWithGroup & ":" & u.value

proc serialize(u: VC3_Class): string =
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
  result = "BEGIN:vCard" & CRLF
  result &= "VERSION:3.0" & CRLF
  for c in vc3.content.filterIt(not (it of VC3_Version)):
    result &= foldContentLine(serialize(c)) & CRLF
  result &= "END:vCard" & CRLF


# Parsing
# =============================================================================

type
  VC3_ParseEvent = enum
    peStart,
    peContentLine,
    peName,
    peParam,
    peParamName,
    peParamValue,
    prPText,
    peQuoted,
    peSafeChar,
    peQSafeChar,
    peValue,
    peEnd

  VC3Parser = object of VCardLexer
    filename: string
    state: seq[VC3_ParseEvent]

  VCard3ParsingError = object of ValueError

const NON_ASCII = { '\x80'..'\xFF' }
const WSP = {' ', '\t'}
const SAFE_CHARS = WSP + { '\x21', '\x23'..'\x2B', '\x2D'..'\x39', '\x3C'..'\x7E' } + NON_ASCII
const QSAFE_CHARS = WSP + { '\x21', '\x23'..'\x7E' } + NON_ASCII
const VALUE_CHAR = WSP + { '\x21'..'\x7E' } + NON_ASCII
const ALPHA_NUM = { 'a'..'z', 'A'..'Z', '0'..'9' }
const NAME_CHARS = { 'a'..'z', 'A'..'Z', '0'..'9' }

proc error(p: VC3Parser, msg: string) =
  raise newException(VCard3ParsingError, "$1($2, $3) Error: $4] " %
    [ p.filename, $p.lineNumber, $p.getColNumber(p.pos), msg ])

proc readGroup(p: var VC3Parser): Option[string] =
  p.setBookmark

  var ch = p.read
  while ALPHA_NUM.contains(ch): ch = p.read

  if (ch == '.'):
    p.unsetBookmark
    return some(readSinceBookmark(p)[0..^1])
  else:
    p.returnToBookmark
    return none[string]()

proc readName(p: var VC3Parser): string =
  while ALPHA_

proc expect(p: var VC3Parser, expected: string, caseSensitive = false) =
  p.setBookmark

  if caseSensitive:
    for ch in expected:
      if p.read != ch:
        p.error("expected '$1' but found '$2'" %
          [expected, p.readSinceBookmark])

  else:
    for rune in expected.runes:
      if p.readRune.toLower != rune.toLower:
        p.error("expected '$1' but found '$2'" %
          [ expected, p.readSinceBookmark ])

  p.unsetBookmark

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

proc parseContentLines(p: var VC3Parser): seq[VC3_Content] =
  while true:
    let group = p.readGroup
    let name = p.readName
    if name.toLower == "end":
      p.expect(":VCARD\r\n")
      break


proc parseVCard3*(input: Stream, filename = "input"): seq[VCard3] =
  var p: VC3Parser
  lexer.open(p, input)
  p.state = @[peStart]

  discard p.readGroup
  p.expect("begin:vcard")
  while (p.skip("\r\n", true)): discard



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
