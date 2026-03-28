# vCard 3.0 implementation
# © 2022 Jonathan Bernard

## This module implements a high-performance vCard parser for vCard version
## 3.0 (defined in RFCs 2425_ and  2426_).
##
## .. _rfc2425: https://tools.ietf.org/html/rfc2425
## .. _rfc2426: https://tools.ietf.org/html/rfc2426

import std/[base64, genasts, macros, options, sequtils, streams, strutils,
            tables, times, unicode]

import zero_functional

import ./common
import ./private/[internals, lexer]

# Internal enumerations used to capture the value types and properties defined
# by the spec and constants used.
type
  VC3_ValueTypes = enum
    vtUri = "uri",
    vtText = "text",
    vtPText = "ptext",
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

  VC3_PropertyName = enum
    pnName = "NAME"
    pnProfile = "PROFILE"
    pnSource = "SOURCE"
    pnFn = "FN"
    pnN = "N"
    pnNickname = "NICKNAME"
    pnPhoto = "PHOTO"
    pnBday = "BDAY"
    pnAdr = "ADR"
    pnLabel = "LABEL"
    pnTel = "TEL"
    pnEmail = "EMAIL"
    pnMailer = "MAILER"
    pnTz = "TZ"
    pnGeo = "GEO"
    pnTitle = "TITLE"
    pnRole = "ROLE"
    pnLogo = "LOGO"
    pnAgent = "AGENT"
    pnOrg = "ORG"
    pnCategories = "CATEGORIES"
    pnNote = "NOTE"
    pnProdid = "PRODID"
    pnRev = "REV"
    pnSortString = "SORT-STRING"
    pnSound = "SOUND"
    pnUid = "UID"
    pnUrl = "URL"
    pnVersion = "VERSION"
    pnClass = "CLASS"
    pnKey = "KEY"

const propertyCardMap: Table[VC3_PropertyName, VC_PropCardinality] = [
  (pnName, vpcAtMostOne),
  (pnProfile, vpcAtMostOne),
  (pnSource, vpcAtMostOne),
  (pnFn, vpcExactlyOne),
  (pnN, vpcExactlyOne),
  (pnNickname, vpcAtMostOne),
  (pnPhoto, vpcAny),
  (pnBday, vpcAtMostOne),
  (pnAdr, vpcAny),
  (pnLabel, vpcAny),
  (pnTel, vpcAny),
  (pnEmail, vpcAny),
  (pnMailer, vpcAny),
  (pnTz, vpcAny),
  (pnGeo, vpcAny),
  (pnTitle, vpcAny),
  (pnRole, vpcAny),
  (pnLogo, vpcAny),
  (pnAgent, vpcAny),
  (pnOrg, vpcAny),
  (pnCategories, vpcAtMostOne),
  (pnNote, vpcAny),
  (pnProdid, vpcAtMostOne),
  (pnRev, vpcAtMostOne),
  (pnSortString, vpcAtMostOne),
  (pnSound, vpcAny),
  (pnUid, vpcAtMostOne),
  (pnUrl, vpcAny),
  (pnVersion, vpcExactlyOne),
  (pnClass, vpcAny),
  (pnKey, vpcAny)
].toTable

const DATE_FMT = "yyyy-MM-dd"
const DATETIME_FMT = "yyyy-MM-dd'T'HH:mm:sszz"


## Externally Exposed Types
## ========================
##
## The following are the object types for the vCard 3.0 implementation
## interface.
type
  VC3_Property* = ref object of RootObj
    ## Abstract base class for all vCard 3.0 property objects.
    propertyId: int
    group*: Option[string]
    name*: string

  VC3_SimpleTextProperty* = ref object of VC3_Property
    ## Abstract base class for all vCard 3.0 properties that only support the
    ## text value type (`VALUE=text` or `VALUE=ptext`).
    value*: string
    isPText*: bool ## true if VALUE=ptext, false by default
    language*: Option[string]
    xParams*: seq[VC_XParam]

  VC3_BinaryProperty* = ref object of VC3_Property
    ## Abstract base class for all vCard 3.0 properties that support the binary
    ## or uri value types (`ENCODING=b` or `VALUE=uri`).
    valueType*: Option[string] ## \
      ## binary / uri. Stored separately from ENCODING (captured in the
      ## isInline field) because the VALUE parameter is not set by default, but
      ## is allowed to be set.
    value*: string  ## \
      ## either a URI or bit sequence, both stored as string
    binaryType*: Option[string] ## \
      ## if using ENCODING=b, there may also be a TYPE parameter specifying the
      ## MIME type of the binary-encoded object, which is stored here.
    isInline*: bool ## \
      ## true if ENCODING=b, false by default

  VC3_Name* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 NAME properties.
    value*: string

  VC3_Profile* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 PROFILE properties.

  VC3_Source* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 SOURCE properties.
    valueType*: Option[string]  ## If set, this must be set to "uri"
    value*: string              ## The URI value.
    context*: Option[string]
    xParams*: seq[VC_XParam]

  VC3_Fn* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 FN properties.

  VC3_N* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 N properties.
    family*: seq[string]      ## Surname / family name
    given*: seq[string]       ## First name / given name
    additional*: seq[string]  ## Additional / middle names
    prefixes*: seq[string]    ## e.g. Mr., Dr., etc.
    suffixes*: seq[string]    ## e.g. Esq., II, etc.
    language*: Option[string]
    isPText*: bool            ## true if VALUE=ptext, false by default
    xParams*: seq[VC_XParam]

  VC3_Nickname* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 NICKNAME properties.

  VC3_Photo* = ref object of VC3_BinaryProperty
    ## Concrete class representing vCard 3.0 PHOTO properties.

  VC3_Bday* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 BDAY properties.
    valueType*: Option[string] # date / date-time
    value*: DateTime

  VC3_AdrTypes* = enum
    ## Standard types for ADR values defined in RFC2426
    atDom = "DOM"
    atIntl = "INTL"
    atPostal = "POSTAL"
    atParcel = "PARCEL"
    atHome = "HOME"
    atWork = "WORK"
    atPref = "PREF"

  VC3_Adr* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 ADR properties.
    adrType*: seq[string]
    poBox*: string
    extendedAdr*: string
    streetAdr*: string
    locality*: string
    region*: string
    postalCode*: string
    country*: string
    isPText*: bool             ## `true` if `VALUE=ptext`, `false` by default
    language*: Option[string]
    xParams*: seq[VC_XParam]

  VC3_Label* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 LABEL properties.
    adrType*: seq[string]

  VC3_TelTypes* = enum
    ## Standard types for TEL values defined in RFC2426
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

  VC3_Tel* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 TEL properties.
    telType*: seq[string]
    value*: string

  VC3_EmailType* = enum
    ## Standard types for EMAIL values defined in RFC2426
    etInternet = "INTERNET",
    etX400 = "X400"

  VC3_Email* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 EMAIL properties.
    emailType*: seq[string]
    value*: string

  VC3_Mailer* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 MAILER properties.

  VC3_TZ* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 TZ properties.
    value*: string
    isText*: bool ## `true` if `VALUE=text`, `false` by default

  VC3_Geo* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 GEO properties.
    lat*, long*: float

  VC3_Title* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 TITLE properties.

  VC3_Role* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 ROLE properties.

  VC3_Logo* = ref object of VC3_BinaryProperty
    ## Concrete class representing vCard 3.0 LOGO properties.

  VC3_Agent* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 AGENT properties.
    value*: string  ## either an escaped vCard object, or a URI
    isInline*: bool ## `false` if `VALUE=uri`, `true` by default

  VC3_Org* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 ORG properties.
    value*: seq[string]
    isPText*: bool ## `true` if `VALUE=ptext`, `false` by default
    language*: Option[string]
    xParams*: seq[VC_XParam]

  VC3_Categories* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 CATEGORIES properties.
    value*: seq[string]
    isPText*: bool ## `true` if `VALUE=ptext`, `false` by default
    language*: Option[string]
    xParams*: seq[VC_XParam]

  VC3_Note* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 NOTE properties.

  VC3_Prodid* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 PRODID properties.

  VC3_Rev* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 REV properties.
    valueType*: Option[string] # date / date-time
    value*: DateTime

  VC3_SortString* = ref object of VC3_SimpleTextProperty
    ## Concrete class representing vCard 3.0 SORT-STRING properties.

  VC3_Sound* = ref object of VC3_BinaryProperty
    ## Concrete class representing vCard 3.0 SOUND properties.

  VC3_UID* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 UID properties.
    value*: string

  VC3_URL* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 URL properties.
    value*: string

  VC3_Version* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 VERSION properties.
    value*: string # 3.0

  VC3_Class* = ref object of VC3_Property
    ## Concrete class representing vCard 3.0 CLASS properties.
    value*: string

  VC3_Key* = ref object of VC3_BinaryProperty
    ## Concrete class representing vCard 3.0 KEY properties.
    keyType*: Option[string] # x509 / pgp

  VC3_XType* = ref object of VC3_SimpleTextProperty

  VCard3* = ref object of VCard
    ## Concrete class implementing the vCard 3.0 type.
    nextPropertyId: int
    content*: seq[VC3_Property]

# Internal Utility/Implementation
# ===============================

template takePropertyId(vc3: VCard3): int =
  vc3.nextPropertyId += 1
  vc3.nextPropertyId - 1

func namesForProp(prop: VC3_PropertyName):
    tuple[enumName, typeName, initFuncName, accessorFuncName: NimNode] =

  var name: string = ($prop).replace("-", "")
  if name.len > 1: name = name[0] & name[1..^1].toLower
  return (
    ident("pn" & name),
    ident("VC3_" & name),
    ident("newVC3_" & name),
    ident(name.toLower))

## Initializers
## ============

func newVC3_Name*(value: string, group = none[string]()): VC3_Name =
  return VC3_Name(name: "NAME", value: value, group: group)

func newVC3_Profile*(group = none[string]()): VC3_Profile =
  return VC3_Profile(name: "PROFILE", group: group)

func newVC3_Source*(
  value: string,
  inclContext = false,
  inclValue = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Source =

  return assignFields(
    VC3_Source(
      name: $pnSource,
      valueType: if inclValue: some("uri")
                 else: none[string](),
      context: if inclContext: some("word")
               else: none[string]()),
    value, group, xParams)

func newVC3_Fn*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Fn =

  return assignFields(
    VC3_Fn(name: $pnFn),
    value, language, isPText, group, xParams)

func newVC3_N*(
  family: seq[string] = @[],
  given: seq[string] = @[],
  additional: seq[string] = @[],
  prefixes: seq[string] = @[],
  suffixes: seq[string] = @[],
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_N =

  return assignFields(
    VC3_N(name: $pnN),
    family, given, additional, prefixes, suffixes, language, xParams)

func newVC3_Nickname*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Nickname =

  return assignFields(
    VC3_Nickname(name: $pnNickname),
    value, language, isPText, group, xParams)

func newVC3_Photo*(
  value: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Photo =

  return assignFields(
    VC3_Photo(name: $pnPhoto),
    value, valueType, binaryType, isInline, group)

func newVC3_Bday*(
  value: DateTime,
  valueType = none[string](),
  group = none[string]()): VC3_Bday =

    return assignFields(VC3_Bday(name: $pnBday), value, valueType, group)

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
  xParams: seq[VC_XParam] = @[]): VC3_Adr =

  return assignFields(
    VC3_Adr(name: $pnAdr),
    adrType, poBox, extendedAdr, streetAdr, locality, region, postalCode,
    country, isPText, language, group, xParams)

func newVC3_Label*(
  value: string,
  adrType = @[$atIntl,$atPostal,$atParcel,$atWork],
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Label =

  return assignFields(
    VC3_Label(name: $pnLabel),
    value, adrType, language, isPText, group, xParams)

func newVC3_Tel*(
  value: string,
  telType = @[$ttVoice],
  group = none[string]()): VC3_Tel =

  return assignFields(VC3_Tel(name: $pnTel), value, telType, group)

func newVC3_Email*(
  value: string,
  emailType = @[$etInternet],
  group = none[string]()): VC3_Email =

  return assignFields(VC3_Email(name: $pnEmail), value, emailType, group)

func newVC3_Mailer*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Mailer =

  return assignFields(
    VC3_Mailer(name: $pnMailer),
    value, language, isPText, xParams, group)

func newVC3_TZ*(value: string, isText = false, group = none[string]()): VC3_TZ =
  return assignFields(VC3_TZ(name: $pnTz), value, isText, group)

func newVC3_Geo*(lat, long: float, group = none[string]()): VC3_Geo =
  return assignFields(VC3_Geo(name: $pnGeo), lat, long, group)

func newVC3_Title*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Title =

  return assignFields(
    VC3_Title(name: $pnTitle),
    value, language, isPText, xParams, group)

func newVC3_Role*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Role =

  return assignFields(
    VC3_Role(name: $pnRole),
    value, language, isPText, xParams, group)

func newVC3_Logo*(
  value: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Logo =

  return assignFields(
    VC3_Logo(name: $pnLogo),
    value, valueType, binaryType, isInline, group)

func newVC3_Agent*(
  value: string,
  isInline = true,
  group = none[string]()): VC3_Agent =

  return VC3_Agent(name: $pnAgent, isInline: isInline, group: group)

func newVC3_Org*(
  value: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Org =

  return assignFields(
    VC3_Org(name: $pnOrg),
    value, isPText, language, xParams, group)

func newVC3_Categories*(
  value: seq[string],
  isPText = false,
  language = none[string](),
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Categories =

  return assignFields(
    VC3_Categories(name: $pnCategories),
    value, isPText, language, xParams, group)

func newVC3_Note*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Note =

  return assignFields(
    VC3_Note(name: $pnNote),
    value, language, isPText, xParams, group)

func newVC3_Prodid*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_Prodid =

  return assignFields(
    VC3_Prodid(name: $pnProdid),
    value, language, isPText, xParams, group)

func newVC3_Rev*(
  value: DateTime,
  valueType = none[string](),
  group = none[string]()): VC3_Rev =

  return assignFields(VC3_Rev(name: $pnRev), value, valueType, group)

func newVC3_SortString*(
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_SortString =

  return assignFields(
    VC3_SortString(name: $pnSortstring),
    value, language, isPText, xParams, group)

func newVC3_Sound*(
  value: string,
  valueType = some("uri"),
  binaryType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Sound =

  return assignFields(
    VC3_Sound(name: $pnSound),
    value, valueType, binaryType, isInline, group)

func newVC3_UID*(value: string, group = none[string]()): VC3_UID =
  return VC3_UID(name: $pnUid, value: value, group: group)

func newVC3_URL*(value: string, group = none[string]()): VC3_URL =
  return VC3_URL(name: $pnUrl, value: value, group: group)

func newVC3_Version*(group = none[string]()): VC3_Version =
  return VC3_Version(name: $pnVersion, value: "3.0", group: group)

func newVC3_Class*(value: string, group = none[string]()): VC3_Class =
  return VC3_Class(name: $pnClass, value: value, group: group)

func newVC3_Key*(
  value: string,
  valueType = some("uri"),
  keyType = none[string](),
  isInline = false,
  group = none[string]()): VC3_Key =

  return assignFields(
    VC3_Key(name: $pnKey, binaryType: keyType),
    value, valueType, keyType, isInline, group)

func newVC3_XType*(
  name: string,
  value: string,
  language = none[string](),
  isPText = false,
  xParams: seq[VC_XParam] = @[],
  group = none[string]()): VC3_XType =

  if not name.startsWith("X-"):
    raise newException(ValueError, "Extended types must begin with 'x-'.")

  return assignFields(
    VC3_XType(name: name),
    value, language, isPText, xParams, group)

# Accessors
# =========

func forGroup*(vc3: VCard3, group: string): seq[VC3_Property] =
  ## Return all properties defined on the vCard associated with the given
  ## group.
  return vc3.content.filterIt(it.group.isSome and it.group.get == group)

func groups*(vc3: VCard3): seq[string] =
  ## Return a list of all groups present on the vCard
  result = @[]
  for c in vc3.content:
    if c.group.isSome:
      let grp = c.group.get
      if not result.contains(grp): result.add(grp)

macro genPropertyAccessors(
    properties: static[openarray[(VC3_PropertyName, VC_PropCardinality)]]
  ): untyped =

  result = newStmtList()
  for (pn, pCard) in properties:
    let (_, typeName, _, funcName) = namesForProp(pn)

    case pCard:
    of vpcAtMostOne:
      let funcDef = genAstOpt({kDirtyTemplate}, funcName, typeName):
        func funcName*(vc3: VCard3): Option[typeName] =
          result = findFirst[typeName, VC3_Property](vc3.content)
      funcDef[6].insert(0, newCommentStmtNode(
        "Return the single " & $pn & " property (if present)."))
      result.add(funcDef)


    of vpcExactlyOne:
      let funcDef = genAstOpt({kDirtyTemplate}, funcName, pn, typeName):
        func funcName*(vc3: VCard3): typeName =
          let props = findAll[typeName, VC3_Property](vc3.content)
          if props.len != 1:
            raise newException(ValueError,
              "VCard should have exactly one $# property, but $# were found" %
                [$pn, $props.len])
          result = props[0]
      funcDef[6].insert(0, newCommentStmtNode(
        "Return the " & $pn & " property."))
      result.add(funcDef)

    of vpcAtLeastOne, vpcAny:
      let funcDef = genAstOpt({kDirtyTemplate}, funcName, typeName):
        func funcName*(vc3: VCard3): seq[typeName] =
          result = findAll[typeName, VC3_Property](vc3.content)
      funcDef[6].insert(0, newCommentStmtNode(
        "Return all instances of the " & $pn & " property."))
      result.add(funcDef)

genPropertyAccessors(propertyCardMap.pairs.toSeq -->
  filter(not [pnVersion].contains(it[0])))

func version*(vc3: VCard3): VC3_Version =
  ## Return the VERSION property.
  let found = findFirst[VC3_Version, VC3_Property](vc3.content)
  if found.isSome: return found.get
  else: return VC3_Version(
    propertyId: vc3.content.len + 1,
    group: none[string](),
    name: "VERSION",
    value: "3.0")

func xTypes*(vc3: VCard3): seq[VC3_XType] = findAll[VC3_XType, VC3_Property](vc3.content)
  ## Return all extended properties (starting with `x-`).

# Setters
# =============================================================================

func set*[T: VC3_Property](vc3: VCard3, content: varargs[T]): void =
  ## Set the given property on the vCard. This will replace the first existing
  ## instance of this property or append this as a new property if there is no
  ## existing instance of this property. This is useful for properties which
  ## should only appear once within the vCard.
  for c in content:
    var nc = c
    let existingIdx = vc3.content.indexOfIt(it of T)
    if existingIdx < 0:
      nc.propertyId = vc3.takePropertyId
      vc3.content.add(nc)
    else:
      nc.propertyId = vc3.content[existingIdx].propertyId
      vc3.content[existingIdx] = nc

func add*[T: VC3_Property](vc3: VCard3, content: varargs[T]): void =
  ## Add a new property to the end of the vCard.
  for c in content:
    var nc = c
    nc.propertyId = vc3.takePropertyId
    vc3.content.add(nc)

func updateOrAdd*[T: VC3_Property](vc3: VCard3, content: seq[T]): VCard3 =
  for c in content:
    let existingIdx = vc3.content.indexOfIt(it.propertyId == c.propertyId)
    if existingIdx < 0: vc3.content.add(c)
    else: c.content[existingIdx] = c

# Output
# =============================================================================

func nameWithGroup(s: VC3_Property): string =
  if s.group.isSome: s.group.get & "." & s.name
  else: s.name

func serialize(xp: seq[VC_XParam]): string =
  return (xp --> map(";" & it.name & "=" & it.value)).join("")

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

func serialize(s: VC3_SimpleTextProperty): string =
  result = s.nameWithGroup
  if s.isPText: result &= ";VALUE=ptext"
  if s.language.isSome: result &= ";LANGUAGE=" & s.language.get
  result &= serialize(s.xParams)
  result &= ":" & s.value

proc serialize(b: VC3_BinaryProperty): string =
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
  elif r.valueType.isSome and r.valueType.get == "date":
    result &= ";VALUE=date-time:" & r.value.format(DATETIME_FMT)
  else:
    result &= r.value.format(DATETIME_FMT)

proc serialize(u: VC3_UID | VC3_URL | VC3_VERSION | VC3_Class): string =
  result = u.nameWithGroup & ":" & u.value

proc serialize(c: VC3_Property): string =
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
  elif c of VC3_SimpleTextProperty:
    return serialize(cast[VC3_SimpleTextProperty](c))
  elif c of VC3_BinaryProperty:
    return serialize(cast[VC3_BinaryProperty](c))

proc `$`*(vc3: VCard3): string =
  ## Serialize a vCard into its textual representation.
  result = "BEGIN:VCARD" & CRLF
  result &= "VERSION:3.0" & CRLF
  for c in vc3.content.filterIt(not (it of VC3_Version)):
    result &= foldContentLine(serialize(c)) & CRLF
  result &= "END:VCARD" & CRLF


# Parsing
# =============================================================================

proc readParamValue(p: var VCardParser): string =
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

proc readParams(p: var VCardParser): seq[VC_Param] =
  ## Read all parameters for the current content line at the current read head.
  result = @[]
  while p.peek == ';':
    discard p.read
    var param: VCParam = (p.readName, @[])
    p.expect("=", true)
    param.values.add(p.readParamValue)
    while p.peek == ',':
      discard p.read
      param.values.add(p.readParamValue)
    result.add(param)

proc getXParams(params: openarray[VCParam]): seq[VC_XParam] =
  ## Filter out and return only the non-standard parameters starting with "x-"

  let ps = params.toSeq
  return ps -->
    filter(it.name.startsWith("x-")).
    map((name: it.name, value: it.values.join(",")))

proc readTextValue(p: var VCardParser, ignorePrefix: set[char] = {}): string =
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
    p: var VCardParser,
    seps: set[char] = {','},
    ifPrefix = none[char]()
  ): seq[string] =
  ## Read in a list of multiple text-value (defined by RFC2426) values from the
  ## current read position. This is used, for example, by the N content type.

  if ifPrefix.isSome:
    if p.peek != ifPrefix.get: return @[]
    discard p.read

  result = @[p.readTextValue]
  while seps.contains(p.peek): result.add(p.readTextValue(ignorePrefix = seps))

proc parseContentLines*(p: var VCardParser): seq[VC3_Property] =
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
      p.expect(":VCARD" & CRLF)
      break
    let params = p.readParams
    p.expect(":")

    case name

    of $pnName:
      p.validateNoParameters(params, "NAME")
      result.add(newVC3_Name(p.readValue, group))

    of $pnProfile:
      if p.readValue.toUpper != "VCARD":
        p.error("the value of the PROFILE content type must be \"$1\"" %
          ["vcard"])
      p.validateNoParameters(params, "NAME")
      result.add(VC3_Property(group: group, name: name))

    of $pnSource:
      p.validateRequiredParameters(params,
        [("CONTEXT", "word"), ("VALUE", "uri")])

      result.add(newVC3_Source(
        group = group,
        value = p.readValue,
        inclContext = params.existsWithValue("CONTEXT", "WORD"),
        inclValue = params.existsWithValue("VALUE", $vtUri),
        xParams = params.getXParams))

    of $pnFn:
      result.add(assignCommon(newVC3_Fn(value = p.readValue)))

    of $pnN:
      result.add(assignCommon(newVC3_N(
        family = p.readTextValueList,
        given = p.readTextValueList(ifPrefix = some(';')),
        additional = p.readTextValueList(ifPrefix = some(';')),
        prefixes = p.readTextValueList(ifPrefix = some(';')),
        suffixes = p.readTextValueList(ifPrefix = some(';')))))

    of $pnNickname:
      result.add(assignCommon(newVC3_Nickname(value = p.readValue)))

    of $pnPhoto:
      result.add(newVC3_Photo(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        binaryType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    of $pnBday:
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

    of $pnAdr:
      result.add(assignCommon(newVC3_Adr(
        adrType = params.getMultipleValues("TYPE"),
        poBox = p.readTextValue,
        extendedAdr = p.readTextValue(ignorePrefix = {';'}),
        streetAdr = p.readTextValue(ignorePrefix = {';'}),
        locality = p.readTextValue(ignorePrefix = {';'}),
        region = p.readTextValue(ignorePrefix = {';'}),
        postalCode = p.readTextValue(ignorePrefix = {';'}),
        country = p.readTextValue(ignorePrefix = {';'}))))

    of $pnLabel:
      result.add(assignCommon(newVC3_Label(
        value = p.readValue,
        adrType = params.getMultipleValues("TYPE"))))

    of $pnTel:
      result.add(newVC3_Tel(
        group = group,
        value = p.readValue,
        telType = params.getMultipleValues("TYPE")))

    of $pnEmail:
      result.add(newVC3_Email(
        group = group,
        value = p.readValue,
        emailType = params.getMultipleValues("TYPE")))

    of $pnMailer:
      result.add(assignCommon(newVC3_Mailer(value = p.readValue)))

    of $pnTz:
      result.add(newVC3_Tz(
        value = p.readValue,
        isText = params.existsWithValue("VALUE", "TEXT")))

    of $pnGeo:
      let rawValue = p.readValue
      try:
        let partsStr = rawValue.split(';')
        result.add(newVC3_Geo(
          group = group,
          lat = parseFloat(partsStr[0]),
          long = parseFloat(partsStr[1])
        ))
      except ValueError:
        p.error("expected two float values separated by ';' for the GEO " &
          "content type but received '" & rawValue & "'")

    of $pnTitle:
      result.add(assignCommon(newVC3_Title(value = p.readValue)))

    of $pnRole:
      result.add(assignCommon(newVC3_Role(value = p.readValue)))

    of $pnLogo:
      result.add(newVC3_Logo(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        binaryType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    of $pnAgent:
      let valueParam = params.getSingleValue("VALUE")
      if valueParam.isSome and valueParam.get != $vtUri:
        p.error("the VALUE parameter must be set to '" & $vtUri &
                "' if present on the AGENT content type, but it was '" &
                valueParam.get & "'")

      result.add(newVC3_Agent(
        group = group,
        value = p.readValue,
        isInline = valueParam.isNone))

    of $pnOrg:
      result.add(assignCommon(newVC3_Org(
        value = p.readTextValueList(seps = {';'}))))

    of $pnCategories:
      result.add(assignCommon(newVC3_Categories(
        value = p.readTextValueList())))

    of $pnNote:
      result.add(assignCommon(newVC3_Note(value = p.readTextValue)))

    of $pnProdid:
      result.add(assignCommon(newVC3_Prodid(value = p.readValue)))

    of $pnRev:
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

    of $pnSortString:
      result.add(assignCommon(newVC3_SortString(value = p.readValue)))

    of $pnSound:
      result.add(newVC3_Sound(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        binaryType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    of $pnUid:
      result.add(newVC3_UID(group = group, value = p.readValue))

    of $pnUrl:
      result.add(newVC3_URL(group = group, value = p.readValue))

    of $pnVersion:
      p.expect("3.0")
      p.validateNoParameters(params, "VERSION")
      result.add(newVC3_Version(group = group))

    of $pnClass:
      result.add(newVC3_Class(group = group, value = p.readValue))

    of $pnKey:
      result.add(newVC3_Key(
        group = group,
        value = p.readValue,
        valueType = params.getSingleValue("VALUE"),
        keyType = params.getSingleValue("TYPE"),
        isInline = params.existsWithValue("ENCODING", "B")))

    else:
      if not name.startsWith("X-"):
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
proc runVCard3PrivateTests*() =

  proc initParser(input: string): VCardParser =
    result = VCardParser(filename: "private unittests")
    lexer.open(result, newStringStream(input))

  # "readGroup":
  block:
    var p = initParser("mygroup.BEGIN:VCARD")
    let g = p.readGroup
    assert g.isSome
    assert g.get == "mygroup"

  # "readGroup without group":
  block:
    var p = initParser("BEGIN:VCARD")
    assert p.readGroup.isNone

  # "expect (case-sensitive)":
  block:
    var p = initParser("BEGIN:VCARD")
    p.expect("BEGIN", true)

    try:
      p.expect(":vcard", true)
      assert "" == "expect should have raised an error"
    except CatchableError: discard

  # "expect (case-insensitive)":
  block:
    var p = initParser("BEGIN:VCARD")
    p.expect("begin")

    try:
      p.expect("begin")
      assert "" == "expect should have raised an error"
    except CatchableError: discard

  # "readName":
  block:
    var p = initParser("TEL;tel;x-Example;x-Are1+Name")
    assert p.readName == "TEL"
    assert p.read == ';'
    assert p.readName == "TEL"
    assert p.read == ';'
    assert p.readName == "X-EXAMPLE"
    assert p.read == ';'
    assert p.readName == "X-ARE1"

    try:
      discard p.readName
      assert "" == "readName should have raised an error"
    except CatchableError: discard

  # "readParamValue":
  block:
    var p = initParser("TEL;TYPE=WORK;TYPE=Fun&Games%:+15551234567")
    assert p.readName == "TEL"
    assert p.read == ';'
    assert p.readName == "TYPE"
    assert p.read == '='
    assert p.readParamValue == "WORK"
    assert p.read == ';'
    assert p.readName == "TYPE"
    assert p.read == '='
    assert p.readParamValue == "Fun&Games%"

  # "readParams":
  block:
    var p = initParser("TEL;TYPE=WORK;TYPE=Fun&Games%,Extra:+15551234567")
    assert p.readName == "TEL"
    let params = p.readParams
    assert params.len == 2
    assert params[0].name == "TYPE"
    assert params[0].values.len == 1
    assert params[0].values[0] == "WORK"
    assert params[1].name == "TYPE"
    assert params[1].values.len == 2
    assert params[1].values[0] == "Fun&Games%"
    assert params[1].values[1] == "Extra"

  # "readValue":
  block:
    var p = initParser("TEL;TYPE=WORK:+15551234567\r\nFN:John Smith\r\n")
    assert p.skip("TEL")
    discard p.readParams
    assert p.read == ':'
    assert p.readValue == "+15551234567"
    p.expect("\r\n")
    assert p.readName == "FN"
    discard p.readParams
    assert p.read == ':'
    assert p.readValue == "John Smith"

  # "readTextValueList":
  block:
    var p = initParser("Public;John;Quincey,Adams;Rev.;Esq:limited\r\n")
    assert p.readTextValueList == @["Public"]
    assert p.readTextValueList(ifPrefix = some(';')) == @["John"]
    assert p.readTextValueList(ifPrefix = some(';')) == @["Quincey", "Adams"]
    assert p.readTextValueList(ifPrefix = some(';')) == @["Rev."]
    assert p.readTextValueList(ifPrefix = some(';')) == @["Esq:limited"]
    assert p.readTextValueList(ifPrefix = some(';')) == newSeq[string]()

  # "existsWithValue":
  block:
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    assert params.existsWithValue("TYPE", "WORK")
    assert params.existsWithValue("TYPE", "CELL")
    assert not params.existsWithValue("TYPE", "ISDN")

  # "getSingleValue":
  block:
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    let val = params.getSingleValue("TYPE")
    assert val.isSome
    assert val.get == "WORK"
    assert params.getSingleValue("VALUE").isNone

  # "getMultipleValues":
  block:
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    assert params.getMultipleValues("TYPE") == @["WORK", "VOICE", "CELL"]
    assert params.getMultipleValues("VALUE") == newSeq[string]()

  # "validateNoParameters":
  block:
    var p = initParser(";TYPE=WORK;TYPE=VOICE;TYPE=CELL")
    let params = p.readParams
    p.validateNoParameters(@[], "TEST")
    try:
      p.validateNoParameters(params, "TEST")
      assert "" == "validateNoParameters should have errored"
    except CatchableError: discard

  # "validateRequredParameters":
  block:
    var p = initParser(";CONTEXT=word;VALUE=uri;TYPE=CELL")
    let params = p.readParams
    p.validateRequiredParameters(params,
      [("VALUE", "uri"), ("CONTEXT", "word")])

    try:
      p.validateRequiredParameters(params, [("TYPE", "VOICE")])
      assert "" == "validateRequiredParameters should have errored"
    except CatchableError: discard

when isMainModule: runVCard3PrivateTests()
