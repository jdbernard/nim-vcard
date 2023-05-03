import std/[algorithm, genasts, macros, options, sequtils, sets, streams,
            strutils, tables, times, unicode]
import zero_functional

#from std/sequtils import toSeq

import ./private/[common, lexer]

type
  VC4_ValueType* = enum
    vtText = "text",
    vtTextList = "text-list",
    vtUri = "uri",
    vtDate = "date",
    vtTime = "time",
    vtDateTime = "date-time",
    vtDateAndOrTime = "date-and-or-time",
    vtTimestamp = "timestamp",
    vtBoolean = "boolean",
    vtInteger = "integer",
    vtFloat = "float",
    vtUtcOffset = "utc-offset",
    vtLanguageTag = "language-tag"

    # not in the standard, added to cover common combinations
    vtDateTimeOrText = "_date-time-or-text_"
      ## used by BDAY and ANNIVERSARY
    vtTextOrUri = "_text-or-uri_"
      ## used by TEL, UID, and KEY

  VC4_PropertyName = enum
    pnSource = "SOURCE",
    pnKind = "KIND",
    pnXml = "XML",
    pnFn = "FN",
    pnN = "N",
    pnNickname = "NICKNAME",
    pnPhoto = "PHOTO",
    pnBday = "BDAY",
    pnAnniversary = "ANNIVERSARY",
    pnGender = "GENDER",
    pnAdr = "ADR",
    pnTel = "TEL",
    pnEmail = "EMAIL",
    pnImpp = "IMPP",
    pnLang = "LANG",
    pnTz = "TZ",
    pnGeo = "GEO",
    pnTitle = "TITLE",
    pnRole = "ROLE",
    pnLogo = "LOGO",
    pnOrg = "ORG",
    pnMember = "MEMBER",
    pnRelated = "RELATED",
    pnCategories = "CATEGORIES",
    pnNote = "NOTE",
    pnProdId = "PRODID",
    pnRev = "REV",
    pnSound = "SOUND",
    pnUid = "UID",
    pnClientPidMap = "CLIENTPIDMAP",
    pnUrl = "URL",
    pnVersion = "VERSION",
    pnKey = "KEY",
    pnFbUrl = "FBURL",
    pnCaladrUri = "CALADRURI",
    pnCalUri = "CALURI",

    ## Non-standard, added to be a catch-all for non-standard names
    pnUnknown = "UNKNOWN"

const propertyCardMap: Table[VC4_PropertyName, VC_PropCardinality] = [
  (pnSource, vpcAny),
  (pnKind, vpcAtMostOne),
  (pnXml, vpcAny),
  (pnFn, vpcAtLeastOne),
  (pnN, vpcAtMostOne),
  (pnNickname, vpcAny),
  (pnPhoto, vpcAny),
  (pnBday, vpcAtMostOne),
  (pnAnniversary, vpcAtMostOne),
  (pnGender, vpcAtMostOne),
  (pnAdr, vpcAny),
  (pnTel, vpcAny),
  (pnEmail, vpcAny),
  (pnImpp, vpcAny),
  (pnLang, vpcAny),
  (pnTz, vpcAny),
  (pnGeo, vpcAny),
  (pnTitle, vpcAny),
  (pnRole, vpcAny),
  (pnLogo, vpcAny),
  (pnOrg, vpcAny),
  (pnMember, vpcAny),
  (pnRelated, vpcAny),
  (pnCategories, vpcAny),
  (pnNote, vpcAny),
  (pnProdId, vpcAtMostOne),
  (pnRev, vpcAtMostOne),
  (pnSound, vpcAny),
  (pnUid, vpcAtMostOne),
  (pnClientPidMap, vpcAny),
  (pnUrl, vpcAny),
  (pnVersion, vpcExactlyOne),
  (pnKey, vpcAny),
  (pnFbUrl, vpcAny),
  (pnCaladrUri, vpcAny),
  (pnCalUri, vpcAny)
].toTable()

const fixedValueTypeProperties = [
  (pnSource, vtUri),
  (pnKind, vtText),
  (pnXml, vtText),
  (pnFn, vtText),
  (pnNickname, vtTextList),
  (pnPhoto, vtUri),
  (pnBday, vtDateTimeOrText),
  (pnAnniversary, vtDateTimeOrText),
  (pnTel, vtTextOrUri),
  (pnEmail, vtText),
  (pnImpp, vtUri),
  (pnLang, vtText), # technically "language-tag" but the same in function
  (pnTz, vtText), # technically "uri" and "utc-offset" are possible value types
                  # for this as well, but these differences are basically
                  # ignored by this implementation. The "uri" value type was
                  # anticipating a standard that never materialized. The
                  # "utc-offset" value type is not recommended due to the
                  # realities of Daylight savings time. The actual storage type
                  # for both of those types in text (UTC offset could arguably
                  # by an integer, but the format is significant), so users of
                  # this library will still have access to the data and can
                  # query the TYPE parameter to decide how to treat it.
  (pnGeo, vtUri),
  (pnTitle, vtText),
  (pnRole, vtText),
  (pnLogo, vtUri),
  (pnOrg, vtText),
  (pnMember, vtUri),
  (pnRelated, vtTextOrUri),
  (pnCategories, vtTextList),
  (pnNote, vtText),
  (pnProdId, vtText),
  (pnSound, vtUri),
  (pnUid, vtTextOrUri),
  (pnUrl, vtUri),
  (pnVersion, vtText),
  (pnKey, vtTextOrUri),
  (pnFbUrl, vtUri),
  (pnCaladrUri, vtUri),
  (pnCalUri, vtUri)
]

const supportedParams: Table[string, HashSet[VC4_PropertyName]] = [
  ("LANGUAGE", [pnFn, pnN, pnNickname, pnBday, pnAdr, pnTitle, pnRole,
    pnLogo, pnOrg, pnRelated, pnNote, pnSound].toHashSet),

  ("PREF", (propertyCardMap.pairs.toSeq -->
    filter(it[1] == vpcAtLeastOne or it[1] == vpcAny).
    map(it[0])).
    toHashSet),

  # ("ALTID", all properties),

  ("PID", (propertyCardMap.pairs.toSeq -->
    filter((it[1] == vpcAtLeastOne or it[1] == vpcAny) and
            it[0] != pnClientPidMap).
    map(it[0])).toHashSet),

  ("TYPE", @[ pnFn, pnNickname, pnPhoto, pnAdr, pnTel, pnEmail, pnImpp, pnLang,
    pnTz, pnGeo, pnTitle, pnRole, pnLogo, pnOrg, pnRelated, pnCategories,
    pnNote, pnSound, pnUrl, pnKey, pnFburl, pnCaladrUri, pnCalUri ].toHashSet),

].toTable

const TIMESTAMP_FORMATS = [
  "yyyyMMdd'T'hhmmssZZZ",
  "yyyyMMdd'T'hhmmssZZ",
  "yyyyMMdd'T'hhmmssZ",
  "yyyyMMdd'T'hhmmss"
]

const TEXT_CHARS = WSP + NON_ASCII + { '\x21'..'\x2B', '\x2D'..'\x7E' }
const COMPONENT_CHARS = WSP + NON_ASCII +
  { '\x21'..'\x2B', '\x2D'..'\x3A', '\x3C'..'\x7E' }

macro genPropTypes(
    props: static[openarray[(VC4_PropertyName, VC4_ValueType)]]
  ): untyped =

  result = newNimNode(nnkTypeSection)

  for (pn, pt) in props:
    var name: string = $pn
    if name.len > 1: name = name[0] & name[1..^1].toLower
    let typeName = ident("VC4_" & name)

    let parentType =
      case pt
      of vtDateTimeOrText:  ident("VC4_DateTimeOrTextProperty")
      of vtText:            ident("VC4_TextProperty")
      of vtTextList:        ident("VC4_TextListProperty")
      of vtTimestamp:       ident("VC4_DateTimeProperty")
      of vtTextOrUri:       ident("VC4_TextOrUriProperty")
      of vtUri:             ident("VC4_UriProperty")
      else:
        raise newException(ValueError,
          "types for " & $pn & " properties must be hand-written")

    result.add(
      nnkTypeDef.newTree(
        nnkPostfix.newTree(ident("*"), typeName),
        newEmptyNode(),
        nnkRefTy.newTree(
          nnkObjectTy.newTree(
            newEmptyNode(),
            nnkOfInherit.newTree(parentType),
            newEmptyNode()))))

  # echo result.treeRepr

# General VCard 4 data types
type
  PidValue* = object
    sourceId*: int
    propertyId*: int

  VC4_Property* = ref object of RootObj
    propertyId: int
    group*: Option[string]
    params*: seq[VC_Param]

  VC4_DateTimeOrTextProperty* = ref object of VC4_Property
    valueType: VC4_ValueType # should only be vtDateAndOrTime or vtText
    value*: string
    year*: Option[int]
    month*: Option[int]
    day*: Option[int]
    hour*: Option[int]
    minute*: Option[int]
    second*: Option[int]
    timezone*: Option[string]

  VC4_TextListProperty* = ref object of VC4_Property
    value*: seq[string]

  VC4_TextProperty* = ref object of VC4_Property
    value*: string

  VC4_TextOrUriProperty* = ref object of VC4_Property
    isUrl: bool
    value*: string

  VC4_UriProperty* = ref object of VC4_Property
    mediaType*: Option[string]
    value*: string

  VC4_DateTimeProperty* = ref object of VC4_Property
    value*: DateTime

  VC4_Unknown* = ref object of VC4_Property
    name*: string
    value*: string

  VCard4* = ref object of VCard
    nextPropertyId: int
    content*: seq[VC4_Property]
    pidParam*: seq[PidValue]

# Hand-written implementations for more complicated property types
type
  VC4_Type* = enum
    ## Enum defining the well-known values for the TYPE parameter in general.
    ## Because the specification actually allows any value, this is provided as
    ## a convenience for accessing the standard values.
    tWork = "work",
    tHome = "home"

  VC4_TelType* = enum
    ## Enum defining the well-known values for the TYPE parameter used by the
    ## TEL property. Because the specification actually allows any value, this
    ## is provided as a convenience for accessing the standard values.
    ttWork = "work",
    ttHome = "home",
    ttText = "text",
    ttVoice = "voice",
    ttFax = "fax",
    ttCell = "cell",
    ttVideo = "video",
    ttPager = "pager",
    ttTextPhone = "textphone"

  VC4_RelatedType* = enum
    ## Enum defining the well-known values for the TYPE parameter used by the
    ## RELATED property. Because the specification actually allows any value,
    ## this is provided as a convenience for accessing the standard values.
    trContact = "contact",
    trAcquaintance = "acquaintance",
    trFriend = "friend",
    trMet = "met",
    trCoWorker = "co-worker",
    trColleague = "colleague",
    trCoResident = "co-resident",
    trNeighbor = "neighbor",
    trChild = "child",
    trParent = "parent",
    trSibling = "sibling",
    trSpouse = "spouse",
    trKin = "kin",
    trMuse = "muse",
    trCrush = "crush",
    trDate = "date",
    trSweetheart = "sweetheart",
    trMe = "me",
    trAgent = "agent",
    trEmergency = "emergency"

  VC4_N* = ref object of VC4_Property
    family*: seq[string]
    given*: seq[string]
    additional*: seq[string]
    prefixes*: seq[string]
    suffixes*: seq[string]

  VC4_Sex* = enum
    Male = "M"
    Female = "F"
    Other = "O"
    NoneNa = "N"
    Unknown = "U"

  VC4_Gender* = ref object of VC4_Property
    sex*: Option[VC4_Sex]
    genderIdentity*: Option[string]

  VC4_Adr* = ref object of VC4_Property
    poBox*: string
    ext*: string
    street*: string
    locality*: string
    region*: string
    postalCode*: string
    country*: string

  VC4_ClientPidMap* = ref object of VC4_Property
    id*: int
    uri*: string

  VC4_Rev* = ref object of VC4_Property
    value*: DateTime

# Generate all simple property types
genPropTypes(fixedValueTypeProperties)

# Internal Utility/Implementation
# =============================================================================

template takePropertyId(vc4: VCard4): int =
  vc4.nextPropertyId += 1
  vc4.nextPropertyId - 1

func flattenParameters(
    params: seq[VC_Param],
    addtlParams: varargs[VC_Param] = @[]
  ): seq[VC_Param] =

  let paramTable = newTable[string, seq[string]]()
  let allParams = params & toSeq(addtlParams)

  for p in (allParams --> filter(it.values.len > 0)):
    let pname = p.name.toUpper
    if paramTable.contains(pname):
      for v in p.values:
        if not paramTable[pname].contains(v):
          paramTable[pname].add(v)
    else:
      let values = p.values
      paramTable[pname] = values

  result = @[]
  for k, v in paramTable.pairs: result.add((k, v))

proc parseDateAndOrTime[T](
    prop: var T,
    value: string
  ): void =

  prop.value = value
  var p = VCardParser(filename: value)

  try:
    p.open(newStringStream(value))
    p.setBookmark
    prop.year = none[int]()
    prop.month = none[int]()
    prop.day = none[int]()
    prop.hour = none[int]()
    prop.minute = none[int]()
    prop.second = none[int]()
    prop.timezone = none[string]()

    if p.peek != 'T':
      # Attempt to parse the year
      if p.peek == '-': p.expect("--")
      else: prop.year = some(parseInt(p.readLen(4)))

      # Attempt to parse the month
      if DIGIT.contains(p.peek) or p.peek == '-':
        if p.peek == '-': p.expect("-")
        else: prop.month = some(parseInt(p.readLen(2)))

        # Attempt to parse the month
        if DIGIT.contains(p.peek):
          prop.day = some(parseInt(p.readLen(2)))

    if p.peek == 'T':
      p.expect("T")

      # Attempt to parse the hour
      if p.peek == '-': p.expect("-")
      else: prop.hour = some(parseInt(p.readLen(2)))

      # Attempt to parse the minute
      if DIGIT.contains(p.peek) or p.peek == '-':
        if p.peek == '-': p.expect("-")
        else: prop.minute = some(parseInt(p.readLen(2)))

        # Attempt to parse the second
        if DIGIT.contains(p.peek):
          prop.second = some(parseInt(p.readLen(2)))

          # Attempt to parse the timezone
          if {'-', '+', 'Z'}.contains(p.peek):
            try:
              p.setBookmark
              discard p.read
              var i = 0
              while i < 4 and DIGITS.contains(p.peek):
                discard p.read
                i += 1
              prop.timezone = some(p.readSinceBookmark)
            finally: p.unsetBookmark

  except ValueError, IOError, OSError:
    p.error("unable to parse date-and-or-time value: " & p.readSinceBookmark)
  finally: p.unsetBookmark

proc parseTimestamp(value: string): DateTime =
  for fmt in TIMESTAMP_FORMATS:
    try: return value.parse(fmt)
    except: discard
  raise newException(VCardParsingError, "unable to parse timestamp value: " & value)

func parsePidValues(param: VC_Param): seq[PidValue]
  {.raises:[VCardParsingError].} =

  result = @[]
  for v in param.values:
    try:
      let pieces = v.split(".")
      if pieces.len != 2: raise newException(ValueError, "")
      result.add(PidValue(
        propertyId: parseInt(pieces[0]),
        sourceId: parseInt(pieces[1])))
    except ValueError:
      raise newException(VCardParsingError, "PID value expected to be two " &
        "integers separated by '.' (2.1 for example)")

template validateType(p: VCardParser, params: seq[VC_Param], t: VC4_ValueType) =
  p.validateRequiredParameters(params, [("VALUE", $t)])

func cmp[T: VC4_Property](x, y: T): int =
  return cmp(x.pref, y.pref)

# Initializers
# =============================================================================

func addConditionalParams(prop: VC4_PropertyName, funcDef: NimNode) =

  let formalParams = funcDef[3]
  let paramsInit =
    case funcDef[6][0].kind

    # Add parameter mapping to the returned object construction.
    # Specifically this is:
    # FuncDef
    #   [6] -> Funtion body StatementList
    #     [0] -> ReturnStmt
    #       [0] -> ObjConstr
    #         [1] -> `params` value
    #           [1] -> flattenParams call
    of nnkReturnStmt: funcDef[6][0][0][1][1]

    # Add parameter mapping to the returned object construction.
    # Specifically this is:
    # FuncDef
    #   [6] -> Funtion body StatementList
    #     [0] -> Asgn
    #       [1] -> ObjConstr
    #         [1] -> params ExprColonExpr
    #           [1] -> flattenParams Call
    of nnkAsgn: funcDef[6][0][1][1][1]

    else:
      raise newException(ValueError, "cannot generate conditional params " &
        "initialization code for this function shape:\n\r " & funcDef.treeRepr)

  if supportedParams["LANGUAGE"].contains(prop):
    # Add "language" as a function parameter
    formalParams.add(newIdentDefs(
      ident("language"),
      newEmptyNode(),
      quote do: none[string]()))

    paramsInit.add(quote do:
      ("LANGUAGE", if language.isSome: @[language.get] else: @[]))

  if supportedParams["PREF"].contains(prop):
    # Add "pref" and "pids" as function parameters
    formalParams.add(newIdentDefs(
      ident("pref"),
      newEmptyNode(),
      quote do: none[int]()))

    paramsInit.add(quote do:
      ("PREF", if pref.isSome: @[$pref.get] else: @[]))


  if supportedParams["PID"].contains(prop):
    # Add "pids" as a function parameter
    formalParams.add(newIdentDefs(
      ident("pids"),
      quote do: seq[PidValue],
      quote do: @[]))

    # Add PID parameter mapping to the object construction. See the note on the
    # LANGUAGE for details on how this hooks into the AST
    paramsInit.add(quote do: ("PID", pids --> map($it)))


  if supportedParams["TYPE"].contains(prop):
    # Add "type" as a function parameter
    formalParams.add(newIdentDefs(
      ident("types"),
      quote do: seq[string],
      quote do: @[]))

    # Add TYPE parameter mapping to the object construction. See the note on
    # the LANGUAGE parameter for details on how this hooks into the AST
    paramsInit.add(quote do: ("TYPE", types))

func namesForProp(prop: VC4_PropertyName):
    tuple[enumName, typeName, initFuncName, accessorName: NimNode] =

  var name: string = $prop
  if name.len > 1: name = name[0] & name[1..^1].toLower
  return (
    ident("pn" & name),
    ident("VC4_" & name),
    ident("newVC4_" & name),
    ident(name.toLower))

macro genDateTimeOrTextPropInitializers(
    properties: static[openarray[VC4_PropertyName]]
  ): untyped =

  # TODO: the below does not provide for the case where you want to initialize
  # a property with a date-and-or-time value that is not a specific DateTime
  # instant (for example, a truncated date like "BDAY:--1224", a birthday on
  # Dec. 24th without specifying the year.
  result = newStmtList()
  for prop in properties:
    let (enumName, typeName, initFuncName, _) = namesForProp(prop)
    let datetimeFuncDef = genAstOpt({kDirtyTemplate}, enumName, initFuncName, typeName):
      func initFuncName*(
          value: DateTime,
          altId: Option[string] = none[string](),
          group: Option[string] = none[string](),
          params: seq[VC_Param] = @[]): typeName =
        return typeName(
          params: flattenParameters(params,
            ("ALTID", if altId.isSome: @[altId.get] else: @[])),
          group: group,
          value: value.format(TIMESTAMP_FORMATS[0]),
          year: some(value.year),
          month: some(ord(value.month)),
          day: some(ord(value.monthday)),
          hour: some(ord(value.hour)),
          minute: some(ord(value.minute)),
          second: some(ord(value.second)),
          timezone: some(value.format("ZZZ")),
          valueType: vtDateAndOrTime)

    let textFuncDef = genAstOpt({kDirtyTemplate}, enumName, initFuncName, typeName):
      proc initFuncName*(
          value: string,
          valueType: Option[string] = some($vtDateAndOrTime),
          altId: Option[string] = none[string](),
          group: Option[string] = none[string](),
          params: seq[VC_Param] = @[]): typeName =
        result = typeName(
          params: flattenParameters(params,
            ("ALTID", if altId.isSome: @[altId.get] else: @[])),
          group: group,
          value: value,
          valueType: vtText)

        if valueType.isNone or valueType.get == $vtDateAndOrTime:
          result.parseDateAndOrTime(value)

    addConditionalParams(prop, datetimeFuncDef)
    addConditionalParams(prop, textFuncDef)
    result.add(textFuncDef)
    result.add(datetimeFuncDef)

macro genTextPropInitializers(
    properties: static[openarray[VC4_PropertyName]]
  ): untyped =

  result = newStmtList()
  for prop in properties:
    let (enumName, typeName, initFuncName, _) = namesForProp(prop)
    let funcDef = genAstOpt({kDirtyTemplate}, enumName, initFuncName, typeName):
      func initFuncName*(
          value: string,
          altId: Option[string] = none[string](),
          group: Option[string] = none[string](),
          params: seq[VC_Param] = @[]): typeName =
        return typeName(
          params: flattenParameters(params,
            ("ALTID", if altId.isSome: @[altId.get] else: @[])),
          group: group,
          value: value)

    addConditionalParams(prop, funcDef)
    result.add(funcDef)

macro genTextListPropInitializers(
    properties: static[openarray[VC4_PropertyName]]
  ): untyped =

  result = newStmtList()

  for prop in properties:
    let (enumName, typeName, initFuncName, _) = namesForProp(prop)
    let funcDef = genAstOpt({kDirtyTemplate}, enumName, initFuncName, typeName):
      func initFuncName*(
          value: seq[string],
          altId: Option[string] = none[string](),
          group: Option[string] = none[string](),
          params: seq[VC_Param] = @[]): typeName =
        return typeName(
          params: flattenParameters(params,
            ("ALTID", if altId.isSome: @[altId.get] else: @[])),
          group: group,
          value: value)

    addConditionalParams(prop, funcDef)
    result.add(funcDef)

macro genTextOrUriPropInitializers(
    properties: static[openarray[VC4_PropertyName]]
  ): untyped =

  result = newStmtList()
  for prop in properties:
    let (enumName, typeName, initFuncName, _) = namesForProp(prop)
    let funcDef = genAstOpt({kDirtyTemplate}, enumName, initFuncName, typeName):
      func initFuncName*(
          value: string,
          isUrl = false,
          altId: Option[string] = none[string](),
          group: Option[string] = none[string](),
          params: seq[VC_Param] = @[]): typeName =
        return typeName(
          params: flattenParameters(params,
            ("ALTID", if altId.isSome: @[altId.get] else: @[])),
          isUrl: isUrl,
          group: group)

    addConditionalParams(prop, funcDef)
    result.add(funcDef)

macro genUriPropInitializers(
    properties: static[openarray[VC4_PropertyName]]
  ): untyped =

  result = newStmtList()
  for prop in properties:
    let (enumName, typeName, initFuncName, _) = namesForProp(prop)
    let funcDef = genAstOpt({kDirtyTemplate}, enumName, initFuncName, typeName):
      func initFuncName*(
          value: string,
          altId: Option[string] = none[string](),
          mediaType: Option[string] = none[string](),
          group: Option[string] = none[string](),
          params: seq[VC_Param] = @[]): typeName =
        return typeName(
          params: flattenParameters(params,
            ("ALTID", if altId.isSome: @[altId.get] else: @[])),
          group: group,
          mediaType: mediaType,
          value: value)

    addConditionalParams(prop, funcDef)
    result.add(funcDef)

genDateTimeOrTextPropInitializers(fixedValueTypeProperties -->
  filter(it[1] == vtDateTimeOrText).map(it[0]))

genTextPropInitializers(fixedValueTypeProperties -->
  filter(it[1] == vtText).map(it[0]))

genTextListPropInitializers(fixedValueTypeProperties -->
  filter(it[1] == vtTextList).map(it[0]))

genTextOrUriPropInitializers(fixedValueTypeProperties -->
  filter(it[1] == vtTextOrUri).map(it[0]))

genUriPropInitializers(fixedValueTypeProperties -->
  filter(it[1] == vtUri).map(it[0]))

func newVC4_N*(
    family: seq[string] = @[],
    given: seq[string] = @[],
    additional: seq[string] = @[],
    prefixes: seq[string] = @[],
    suffixes: seq[string] = @[],
    altId: Option[string] = none[string](),
    group: Option[string] = none[string](),
    params: seq[VC_Param] = @[]): VC4_N =

  return assignFields(
    VC4_N(params: flattenParameters(params,
      ("ALTID", if altId.isSome: @[altId.get] else: @[]))),
    group, family, given, additional, prefixes, suffixes)

func newVC4_Gender*(
    sex: Option[VC4_Sex] = none[VC4_Sex](),
    genderIdentity: Option[string] = none[string](),
    altId: Option[string] = none[string](),
    group: Option[string] = none[string](),
    params: seq[VC_Param] = @[]): VC4_Gender =

  return assignFields(
    VC4_Gender(params: flattenParameters(params,
      ("ALTID", if altId.isSome: @[altId.get] else: @[]))),
    sex, genderIdentity, group)

func newVC4_Adr*(
    poBox = "",
    ext = "",
    street = "",
    locality = "",
    region = "",
    postalCode = "",
    country = "",
    altId: Option[string] = none[string](),
    geo: Option[string] = none[string](),
    group: Option[string] = none[string](),
    label: Option[string] = none[string](),
    language: Option[string] = none[string](),
    params: seq[VC_Param] = @[],
    pids: seq[PidValue] = @[],
    pref: Option[int] = none[int](),
    types: seq[string] = @[],
    tz: Option[string] = none[string]()): VC4_Adr =

  if pref.isSome and (pref.get < 1 or pref.get > 100):
    raise newException(ValueError, "PREF must be an integer between 1 and 100")

  return assignFields(
    VC4_Adr(params: flattenParameters(params,
      ("ALTID", if altId.isSome: @[altId.get] else: @[]),
      ("GEO", if geo.isSome: @[geo.get] else: @[]),
      ("LABEL", if label.isSome: @[label.get] else: @[]),
      ("LANGUAGE", if language.isSome: @[language.get] else: @[]),
      ("PID", pids --> map($it)),
      ("PREF", if pref.isSome: @[$pref.get] else: @[]),
      ("TYPE", types),
      ("TZ", if tz.isSome: @[tz.get] else: @[]))),
    poBox, ext, street, locality, region, postalCode, country, group)

func newVC4_ClientPidMap*(
    id: int,
    uri: string,
    group: Option[string] = none[string](),
    params: seq[VC_Param] = @[]): VC4_ClientPidMap =

  result = assignFields(
    VC4_ClientPidMap(params: flattenParameters(params)),
    id, uri, group)

func newVC4_Rev*(
    value: DateTime,
    group: Option[string] = none[string](),
    params: seq[VC_Param]= @[]): VC4_Rev =

  return assignFields(
    VC4_Rev(params: flattenParameters(params)),
    value, group)

# Accessors
# =============================================================================

macro genPropAccessors(
    properties: static[openarray[(VC4_PropertyName, VC_PropCardinality)]]
  ): untyped =

  result = newStmtList()
  for (pn, pCard) in properties:
    let (_, typeName, _, funcName) = namesForProp(pn)

    case pCard:
    of vpcAtMostOne:
      let funcDef = genAstOpt({kDirtyTemplate}, funcName, pn, typeName):
        func funcName*(vc4: VCard4): Option[typeName] =
          let alts = allAlternatives[typeName](vc4)
          if alts.len > 1:
            raise newException(ValueError,
              ("VCard should have at most one $# property, but $# " &
               "distinct properties were found") % [$pn, $alts.len])

          if alts.len == 0: result = none[typeName]()
          else: result = some(alts[toSeq(alts.keys)[0]][0])

      result.add(funcDef)

    of vpcExactlyOne:
      let funcDef = genAstOpt({kDirtyTemplate}, funcName, pn, typeName):
        func funcName*(vc4: VCard4): typeName =
          let alts = allAlternatives[typeName](vc4)
          if alts.len != 1:
            raise newException(ValueError,
              "VCard should have exactly one $# property, but $# were found" %
                [$pn, $alts.len])
          result = alts[toSeq(alts.keys)[0]][0]

      result.add(funcDef)

    of vpcAtLeastOne, vpcAny:
      let funcDef = genAstOpt({kDirtyTemplate}, funcName, typeName):
        func funcName*(vc4: VCard4): seq[typeName] =
          result = findAll[typeName](vc4.content)
      result.add(funcDef)

macro genNameAccessors(propNames: static[seq[VC4_PropertyName]]): untyped =
  result = genAstOpt({kDirtyTemplate}):
    func name*(p: VC4_Property): string =
      if p of VC4_Unknown:
        return cast[VC4_Unknown](p).name

  let genericIfBlock = result[6][0]

  let memSafePNs = propNames
  let propNamesToProcess = (memSafePNs --> filter(it != pnUnknown))
  for pn in propNamesToProcess:
    let (enumName, typeName, _, _) = namesForProp(pn)

    let genericCond = nnkElifExpr.newTree(
      nnkInfix.newTree(ident("of"), ident("p"), typeName),
      quote do: return $`enumName`)

    genericIfBlock.add(genericCond)

  # echo result.repr

macro genLanguageAccessors(props: static[openarray[VC4_PropertyName]]): untyped =

  result = newStmtList()

  for p in props:
    var name = $p
    if name.len > 1: name = name[0] & name[1..^1].toLower
    let typeName = ident("VC4_" & name)

    let langFunc = genAstOpt({kDirtyTemplate}, typeName):
      func language*(prop: typeName): Option[string] =
        let langParam = prop.params --> find(it.name == "LANGUAGE")
        if langParam.isSome and langParam.get.values.len > 0:
          return some(langParam.get.values[0])
        else: return none[string]()
    result.add(langFunc)

macro genPrefAccessors(props: static[openarray[VC4_PropertyName]]): untyped =

  result = newStmtList()

  for p in props:
    var name = $p
    if name.len > 1: name = name[0] & name[1..^1].toLower
    let typeName = ident("VC4_" & name)

    let prefFunc = genAstOpt({kDirtyTemplate}, typeName):
      func pref*(prop: typeName): int =
        let prefParam = prop.params --> find(it.name == "PREF")
        if prefParam.isSome and prefParam.get.values.len > 0:
          return parseInt(prefParam.get.values[0])
        else: return 101
    result.add(prefFunc)

macro genPidAccessors(props: static[openarray[VC4_PropertyName]]): untyped =

  result = newStmtList()

  for p in props:
    var name = $p
    if name.len > 1: name = name[0] & name[1..^1].toLower
    let typeName = ident("VC4_" & name)

    let pidFunc = genAstOpt({kDirtyTemplate}, typeName):
      func pid*(prop: typeName): seq[PidValue] =
        let pidParam = prop.params --> find(it.name == "PREF")
        if pidParam.isSome: return parsePidValues(pidParam.get)
        else: return @[]
    result.add(pidFunc)

macro genTypeAccessors(props: static[openarray[VC4_PropertyName]]): untyped =

  result = newStmtList()

  for p in props:
    var name = $p
    if name.len > 1: name = name[0] & name[1..^1].toLower
    let typeName = ident("VC4_" & name)

    let typeFun = genAstOpt({kDirtyTemplate}, typeName):
      func types*(prop: typeName): seq[string] =
        let typeParam = prop.params --> find(it.name == "TYPE")
        if typeParam.isSome: return typeParam.get.values
        else: return @[]
    result.add(typeFun)

func inPrefOrder*[T: VC4_Property](props: seq[T]): seq[T] =
  return props.sorted(vcard4.cmp[T])

func altId*(p: VC4_Property): Option[string] =
  p.params.getSingleValue("ALTID")

func valueType*(p: VC4_Property): Option[string] =
  p.params.getSingleValue("VALUE")

func allAlternatives*[T](vc4: VCard4): Table[string, seq[T]] =
  result = initTable[string, seq[T]]()

  for p in vc4.content:
    if p of T:
      let altId =
        if p.altId.isSome: p.altId.get
        else: ""

      if not result.contains(altId): result[altId] = @[cast[T](p)]
      else: result[altId].add(cast[T](p))

genPropAccessors(propertyCardMap.pairs.toSeq -->
  filter(not [pnVersion, pnUnknown].contains(it[0])))

genNameAccessors(toSeq(VC4_PropertyName))

func customProp*(vc4: VCard4, name: string): seq[VC4_Unknown] =
  result = vc4.content -->
    filter(it of VC4_Unknown and it.name == name).
    map(cast[VC4_Unknown](it))

genLanguageAccessors(supportedParams["LANGUAGE"].toSeq())
genPidAccessors(supportedParams["PID"].toSeq())
genPrefAccessors(supportedParams["PREF"].toSeq())
genTypeAccessors(supportedParams["TYPE"].toSeq())

# Setters
# =============================================================================

func set*[T: VC4_Property](vc4: VCard4, content: varargs[T]): void =
  for c in content:
    var nc = c
    let existingIdx = vc4.content.indexOfIt(it of T)
    if existingIdx < 0:
      nc.propertyId = vc4.takePropertyId
      vc4.content.add(nc)
    else:
      nc.propertyId = vc4.content[existingIdx].propertyId
      vc4.content[existingIdx] = nc

func add*[T: VC4_Property](vc4: VCard4, content: varargs[T]): void =
  for c in content:
    var nc = c
    nc.propertyId = vc4.takePropertyId
    vc4.content.add(nc)

func updateOrAdd*[T: VC4_Property](vc4: VCard4, content: seq[T]): VCard4 =
  for c in content:
    let existingIdx = vc4.content.indexOfIt(it.propertyId == c.propertyId)
    if existingIdx < 0: vc4.content.add(c)
    else: c.content[existingIdx] = c

# Ouptut
# =============================================================================

func nameWithGroup(s: VC4_Property): string =
  if s.group.isSome: s.group.get & "." & s.name
  else: s.name

macro genSerializers(
    props: static[openarray[(VC4_PropertyName, VC4_ValueType)]]
  ): untyped =

  result = newStmtList()

  for (pn, pt) in props:
    let (enumName, typeName, _, _) = namesForProp(pn)

    case pt
    of vtText, vtTextOrUri, vtUri, vtDateTimeOrText:
      let funcDef = genAstOpt({kDirtyTemplate}, enumName, typeName):
        func serialize*(p: typeName): string =
          result =
            p.nameWithGroup &
            serialize(p.params) &
            ":" & serializeValue(p.value)
      result.add(funcDef)

    of vtTextList:
      let funcDef = genAstOpt({kDirtyTemplate}, enumName, typeName):
        func serialize*(p: typeName): string =
          result = p.nameWithGroup & serialize(p.params) &
            serialize(p.params) & ":" &
            (p.value --> map(serializeValue(it))).join(",")
      result.add(funcDef)

    else:
      raise newException(ValueError, "serializer for " & $pn &
        " properties must be hand-written")

macro genGenericSerializer(props: static[openarray[VC4_PropertyName]]): untyped =
  result = genAstOpt({kDirtyTemplate}):
    func serialize*(c: VC4_Property): string

  let ifExpr = nnkIfExpr.newTree()

  for p in props:
    let (_, typeName, _, _) = namesForProp(p)

    let returnStmt = genAstOpt({kDirtyTemplate}, typeName):
      return serialize(cast[typeName](c))

    ifExpr.add(nnkElifBranch.newTree(
      nnkInfix.newTree(ident("of"), ident("c"), typeName),
      returnStmt))

  result[6] = newStmtList(ifExpr)

func serializeParamValue(value: string): string =
  result = value.multiReplace([("\n", "^n"), ("^", "^^"), ("\"", "^'")])

  for c in result:
    if not SAFE_CHARS.contains(c):
      result = "\"" & result & "\""
      break

func serialize(params: seq[VC_Param]): string =
  result = ""
  for pLent in params:
    let p = pLent
    result &= ";" & p.name & "=" & (p.values -->
      map(serializeParamValue(it))).join(",")

func serializeValue(value: string): string =
  result = value.multiReplace(
    [(",", "\\,"), (";", "\\;"), ("\\", "\\\\"),("\n", "\\n")])

func serialize*(n: VC4_N): string =
  result = "N" & serialize(n.params) & ":" &
    (n.family --> map(serializeValue(it))).join(",") & ";" &
    (n.given --> map(serializeValue(it))).join(",") & ";" &
    (n.additional --> map(serializeValue(it))).join(",") & ";" &
    (n.prefixes --> map(serializeValue(it))).join(",") & ";" &
    (n.suffixes --> map(serializeValue(it))).join(",")

func serialize*(a: VC4_Adr): string =
  result = "ADR" & serialize(a.params) & ":" &
    a.poBox & ";" & a.ext & ";" & a.street & ";" & a.locality & ";" &
    a.region & ";" & a.postalCode & ";" & a.country

func serialize*(g: VC4_Gender): string =
  result = "GENDER" & serialize(g.params) & ":"
  if g.sex.isSome: result &= $g.sex.get
  if g.genderIdentity.isSome: result &= ";" & g.genderIdentity.get

func serialize*(r: VC4_Rev): string =
  result = "REV" & serialize(r.params) &
    ":" & r.value.format(TIMESTAMP_FORMATS[0])

func serialize*(c: VC4_ClientPidMap): string =
  result = "CLIENTPIDMAP" & serialize(c.params) & ":" & $c.id & ";" & c.uri

genSerializers(fixedValueTypeProperties.toSeq & @[(pnUnknown, vtText)])
genGenericSerializer(toSeq(VC4_PropertyName))

func `$`*(pid: PidValue): string = $pid.propertyId & "." & $pid.sourceId

func `$`*(vc4: VCard4): string =
  result = "BEGIN:VCARD" & CRLF
  result &= "VERSION:4.0" & CRLF
  for p in (vc4.content --> filter(not (it of VC4_Version))):
    result &= foldContentLine(serialize(p)) & CRLF
  result &= "END:VCARD" & CRLF


# Parsing
# =============================================================================

proc readParamValue(p: var VCardParser): string =
  ## Read a single parameter value at the current read position or error. Note
  ## that this implementation differs from RFC 6450 in two important ways:
  ## 1. It implements the escaping logic defined in RFC 6868 to allow newlines,
  ##    and double-quote characters to be represented in parameter values.
  ## 2. It ALWAYS treats ',' as value delimiters when outside quoted values
  ##    and NEVER treats them as delimiter within quoted values. This has been
  ##    mentioned in several errata filed for RCF 6350, but no updated document
  ##    has been released since. A decision is required in order to make the
  ##    parsing logic unambigous.

  result = newStringOfCap(32)
  let quoted = p.peek == '"'
  if quoted: discard p.read

  while (quoted and QSAFE_CHARS.contains(p.peek)) or
        (not quoted and SAFE_CHARS.contains(p.peek)):

    let c = p.read
    if c == '^':
      case p.peek
      of '^': result.add(p.read)
      of 'n', 'N':
        result.add('\n')
        discard p.read
      of '\'':
        result.add('"')
        discard p.read
      else:
        p.error("invalid character escape: '^$1'" % [$p.read])
    else: result.add(c)

  if quoted and p.read != '"':
    p.error("quoted parameter value expected to end with a " &
      "double quote (\")")

proc readParams(p: var VCardParser): seq[VC_Param] =
  result = @[]
  while p.peek == ';':
    discard p.read
    var param: VC_Param = (p.readName, @[])
    p.expect("=", true)
    param.values.add(p.readParamValue)
    while p.peek == ',':
      discard p.read
      param.values.add(p.readParamValue)
    result.add(param)

proc readComponentValue(
    p: var VCardParser,
    ignorePrefix: set[char] = {},
    requiredPrefix = none[char]()): string =
  ## Read a component value (defined by RFC6350) from the current read
  ## position. This is very similar to readTextValue. The difference is that
  ## component values cannot contain unescaped semicolons
  result = newStringOfCap(32)

  if requiredPrefix.isSome:
    if p.peek != requiredPrefix.get:
      p.error("expected to read '" & $requiredPrefix.get & "' but found '" &
        $p.read & "'")
    discard p.read

  if ignorePrefix.contains(p.peek): discard p.read
  while COMPONENT_CHARS.contains(p.peek):
    let c = p.read

    if c == '\\':
      case p.peek
      of '\\', ',', ';': result.add(p.read)
      of 'n', 'N':
        result.add('\n')
        discard p.read
      else:
        p.error("invalid character escape: '\\$1'" % [$p.read])
    else: result.add(c)

proc readComponentValueList(
    p: var VCardParser,
    seps: set[char] = {','},
    requiredPrefix = none[char]()
  ): seq[string] =
  ## Read in a list of multiple component values (defined by RFC2426) from the
  ## current read position. This is used, for example, for the N and ADR
  ## properties.

  if requiredPrefix.isSome:
    if p.peek != requiredPrefix.get:
      p.error("expected to read '" & $requiredPrefix.get & "' but found '" &
        $p.read & "'")
    discard p.read

  result = @[p.readComponentValue]
  while seps.contains(p.peek): result.add(p.readComponentValue(ignorePrefix = seps))

proc readTextValue(p: var VCardParser, ignorePrefix: set[char] = {}): string =
  ## Read a text-value (defined by RFC6350) from the current read position.
  result = newStringOfCap(32)

  if ignorePrefix.contains(p.peek): discard p.read
  while TEXT_CHARS.contains(p.peek):
    let c = p.read

    if c == '\\':
      case p.peek
      of '\\', ',': result.add(p.read)
      of 'n', 'N':
        result.add('\n')
        discard p.read
      else:
        p.error("invalid character escape: '\\$1'" % [$p.read])
    else: result.add(c)

proc readTextValueList(
    p: var VCardParser,
    seps: set[char] = {','},
    requiredPrefix = none[char]()
  ): seq[string] =
  ## Read in a list of multiple text-value (defined by RFC2426) values from the
  ## current read position. This is used, for example, for the N and ADR
  ## properties.

  if requiredPrefix.isSome:
    if p.peek != requiredPrefix.get:
      p.error("expected to read '" & $requiredPrefix.get & "' but found '" &
        $p.read & "'")
    discard p.read

  result = @[p.readTextValue]
  while seps.contains(p.peek): result.add(p.readTextValue(ignorePrefix = seps))

macro genPropParsers(
    genProps: static[openarray[(VC4_PropertyName, VC4_ValueType)]],
    group: Option[string],
    name: string,
    params: seq[VC_Param],
    contents: var seq[VC4_Property],
    p: var VCardParser
  ): untyped =

  macro ac(value: untyped): untyped =
    # Assign Common parameter values found on all properties
    result = value
    result.add(newTree(nnkExprColonExpr, ident("group"), ident("group")))
    result.add(newTree(nnkExprColonExpr, ident("params"), ident("params")))
    # echo result.treeRepr

  result = nnkCaseStmt.newTree(quote do:
    parseEnum[VC4_PropertyName](name, pnUnknown))

  for (pn, pt) in genProps:
    let (enumName, typeName, initFuncName, _) = namesForProp(pn)

    let parseCase = nnkOfBranch.newTree(quote do: `enumName`, newEmptyNode())
    result.add(parseCase)

    case pt
    of vtDateTimeOrText:
      parseCase[1] = genAst(contents, initFuncName, typeName, p):
        let valueType = params.getSingleValue("VALUE")
        if valueType.isSome and valueType.get != $vtDateAndOrTime and
           valueType.get != $vtText:
          p.error("VALUE must be either \"date-and-or-time\" or \"text\" for " &
            name & " properties.")

        contents.add(initFuncName(
          value = p.readValue,
          valueType = valueType,
          group = group,
          params = params))

    of vtText:
      parseCase[1] = genAst(contents, typeName, pt):
        p.validateType(params, pt)
        contents.add(ac(typeName(value: p.readTextValue)))

    of vtTextList:
      parseCase[1] = genAst(contents, typeName):
        p.validateType(params, vtText)
        contents.add(ac(typeName(value: p.readTextValueList)))

    of vtTextOrUri:
      parseCase[1] = genAst(contents, typeName):
        let valueType = params.getSingleValue("VALUE")
        if valueType.isNone or valueType.get == $vtUri:
          contents.add(ac(typeName(value: p.readValue)))
        elif valueType.isSome and valueType.get == $vtText:
          contents.add(ac(typeName(value: p.readTextValue)))
        else:
          p.error(("VALUE must be either \"text\" or \"uri\" for $# " &
            "properties (was $#).") % [name, $valueType])

    of vtUri:
      parseCase[1] = genAst(typeName, contents, pt):
        p.validateType(params, pt)
        contents.add(ac(typeName(value: p.readValue)))

    else:
      raise newException(ValueError, "parse statements for for " & $pn &
        " properties must be hand-written")

  block:  # N
    let parseCase = nnkOfBranch.newTree(ident("pnN"), newEmptyNode())
    result.add(parseCase)
    parseCase[1] = genAst(contents):
      p.validateType(params, vtText)
      contents.add(ac(VC4_N(
        family: p.readComponentValueList,
        given: p.readComponentValueList(requiredPrefix = some(';')),
        additional: p.readComponentValueList(requiredPrefix = some(';')),
        prefixes: p.readComponentValueList(requiredPrefix = some(';')),
        suffixes: p.readComponentValueList(requiredPrefix = some(';')))))


  block:  # GENDER
    let parseCase = nnkOfBranch.newTree(ident("pnGender"), newEmptyNode())
    result.add(parseCase)
    parseCase[1] = genAst(contents):
      p.validateType(params, vtText)
      var sex: Option[VC4_Sex] = none[VC4_Sex]()
      let sexCh = p.read
      if {'M', 'F', 'O', 'N', 'U'}.contains(sexCh):
        sex = some(parseEnum[VC4_Sex]($sexCh))
      elif sexCh != ';':
        p.error("The sex component of the GENDER property must be one of " &
          "M, F, O, N, or U (it was " & $sexCh & ")")

      contents.add(ac(VC4_Gender(
        sex: sex,
        genderIdentity:
          if sexCh == ';' or p.peek == ';':
            some(p.readTextValue(ignorePrefix = {';'}))
          else: none[string]())))

  block:  # ADR
    let parseCase = nnkOfBranch.newTree(ident("pnAdr"), newEmptyNode())
    result.add(parseCase)
    parseCase[1] = genAst(contents):
      p.validateType(params, vtText)
      contents.add(ac(VC4_Adr(
        poBox: p.readComponentValue,
        ext: p.readComponentValue(requiredPrefix = some(';')),
        street: p.readComponentValue(requiredPrefix = some(';')),
        locality: p.readComponentValue(requiredPrefix = some(';')),
        region: p.readComponentValue(requiredPrefix = some(';')),
        postalCode: p.readComponentValue(requiredPrefix = some(';')),
        country: p.readComponentValue(requiredPrefix = some(';')))))

  block: # REV
    let parseCase = nnkOfBranch.newTree(ident("pnRev"), newEmptyNode())
    result.add(parseCase)
    parseCase[1] = genAst(contents):
      p.validateType(params, vtTimestamp)
      contents.add(ac(VC4_Rev(value: parseTimestamp(p.readValue))))

  block: # CLIENTPIDMAP
    let parseCase = nnkOfBranch.newTree(ident("pnClientPidMap"), newEmptyNode())
    result.add(parseCase)
    parseCase[1] = genAst(contents):
      contents.add(ac(VC4_ClientPidMap(
        id: parseInt(p.readComponentValue),
        uri: p.readTextValue(ignorePrefix = {';'}))))

  block: # UNKNOWN
    let parseCase = nnkOfBranch.newTree(ident("pnUnknown"), newEmptyNode())
    result.add(parseCase)
    parseCase[1] = genAst(contents):
      contents.add(ac(VC4_Unknown(name: name, value: p.readValue)))
  # echo result.repr

proc parseContentLines*(p: var VCardParser): seq[VC4_Property] =
  result = @[]

  while true:
    let group = p.readGroup
    let name = p.readName
    if name == "END":
      p.expect(":VCARD" & CRLF)
      break
    let params = p.readParams
    p.expect(":")

    genPropParsers(fixedValueTypeProperties, group, name, params, result, p)

    p.expect(CRLF)


# Private Function Unit Tests
# ============================================================================
proc runVCard4PrivateTests*() =

  proc initParser(input: string): VCardParser =
    result = VCardParser(filename: "private unittests")
    lexer.open(result, newStringStream(input))

  # "readGroup":
  block:
    var p = initParser("mygroup.BEGIN:VCARD")
    let g = p.readGroup
    assert g.isSome
    assert g.get == "mygroup"

  # "all property types defined"
  block:
    assert declared(VC4_Source)
    assert declared(VC4_Kind)
    assert declared(VC4_Xml)
    assert declared(VC4_Fn)
    assert declared(VC4_N)
    assert declared(VC4_Nickname)
    assert declared(VC4_Photo)
    assert declared(VC4_Bday)
    assert declared(VC4_Anniversary)
    assert declared(VC4_Gender)
    assert declared(VC4_Adr)
    assert declared(VC4_Tel)
    assert declared(VC4_Email)
    assert declared(VC4_Impp)
    assert declared(VC4_Lang)
    assert declared(VC4_Tz)
    assert declared(VC4_Geo)
    assert declared(VC4_Title)
    assert declared(VC4_Role)
    assert declared(VC4_Logo)
    assert declared(VC4_Org)
    assert declared(VC4_Member)
    assert declared(VC4_Related)
    assert declared(VC4_Categories)
    assert declared(VC4_Note)
    assert declared(VC4_Prodid)
    assert declared(VC4_Rev)
    assert declared(VC4_Sound)
    assert declared(VC4_Uid)
    assert declared(VC4_Clientpidmap)
    assert declared(VC4_Url)
    assert declared(VC4_Version)
    assert declared(VC4_Key)
    assert declared(VC4_Fburl)
    assert declared(VC4_Caladruri)
    assert declared(VC4_Caluri)

  # "all property initializers defined"
  block:
    assert declared(new_VC4_Source)
    assert declared(new_VC4_Kind)
    assert declared(new_VC4_Xml)
    assert declared(new_VC4_Fn)
    assert declared(new_VC4_N)
    assert declared(new_VC4_Nickname)
    assert declared(new_VC4_Photo)
    assert declared(new_VC4_Bday)
    assert declared(new_VC4_Anniversary)
    assert declared(new_VC4_Gender)
    assert declared(new_VC4_Adr)
    assert declared(new_VC4_Tel)
    assert declared(new_VC4_Email)
    assert declared(new_VC4_Impp)
    assert declared(new_VC4_Lang)
    assert declared(new_VC4_Tz)
    assert declared(new_VC4_Geo)
    assert declared(new_VC4_Title)
    assert declared(new_VC4_Role)
    assert declared(new_VC4_Logo)
    assert declared(new_VC4_Org)
    assert declared(new_VC4_Member)
    assert declared(new_VC4_Related)
    assert declared(new_VC4_Categories)
    assert declared(new_VC4_Note)
    assert declared(new_VC4_Prodid)
    assert declared(new_VC4_Rev)
    assert declared(new_VC4_Sound)
    assert declared(new_VC4_Uid)
    assert declared(new_VC4_Clientpidmap)
    assert declared(new_VC4_Url)
    assert declared(new_VC4_Version)
    assert declared(new_VC4_Key)
    assert declared(new_VC4_Fburl)
    assert declared(new_VC4_Caladruri)
    assert declared(new_VC4_Caluri)
