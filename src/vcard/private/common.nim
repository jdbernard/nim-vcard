# Common Functionality
# © 2022-2023 Jonathan Bernard

## This module contains type definitions, constant values, and func/proc
## definitions that are used in common between both the 3.0 and 4.0 parser
## implementations.
##
## This module is not intended to be exposed to library consumers. It is
## intended to be imported by the vcard3 and vcard4 implementations. There are
## a handful of functions (under the **Public Definitions** section) that will
## be re-exported by the root `vcard` module and available on that namespace to
## users of the library.

import std/[macros, options, strutils, times, unicode]
import zero_functional
from std/sequtils import toSeq
import ./lexer

## Internal Types (used by `vcard{3,4}`)
## =====================================
type
  VC_PropCardinality* = enum
    ## enum used to define the possible cardinalities of VCard properties.
    vpcAtMostOne,
    vpcExactlyOne,
    vpcAtLeastOne
    vpcAny

## External Types (exported on `vcard`)
## ====================================
type
  VC_Param* = tuple[name: string, values: seq[string]]
    ## Representation of VCard parameter and its values.

  VCardVersion* = enum VCardV3 = "3.0", VCardV4 = "4.0" ## \
    ## enum used to differentiate VCard3 and VCard4 versions.

  VCardParser* = object of VCardLexer
    ## Common VCard parser object
    filename*: string

  VCardParsingError* = object of ValueError
    ## Error raised when invalid input is detected while parsing a VCard

  VC_XParam* = tuple[name, value: string]
    ## Representation of VCard extended parameters (starting with "X-").
    ## Because the meaning of these parameters is implementation-specific, no
    ## parsing of the parameter value is performed, it is returned verbatim.

  VCard* = ref object of RootObj
    ## Abstract base class for all VCards. `parsedVersion` can be used to
    ## interrogate any concrete instance of this class. `asVCard3` and
    ## `asVCard4` exist as convenience functions to cast an instance to one of
    ## the subclasses depending on the value of `parsedVersion`.
    parsedVersion*: VCardVersion

## Internal constants (used by `vcard{3,4}`)
## =========================================
const CRLF* = "\r\n"
const WSP* = {' ', '\t'}
const DIGIT* = { '0'..'9' }
const ALPHA_NUM* = { 'a'..'z', 'A'..'Z', '0'..'9' }
const NON_ASCII* = { '\x80'..'\xFF' }
const QSAFE_CHARS* = WSP + { '\x21', '\x23'..'\x7E' } + NON_ASCII
const SAFE_CHARS* = WSP + { '\x21', '\x23'..'\x2B', '\x2D'..'\x39', '\x3C'..'\x7E' } + NON_ASCII
const VALUE_CHAR* = WSP + { '\x21'..'\x7E' } + NON_ASCII

const DATE_FMTS = [ "yyyy-MM-dd", "yyyyMMdd" ]
const DATE_TIME_FMTS = [
  "yyyyMMdd'T'HHmmss",
  "yyyyMMdd'T'HHmmssz",
  "yyyyMMdd'T'HHmmsszzz",
  "yyyyMMdd'T'HHmmss'.'fffzzz",
  "yyyy-MM-dd'T'HH:mm:ss",
  "yyyy-MM-dd'T'HH:mm:ssz",
  "yyyy-MM-dd'T'HH:mm:sszzz",
  "yyyy-MM-dd'T'HH:mm:ss'.'fffzzz",
]

const ALL_DATE_AND_OR_TIME_FMTS = DATE_TIME_FMTS.toSeq & DATE_FMTS.toSeq

## Internal Utility/Implementation Functions
## =========================================

proc parseDateTimeStr(
    dateStr: string,
    dateFmts: openarray[string]
  ): DateTime {.inline, raises:[ValueError].} =

  for fmt in dateFmts:
    try: result = parse(dateStr, fmt)
    except ValueError: discard

  if not result.isInitialized:
    raise newException(ValueError, "cannot parse date: " & dateStr )

proc parseDate*(dateStr: string): DateTime =
  parseDateTimeStr(dateStr, DATE_FMTS)

proc parseDateTime*(dateStr: string): DateTime =
  parseDateTimeStr(dateStr, DATE_TIME_FMTS)

proc parseDateOrDateTime*(dateStr: string): DateTime =
  parseDateTimeStr(dateStr, ALL_DATE_AND_OR_TIME_FMTS)

template indexOfIt*(s, pred: untyped): int =
  var result = -1
  for idx, it {.inject.} in pairs(s):
    if pred:
      result = idx
      break
  result

template findAll*[T, VCT](c: openarray[VCT]): seq[T] =
  c.filterIt(it of typeof(T)).mapIt(cast[T](it))

template findFirst*[T, VCT](c: openarray[VCT]): Option[T] =
  let found = c.filterIt(it of typeof(T)).mapIt(cast[T](it))
  if found.len > 0: some(found[0])
  else: none[T]()

macro assignFields*(assign: untyped, fields: varargs[untyped]): untyped =
  result = assign

  for f in fields:
    let exp = newNimNode(nnkExprColonExpr)
    exp.add(f, f)
    result.add(exp)

## Internal Parsing Functionality
## ==============================

proc getSingleValue*(params: openarray[VC_Param], name: string): Option[string];

proc error*(p: VCardParser, msg: string) =
  raise newException(VCardParsingError, "$1($2, $3) Error: $4" %
    [ p.filename, $p.lineNumber, $p.getColNumber(p.pos), msg ])

proc expect*(p: var VCardParser, expected: string, caseSensitive = false) =
  ## Read `expected.len` from the parser's input stream and determine if it
  ## matches the expected value. If the parser is unable to supply
  ## `expected.len` bytes or if the value read does not match the expected
  ## value, raise a VCardParsingError via the `error` utility function
  ## reporting to the user where the input failed to match the expected value.
  ## If the expectation is met, the parser read position is advanced past the
  ## expected value, on the next byte after the expected value that was read.
  try:
    p.setBookmark

    if caseSensitive:
      for ch in expected:
        if p.read != ch: raise newException(ValueError, "")
    else:
      for rune in expected.runes:
        if p.readRune.toLower != rune.toLower:
          raise newException(ValueError, "")

  except ValueError:
    p.error("expected '$1' but found '$2':\n\t$3\n\t$4" %
      [expected, p.readSinceBookmark, p.lineVal,
       " ".repeat(p.getColNumber(p.pos) - 1) & "^\n"])

  finally: p.unsetBookmark

proc isNext*(p: var VCardParser, expected: string, caseSensitive = false): bool =
  ## Read `expected.len` from the parser's input stream and determine if it
  ## matches the expected value. Assuming the parser did not fail to read
  ## `expected.len` bytes, this will reset the parser read state to its initial
  ## position (prior to calling `isNext`). This is similar to `expect` but
  ## exhibits more "`peek`-like" behavior (doesn't disturb the read position,
  ## and doesn't fail).
  result = true
  p.setBookmark

  if caseSensitive:
    for ch in expected:
      if p.read != ch:
        result = false
        break

  else:
    for rune in expected.runes:
      if p.readRune.toLower != rune.toLower:
        result = false
        break

  p.returnToBookmark

proc readGroup*(p: var VCardParser): Option[string] =
  ## All VCARD content items can be optionally prefixed with a group name. This
  ## scans the input to see if there is a group defined at the current read
  ## location. If there is a valid group, the group name is returned and the
  ## read position is advanced past the '.' to the start of the content type
  ## name. If there is not a valid group the read position is left unchanged.

  p.setBookmark
  var ch = p.read
  while ALPHA_NUM.contains(ch): ch = p.read

  if (ch == '.'):
    result = some(readSinceBookmark(p)[0..^2])
    p.unsetBookmark
  else:
    result = none[string]()
    p.returnToBookmark

proc readName*(p: var VCardParser): string =
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

proc readValue*(p: var VCardParser): string =
  ## Read a content value at the current read position.
  p.setBookmark
  while VALUE_CHAR.contains(p.peek): discard p.read
  result = p.readSinceBookmark
  p.unsetBookmark

proc skip*(p: var VCardParser, count: int): bool =
  ## Ignore the next `count` bytes of data from the parser's underlying input
  ## stream.
  for _ in 0..<count: discard p.read

proc skip*(p: var VCardParser, expected: string, caseSensitive = false): bool =

  ## Ignore the next `expected.len` bytes of data from the parser's underlying
  ## input stream, but only if the value read matches the value provided in
  ## `expected`. In other words: read `expected.len` bytes. If they match the
  ## expectation, leave the parser read position in the new state. If they do
  ## not match, reset the parser read position to the state prior to invoking
  ## `skip`.

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

proc existsWithValue*(
    params: openarray[VC_Param],
    name, value: string,
    caseSensitive = false
  ): bool =

  ## Determine if the given parameter exists and has the expected value. By
  ## default, value checks are not case-sensitive, as most VCard3 values are not
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

proc validateNoParameters*(
    p: VCardParser,
    params: openarray[VC_Param],
    name: string
  ) =

  ## Raise a VCardParsingError if there are any parameters.
  if params.len > 0:
    p.error("no parameters allowed on the $1 content type" % [name])

proc validateRequiredParameters*(
    p: VCardParser,
    params: openarray[VC_Param],
    expectations: openarray[tuple[name: string, value: string]]
  ) =

  ## Some content types have specific allowed parameters. For example, the
  ## SOURCE content type requires that the VALUE parameter be set to "uri" if
  ## it is present. This will raise a VCardParsingError if given parameters are
  ## present with different values that expected.

  for (n, v) in expectations:
    let pv = params.getSingleValue(n)
    if pv.isSome and pv.get != v:
      p.error("parameter '$1' must have the value '$2'" % [n, v])

## Internal Serialization Utilities
## ================================

func foldContentLine*(s: string): string =
  result = ""
  var rem = s
  while rem.len > 75: # TODO: unicode multi-byte safe?
    result &= rem[0..<75] & "\r\n "
    rem = rem[75..^1]
  result &= rem


## Publicly Exported Procedure and Functions
## =========================================

proc getMultipleValues*(
    params: openarray[VC_Param],
    name: string
  ): seq[string] =

  ## Get all of the values for a given parameter in a single list. There are
  ## two patterns for multi-valued parameters defined in the VCard3 RFCs:
  ##
  ##   - TYPE=work,cell,voice
  ##   - TYPE=work;TYPE=cell;TYPE=voice
  ##
  ## Parameter values can be specific using both patterns. This method joins
  ## all defined values regardless of the pattern used to define them.

  let ps = params.toSeq
  ps -->
    filter(it.name == name).
    map(it.values).
    flatten()

proc getSingleValue*(
    params: openarray[VC_Param],
    name: string
  ): Option[string] =
  ## Get the first single value defined for a parameter.
  ##
  ## Many parameters only support a single value, depending on the content type.
  ## In order to support multi-valued parameters our implementation stores all
  ## parameters as seq[string]. This function is a convenience around that.

  let ps = params.toSeq
  let foundParam = ps --> find(it.name == name)

  if foundParam.isSome and foundParam.get.values.len > 0:
    return some(foundParam.get.values[0])
  else:
    return none[string]()

func allPropsOfType*[T, VC: VCard](vc: VC): seq[T] = findAll[T](vc)
  ## Get all instances of the requested property type present on the given
  ## VCard.
  ##
  ## This can be useful when there is some logic that hides multiple instances
  ## of a property, or returns a limited subset. For example, on 3.0 versions
  ## of VCards, this library assumes that there will only be one instance of
  ## the NAME property. The 3.0 spec implies that the NAME property should only
  ## be present at most once, but does not explicitly state this. It is
  ## possible for a 3.0 VCard to contain multiple NAME properties. using
  ## `vc3.name` will only return the first. This function allows a caller to
  ## retrieve all instances for any given property type. For example:
  ##
  ##     let vc3 = parseVCards(...)
  ##     let allNames = allPropsOfType[VC3_Name](vc3)
