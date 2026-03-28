# Common Functionality
# © 2022-2023 Jonathan Bernard

## This module contains type definitions, constant values, and func/proc
## definitions that are used in common between both the 3.0 and 4.0 parser
## implementations.
##
## This module is not intended to be exposed to library consumers. It is
## intended to be imported by the vcard3 and vcard4 implementations.

import std/[macros, options, strutils, times, unicode]
import zero_functional
from std/sequtils import toSeq

import ./lexer
import ../common

# Internal Types (used by `vcard{3,4}`)
# =====================================
type
  VC_PropCardinality* = enum
    ## enum used to define the possible cardinalities of vCard properties.
    vpcAtMostOne,
    vpcExactlyOne,
    vpcAtLeastOne
    vpcAny

# Internal constants (used by `vcard{3,4}`)
# =========================================
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

# Internal Utility/Implementation Functions
# =========================================

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

# Internal Parsing Functionality
# ==============================

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
  ## All vCard content items can be optionally prefixed with a group name. This
  ## scans the input to see if there is a group defined at the current read
  ## location. If there is a valid group, the group name is returned and the
  ## read position is advanced past the '.' to the start of the content type
  ## name. If there is not a valid group the read position is left unchanged.

  p.setBookmark
  let validChars = ALPHA_NUM + {'-'}
  var ch = p.read
  while validChars.contains(ch): ch = p.read

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

# Internal Serialization Utilities
# ================================

func foldContentLine*(s: string): string =
  result = ""
  var rem = s
  while rem.len > 75: # TODO: unicode multi-byte safe?
    result &= rem[0..<75] & "\r\n "
    rem = rem[75..^1]
  result &= rem
