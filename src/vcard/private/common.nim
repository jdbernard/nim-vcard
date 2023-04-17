import std/[macros, options, strutils, unicode]
import zero_functional
from std/sequtils import toSeq
import ./lexer

type
  VCardVersion* = enum VCardV3 = "3.0", VCardV4 = "4.0"

  VCardParser* = object of VCardLexer
    filename*: string

  VCParam* = tuple[name: string, values: seq[string]]

  VCardParsingError* = object of ValueError

  VC_XParam* = tuple[name, value: string]

  VCard* = ref object of RootObj
    parsedVersion*: VCardVersion

const CRLF* = "\r\n"
const WSP* = {' ', '\t'}
const DIGIT* = { '0'..'9' }
const ALPHA_NUM* = { 'a'..'z', 'A'..'Z', '0'..'9' }
const NON_ASCII* = { '\x80'..'\xFF' }
const QSAFE_CHARS* = WSP + { '\x21', '\x23'..'\x7E' } + NON_ASCII
const SAFE_CHARS* = WSP + { '\x21', '\x23'..'\x2B', '\x2D'..'\x39', '\x3C'..'\x7E' } + NON_ASCII
const VALUE_CHAR* = WSP + { '\x21'..'\x7E' } + NON_ASCII

# Internal Utility/Implementation
# =============================================================================

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

# Output
# =============================================================================

func serialize*(s: seq[VC_XParam]): string =
  result = ""
  for x in s: result &= ";" & x.name & "=" & x.value

# Parsing
# =============================================================================

proc error*(p: VCardParser, msg: string) =
  raise newException(VCardParsingError, "$1($2, $3) Error: $4] " %
    [ p.filename, $p.lineNumber, $p.getColNumber(p.pos), msg ])

proc isNext*[T](p: var T, expected: string, caseSensitive = false): bool =
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

proc expect*[T](p: var T, expected: string, caseSensitive = false) =
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

proc readGroup*[T](p: var T): Option[string] =
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
  for _ in 0..<count: discard p.read

proc skip*(p: var VCardParser, expected: string, caseSensitive = false): bool =
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
    params: openarray[VCParam],
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

proc getMultipleValues*(
    params: openarray[VCParam],
    name: string
  ): seq[string] =

  ## Get all of the values for a given parameter in a single list. There are
  ## two patterns for multi-valued parameters defined in the VCard3 RFCs:
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

proc getSingleValue*(
    params: openarray[VCParam],
    name: string
  ): Option[string] =
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

proc validateNoParameters*(
    p: VCardParser,
    params: openarray[VCParam],
    name: string
  ) =

  ## Error unless there are no defined parameters
  if params.len > 0:
    p.error("no parameters allowed on the $1 content type" % [name])

proc validateRequiredParameters*(
    p: VCardParser,
    params: openarray[VCParam],
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
