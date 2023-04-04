import options, strutils
import ./lexer

const WSP* = {' ', '\t'}
const ALPHA_NUM* = { 'a'..'z', 'A'..'Z', '0'..'9' }

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
