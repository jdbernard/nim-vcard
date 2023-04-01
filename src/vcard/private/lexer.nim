import std/[streams, unicode]

type VCardLexer* = object of RootObj
  input: Stream

  pos*: int         # current read position
  bookmark*: int    # bookmark to support rewind functionality
  buffer*: string   # buffer of bytes read
  lineNumber*: int  # how many newlines have we seen so far
  lineStart: int    # index into the buffer for the start of the current line

  bufStart: int     # starting boundary for the buffer
  bufEnd: int       # ending boundary for the buffer

proc skipUtf8Bom(vcl: var VCardLexer) =
  if (vcl.buffer[0] == '\xEF') and (vcl.buffer[1] == '\xBB') and (vcl.buffer[2] == '\xBF'):
    inc(vcl.pos, 3)

proc newStartIdx(vcl: VCardLexer): int =
  if vcl.bookmark > 0: vcl.bookmark else: vcl.pos

proc doubleBuffer(vcl: var VCardLexer) =
  let oldBuf = vcl.buffer
  vcl.buffer = newString(oldBuf.len * 2)

  var newIdx = 0
  var oldIdx = vcl.bufStart

  while oldIdx != vcl.bufEnd or newIdx == 0:
    vcl.buffer[newIdx] = oldBuf[oldIdx]
    inc(newIdx)
    oldIdx = (newIdx + vcl.bufStart) mod oldBuf.len

  vcl.pos -= vcl.bufStart
  vcl.lineStart -= vcl.bufStart
  if vcl.bookmark >= 0: vcl.bookmark -= vcl.bufStart
  vcl.bufStart = 0
  vcl.bufEnd = newIdx

proc fillBuffer(vcl: var VCardLexer) =

  var charsRead: int

  # check to see if we have a full buffer
  if (vcl.bufStart == 0 and vcl.bufEnd == vcl.buffer.len) or
     vcl.bufEnd == vcl.bufStart - 1:
    vcl.doubleBuffer()

  # discard used portions of the buffer
  vcl.bufStart = vcl.newStartIdx

  if vcl.bufEnd < vcl.bufStart:
    charsRead = vcl.input.readDataStr(vcl.buffer, vcl.bufEnd ..< vcl.bufStart)
    vcl.bufEnd += charsRead
  else:
    charsRead = vcl.input.readDataStr(vcl.buffer, vcl.bufEnd ..< vcl.buffer.len)
    vcl.bufEnd += charsRead
    if charsRead == vcl.buffer.len - vcl.bufEnd:
      vcl.bufEnd = vcl.input.readDataStr(vcl.buffer, 0 ..< vcl.bufStart)

proc close*(vcl: var VCardLexer) = vcl.input.close

proc open*(vcl: var VCardLexer, input: Stream, bufLen = 16384) =
  assert(bufLen > 0)
  assert(input != nil)
  vcl.input = input
  vcl.pos = 0
  vcl.bookmark = -1
  vcl.buffer = newString(bufLen)
  vcl.lineNumber = 0
  vcl.lineStart = 0
  vcl.fillBuffer
  vcl.skipUtf8Bom

proc setBookmark*(vcl: var VCardLexer) =
  vcl.bookmark = vcl.pos

proc returnToBookmark*(vcl: var VCardLexer) =
  vcl.pos = vcl.bookmark
  vcl.bookmark = -1

proc unsetBookmark*(vcl: var VCardLexer) =
  vcl.bookmark = -1

proc readSinceBookmark*(vcl: var VCardLexer): string =
  if vcl.pos < vcl.bookmark:
    vcl.buffer[vcl.bookmark ..< vcl.buffer.len] & vcl.buffer[0 ..< vcl.pos]
  else: vcl.buffer[vcl.pos ..< vcl.bookmark]

template wrappedIdx(idx: untyped): int = idx mod vcl.buffer.len

proc isLineWrap(vcl: var VCardLexer, allowRefill = true): bool =
  if vcl.buffer[vcl.pos] != '\r': return false

  # less than three characters in the buffer
  if wrappedIdx(vcl.pos + 3) > vcl.bufEnd:
    if allowRefill:
      vcl.fillBuffer()
      return vcl.isLineWrap(false)
    else: return false

  # at least three characters in the buffer
  else:
    return vcl.buffer[wrappedIdx(vcl.pos + 1)] == '\n' and
           vcl.buffer[wrappedIdx(vcl.pos + 2)] == ' '

proc read*(vcl: var VCardLexer, peek = false): char =
  if vcl.pos == vcl.bufEnd: vcl.fillBuffer()

  if vcl.isLineWrap:
    vcl.pos += 3
    vcl.lineNumber += 1
    vcl.lineStart = vcl.pos
    if vcl.pos == vcl.bufEnd: vcl.fillBuffer()

  elif vcl.buffer[vcl.pos] == '\n':
    vcl.lineNumber += 1
    vcl.lineStart = wrappedIdx(vcl.pos + 1)

  result = vcl.buffer[vcl.pos]
  if not peek: vcl.pos = wrappedIdx(vcl.pos + 1)

proc readRune*(vcl: var VCardLexer, peek = false): Rune =
  if vcl.pos == vcl.bufEnd: vcl.fillBuffer()

  if vcl.isLineWrap:
    vcl.pos += 3
    vcl.lineNumber += 1
    vcl.lineStart = vcl.pos
    if vcl.pos == vcl.bufEnd: vcl.fillBuffer()

  elif vcl.buffer[vcl.pos] == '\n':
    vcl.lineNumber += 1
    vcl.lineStart = wrappedIdx(vcl.pos + 1)

  result = vcl.buffer.runeAt(vcl.pos)
  if not peek: vcl.pos += vcl.buffer.runeLenAt(vcl.pos)

proc peek*(vcl: var VCardLexer): char =
  return vcl.read(peek = true)

proc peekRune*(vcl: var VCardLexer): Rune =
  return vcl.readRune(peek = true)

proc getColNumber*(vcl: VCardLexer, pos: int): int =
  if vcl.lineStart < pos: return pos - vcl.lineStart
  else: return (vcl.buffer.len - vcl.lineStart) + pos
