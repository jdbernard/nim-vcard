import std/[streams, unicode]

type VCardLexer* = object of RootObj
  input: Stream

  buffer*: string       # buffer of bytes read
  bufStart: int         # starting boundary for the buffer
  bufEnd: int           # ending boundary for the buffer
  pos*: int             # current read position
  bookmark*: int        # bookmark to support rewind functionality
  bookmarkVal*: string  # value that has been read since the bookmark was set
  lineNumber*: int      # how many newlines have we seen so far
  lineStart: int        # index into the buffer for the start of the current line

proc skipUtf8Bom(vcl: var VCardLexer) =
  if (vcl.buffer[0] == '\xEF') and (vcl.buffer[1] == '\xBB') and (vcl.buffer[2] == '\xBF'):
    inc(vcl.pos, 3)

template wrappedIdx(idx: untyped): int = idx mod vcl.buffer.len

proc newStartIdx(vcl: VCardLexer): int =
  if vcl.bookmark > 0: vcl.bookmark else: vcl.pos

func isFull(vcl: VCardLexer): bool {.inline.} =
  return wrappedIdx(vcl.bufEnd + 1) == vcl.newStartIdx

func atEnd(vcl: VCardLexer): bool {.inline.} =
  vcl.pos == vcl.bufEnd

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
  if vcl.isFull: vcl.doubleBuffer()

  # discard used portions of the buffer
  vcl.bufStart = vcl.newStartIdx

  if vcl.bufEnd < vcl.bufStart:
    #     e       s
    # 0 1 2 3 4 5 6 7 8 9
    charsRead = vcl.input.readDataStr(vcl.buffer,
      vcl.bufEnd ..< (vcl.bufStart - 1))
    vcl.bufEnd += charsRead

  elif vcl.bufStart == 0:
    # s           e
    # 0 1 2 3 4 5 6 7 8 9
    charsRead = vcl.input.readDataStr(vcl.buffer,
      vcl.bufEnd ..< (vcl.buffer.len - 1))
    vcl.bufEnd = wrappedIdx(vcl.bufEnd + charsRead)

  else:
    #     s       e
    # 0 1 2 3 4 5 6 7 8 9
    charsRead = vcl.input.readDataStr(vcl.buffer, vcl.bufEnd..<vcl.buffer.len)
    if charsRead == vcl.buffer.len - vcl.bufEnd:
      vcl.bufEnd = vcl.input.readDataStr(vcl.buffer, 0 ..< (vcl.bufStart - 1))


proc close*(vcl: var VCardLexer) = vcl.input.close

proc open*(vcl: var VCardLexer, input: Stream, bufLen = 16384) =
  assert(bufLen > 0)
  assert(input != nil)
  vcl.input = input
  vcl.pos = 0
  vcl.bookmark = -1
  vcl.buffer = newString(bufLen)
  vcl.bufStart = 0
  vcl.bufEnd = 0
  vcl.lineNumber = 0
  vcl.lineStart = 0
  vcl.fillBuffer
  vcl.skipUtf8Bom

proc setBookmark*(vcl: var VCardLexer) =
  vcl.bookmark = vcl.pos
  vcl.bookmarkVal = newStringOfCap(32)

proc returnToBookmark*(vcl: var VCardLexer) =
  vcl.pos = vcl.bookmark
  vcl.bookmark = -1

proc unsetBookmark*(vcl: var VCardLexer) =
  vcl.bookmark = -1

proc readSinceBookmark*(vcl: var VCardLexer): string =
  return vcl.bookmarkVal
#[
  if vcl.pos < vcl.bookmark:
    #     p e   s b
    # 0 1 2 3 4 5 6 7 8 9
    result = newStringOfCap(vcl.buffer.len - vcl.bookmark + vcl.pos)
  else:
    #   s   b     p   e
    # 0 1 2 3 4 5 6 7 8 9
    result = newStringOfCap(vcl.pos - vcl.bookmark)

  let curPos = vcl.pos
  vcl.pos = vcl.bookmark
  while vcl.pos != curPos: result.add(vcl.read)
]#

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
  if vcl.atEnd: vcl.fillBuffer()

  if vcl.isLineWrap:
    vcl.pos += 3
    vcl.lineNumber += 1
    vcl.lineStart = vcl.pos
    if vcl.atEnd: vcl.fillBuffer()

  elif vcl.buffer[vcl.pos] == '\n':
    vcl.lineNumber += 1
    vcl.lineStart = wrappedIdx(vcl.pos + 1)

  result = vcl.buffer[vcl.pos]
  if not peek:
    if vcl.bookmark != -1: vcl.bookmarkVal.add(result)
    vcl.pos = wrappedIdx(vcl.pos + 1)

proc readRune*(vcl: var VCardLexer, peek = false): Rune =
  if vcl.atEnd: vcl.fillBuffer()

  if vcl.isLineWrap:
    vcl.pos += 3
    vcl.lineNumber += 1
    vcl.lineStart = vcl.pos
    if vcl.atEnd: vcl.fillBuffer()

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

proc dumpLexerState*(l: VCardLexer): string =
  result =
    "pos        = " & $l.pos & "\p" &
    "bookmark   = " & $l.bookmark & "\p" &
    "lineNumber = " & $l.lineNumber & "\p" &
    "lineStart  = " & $l.lineStart & "\p" &
    "bufStart   = " & $l.bufStart & "\p" &
    "bufEnd     = " & $l.bufEnd & "\p" &
    "buffer     = " & l.buffer & "\p"

## Unit Tests
## ============================================================================
proc runVcardLexerPrivateTests*() =

  const longTestString =
    "This is my test string. There are many like it but this one is mine."

  proc bufferIs(vcl: VCardLexer, s: string): bool =
    #debugEcho vcl.buffer & " : " & $vcl.bufStart & "-" & $vcl.bufEnd
    # for i in vcl.bufStart..<vcl.bufEnd:
    #   debugEcho $i & ": " & vcl.buffer[i]

    for i in 0..<s.len:
      # debugEcho "i:" & $i & "\tl.bufStart:" & $(vcl.bufStart + i)
      # debugEcho s[i] & " == " & vcl.buffer[vcl.bufStart + i]
      if s[i] != vcl.buffer[wrappedIdx(vcl.bufStart + i)]:
        return false
    return true

  proc readExpected(vcl: var VCardLexer, s: string): bool =
    for i in 0..<s.len:
      if vcl.read != s[i]:
        return false
    return true

  # "can open and fill buffer":
  block:
    var l: VCardLexer
    l.open(newStringStream("test"))
    assert l.bufferIs("test")
    assert not l.isFull
    assert l.readExpected("test")

  # "refills buffer when emptied":
  block:
    var l: VCardLexer
    l.open(newStringStream("test"), 3)
    assert l.bufferIs("te")
    assert l.isFull
    assert l.read == 't'
    assert l.read == 'e'
    assert l.read == 's'
    assert l.bufferIs("st")
    assert l.read == 't'

  # "isFull correctness":
  block:
    var l = VCardLexer(
      pos: 0,
      bookmark: -1,
      buffer: "0123456789",
      bufStart: 0,
      bufEnd: 9)

    # s                 e
    # 0 1 2 3 4 5 6 7 8 9
    assert l.isFull

    # s p               e
    # 0 1 2 3 4 5 6 7 8 9
    discard l.read
    assert not l.isFull

    #     e s
    # 0 1 2 3 4 5 6 7 8 9
    l.bufStart = 3
    l.pos = 3
    l.bufEnd = 2
    assert l.isFull

    #     e s p
    # 0 1 2 3 4 5 6 7 8 9
    discard l.read
    assert l.pos == 4
    assert not l.isFull

    #                 e s
    # 0 1 2 3 4 5 6 7 8 9
    l.bufStart = 9
    l.pos = 9
    l.bufEnd = 8
    assert l.isFull

    # p               e s
    # 0 1 2 3 4 5 6 7 8 9
    discard l.read
    assert l.pos == 0
    assert not l.isFull

  # "handles wrapped lines":
  block:
    var l: VCardLexer
    l.open(newStringStream("line\r\n  wrap\r\nline 2"), 3)

    assert l.readExpected("line wrap\r\nline 2")

  # "fillBuffer correctness":
  block:
    var l: VCardLexer
    l.open(newStringStream(longTestString), 5)
    assert l.bufferIs(longTestString[0..<4])
    assert l.isFull
    assert l.bufStart == 0
    assert l.bufEnd == 4
    assert l.pos == 0
    assert l.readExpected("Th")
    assert not l.isFull
    assert not l.atEnd
    assert l.pos == 2

    l.fillBuffer
    assert l.isFull
    assert l.bufEnd == 1
    assert l.pos == 2
    assert l.bufStart == 2

  # "bookmark preserves the buffer":
  block:
    var l: VCardLexer
    l.open(newStringStream(longTestString), 7)
    assert l.buffer.len == 7
    assert l.bufferIs(longTestString[0..<6])
    assert l.isFull
    assert l.bufEnd == 6
    assert l.pos == 0
    assert l.bookmark == -1
    assert l.readExpected(longTestString[0..<5])
    assert not l.isFull
    assert not l.atEnd
    assert l.pos == 5

    l.setBookmark
    # read enough to require us to refill the buffer.
    assert l.bookmark == 5
    assert l.readExpected(longTestString[5..<10])
    assert l.pos == 3
    assert newStartIdx(l) == 5
    assert l.buffer.len == 7

    l.returnToBookmark
    assert l.bookmark == -1
    assert l.pos == 5

  # "readRune":
  block:
    var l: VCardLexer
    l.open(newStringStream("TEST"))
    assert l.bufferIs("TEST")
    assert l.peekRune == Rune('T')
    assert l.readRune == Rune('T')
    assert l.readRune == Rune('E')
    assert l.readRune == Rune('S')
    assert l.readRune == Rune('T')

when isMainModule: runVcardLexerTests()
