# vCard-specific Lexer
# © 2022-2023 Jonathan Bernard

## This module defines a lexer with functionality useful for parsing vCard
## content. Specifically:
## - it understands the vCard line-folding logic and transparently joins folded
##   lines as it read input off its input stream.
## - it supports multiple nested bookmarks to make look-ahead decisions more
##   convenient
## - it supports reading from the underlying stream byte-wise or as unicode
##   runes.
##
## This parser uses a ring buffer underneath, only growing the size of its
## buffer when it is completely full.

import std/[streams, unicode]

type VCardLexer* = object of RootObj
  input: Stream

  buffer*: string           ## buffer of bytes read
  bufStart: int             ## starting boundary for the buffer
  bufEnd: int               ## ending boundary for the buffer
  pos*: int                 ## current read position
  bookmark*: seq[int]       ## bookmark to support rewind functionality
  bookmarkVal*: seq[string] ## value read since the bookmark was set
  lineNumber*: int          ## how many newlines have we seen so far
  lineStart: int            ## buffer index buffer for the start of the current line
  lineVal*: string          ## value read since the start of the current line

proc skipUtf8Bom(vcl: var VCardLexer) =
  if (vcl.buffer[0] == '\xEF') and (vcl.buffer[1] == '\xBB') and (vcl.buffer[2] == '\xBF'):
    inc(vcl.pos, 3)

template wrappedIdx(idx: untyped): int = idx mod vcl.buffer.len
  ## Map an index into the buffer bounds (mod)

proc newStartIdx(vcl: VCardLexer): int =
  ## Get the latest safe index to use as a new start index. The implication is
  ## that anything prior to this index has been read and processed and can be
  ## safely overwritten in the buffer.
  if vcl.bookmark.len > 0: vcl.bookmark[0] else: vcl.pos

func isFull(vcl: VCardLexer): bool {.inline.} =
  return wrappedIdx(vcl.bufEnd + 1) == vcl.newStartIdx

func atEnd(vcl: VCardLexer): bool {.inline.} =
  vcl.pos == vcl.bufEnd

proc doubleBuffer(vcl: var VCardLexer) =
  ## Double the capacity of the buffer, copying the contents of the current
  ## buffer into the beginning of the newly expanded buffer.
  let oldBuf = vcl.buffer
  vcl.buffer = newString(oldBuf.len * 2)

  var newIdx = 0
  var oldIdx = vcl.bufStart

  while oldIdx != vcl.bufEnd or newIdx == 0:
    vcl.buffer[newIdx] = oldBuf[oldIdx]
    inc(newIdx)
    oldIdx = (newIdx + vcl.bufStart) mod oldBuf.len

  # We know that for all existing indices, their location in the new buffer can
  # be calculated as a function of the distance we moved the start of the
  # buffer: `idx = idx + (newBufStart - oldBufStart)`. Since we know the new
  # bufStart will be 0, we know that we can calculate all of the new indices as
  # `idx -= oldBufStart` Currently vcl.bufStart is still the old bufStart.
  vcl.pos -= vcl.bufStart
  vcl.lineStart -= vcl.bufStart
  if vcl.bookmark.len > 0: vcl.bookmark[0] -= vcl.bufStart

  # Now that we've updated all of the existing indices, we can reset the
  # buffer start and end indices to their new values.
  vcl.bufStart = 0
  vcl.bufEnd = newIdx

proc fillBuffer(vcl: var VCardLexer) =
  ## Read data into the buffer from the underlying stream until the buffer is
  ## full. If the buffer is already full, double the buffer beforehand.

  # Note that we do not *completely* fill the buffer. We always leave one index
  # of the array empty. This allows us to differentiate between an empty buffer
  # (`bufStart == bufEnd`) and a completly full buffer (`bufStart ==
  # wrappedIdx(bufEnd + 1)`).

  var charsRead: int

  # check to see if we have a full buffer
  if vcl.isFull: vcl.doubleBuffer()

  # discard used portions of the buffer
  vcl.bufStart = vcl.newStartIdx

  # We have three conditions that the ring buffer may be in:
  if vcl.bufEnd < vcl.bufStart:
    # The unused portion of the buffer is all in the middle and we can just
    # read date into the space from bufEnd (e) to bufStart (s).
    #     e       s
    # 0 1 2 3 4 5 6 7 8 9
    charsRead = vcl.input.readDataStr(vcl.buffer,
      vcl.bufEnd ..< (vcl.bufStart - 1))
    vcl.bufEnd += charsRead

  elif vcl.bufStart == 0:
    # The unused portion is entirely at the end of the buffer. We can read data
    # from the bufEnd (e) to the end of our buffer capacity.
    # s           e
    # 0 1 2 3 4 5 6 7 8 9
    charsRead = vcl.input.readDataStr(vcl.buffer,
      vcl.bufEnd ..< (vcl.buffer.len - 1))
    vcl.bufEnd = wrappedIdx(vcl.bufEnd + charsRead)

  else:
    # The used portion of the buffer is in the middle, and the unused portion
    # is on either side ot that. We need to read from bufEnd (e) to the end of
    # our underlying buffer, and then from the start of our underlying buffer
    # to bufStart (s)
    #     s       e
    # 0 1 2 3 4 5 6 7 8 9
    charsRead = vcl.input.readDataStr(vcl.buffer, vcl.bufEnd..<vcl.buffer.len)
    if charsRead == vcl.buffer.len - vcl.bufEnd:
      # Only read into the front part of the buffer if we were able to
      # completely fill the back part.
      vcl.bufEnd = vcl.input.readDataStr(vcl.buffer, 0 ..< (vcl.bufStart - 1))


proc close*(vcl: var VCardLexer) = vcl.input.close
  ## Close this VCardLexer and its underlying stream.

proc open*(vcl: var VCardLexer, input: Stream, bufLen = 16384) =
  ## Open the given stream and initialize the given VCardLexer to read from it.
  assert(bufLen > 0)
  assert(input != nil)
  vcl.input = input
  vcl.pos = 0
  vcl.bookmark = @[]
  vcl.buffer = newString(bufLen)
  vcl.bufStart = 0
  vcl.bufEnd = 0
  vcl.lineNumber = 0
  vcl.lineStart = 0
  vcl.fillBuffer
  vcl.skipUtf8Bom

proc setBookmark*(vcl: var VCardLexer) =
  ## Set a bookmark into the lexer's buffer. This will prevent the lexer from
  ## discarding data from this bookmark forward when it refills.
  ##
  ## The bookmark must be cleared using either `returnToBookmark` or
  ## `unsetBookmark`, otherwise this lexer will no function as a streaming
  ## lexer and will end up reading the entire remainder of the input stream
  ## into memory (if it can).
  ##
  ## This function can be called multiple times in order to create nested
  ## bookmarks. For example, we might set a bookmark at the beginning of a line
  ## to be able to reset if we fail to parse the line, then set a bookmark
  ## midway through when attempting to parse a parameter value. Care should be
  ## taken when nesting bookmarks as all bookmarks must be released to avoid
  ## the behavior described above.
  vcl.bookmark.add(vcl.pos)
  vcl.bookmarkVal.add(newStringOfCap(32))

proc returnToBookmark*(vcl: var VCardLexer) =
  ## Unset the most recent bookmark, resetting the lexer's read position to the
  ## position of the bookmark.
  if vcl.bookmark.len == 0: return
  vcl.pos = vcl.bookmark.pop()
  let valRead = vcl.bookmarkVal.pop()
  for idx in 0..<vcl.bookmarkVal.len:
    if vcl.bookmarkVal[idx].len > valRead.len:
      vcl.bookmarkVal[idx] = vcl.bookmarkVal[idx][0 ..< ^valRead.len]

proc unsetBookmark*(vcl: var VCardLexer) =
  ## Discard the most recent bookmark, leaving the lexer's read position at its
  ## current position..
  if vcl.bookmark.len == 0: return
  discard vcl.bookmark.pop()
  discard vcl.bookmarkVal.pop()

proc readSinceBookmark*(vcl: var VCardLexer): string =
  ## Get the value read since the last bookmark.
  if vcl.bookmarkVal.len > 0:
    return vcl.bookmarkVal[^1]
  else: return ""

proc isLineWrap(vcl: var VCardLexer, allowRefill = true): bool =
  ## Answers the question "is this a folded line"? Transparently handles the
  ## case where it needs to refill the buffer to answer this question.
  if vcl.buffer[vcl.pos] != '\r': return false

  # less than three characters in the buffer
  if wrappedIdx(vcl.pos + 3) > vcl.bufEnd:
    # Only try to refill the buffer once. If we re-enter and still don't have
    # three characters, we know we were unable to fill the buffer and are
    # likely at the end.
    if allowRefill:
      vcl.fillBuffer()
      return vcl.isLineWrap(false)
    else: return false

  # at least three characters in the buffer
  else:
    return vcl.buffer[wrappedIdx(vcl.pos + 1)] == '\n' and
           vcl.buffer[wrappedIdx(vcl.pos + 2)] in {' ', '\t'}

proc read*(vcl: var VCardLexer, peek = false): char =
  ## Read one byte off of the input stream. By default this will advance the
  ## lexer read position by one byte. If `peek` is set to `true`, this will
  ## leave the read position at the same logical position. The underlying
  ## buffer position may still change if, for example, the next byte is the
  ## beginning of a folded line wrap. In this case the internal buffer position
  ## will advance past that line wrap.

  if vcl.atEnd: vcl.fillBuffer()

  if vcl.isLineWrap:
    vcl.pos += 3
    vcl.lineNumber += 1
    vcl.lineStart = vcl.pos
    vcl.lineVal = newStringOfCap(84)
    if vcl.atEnd: vcl.fillBuffer()

  elif vcl.buffer[vcl.pos] == '\n' and not peek:
    vcl.lineNumber += 1
    vcl.lineStart = wrappedIdx(vcl.pos + 1)
    vcl.lineVal = newStringOfCap(84)

  result = vcl.buffer[vcl.pos]
  if not peek:
    for idx in 0..<vcl.bookmarkVal.len: vcl.bookmarkVal[idx].add(result)
    vcl.lineVal.add(result)
    vcl.pos = wrappedIdx(vcl.pos + 1)

proc readLen*(vcl: var VCardLexer, bytesToRead: int, peek = false): string =
  ## Convenience procedure to read multiple bytes (if able) and return the
  ## value read.
  result = newStringOfCap(bytesToRead)
  for i in 0..<bytesToRead: result.add(vcl.read)

proc readRune*(vcl: var VCardLexer, peek = false): Rune =
  ## Read one unicode rune off of the input stream. By default this will
  ## advance the lexer read position by the byte length of the rune read. If
  ## `peek` is set to `true`, this will leave the read position at the same
  ## logical position. The underlying buffer position may still change if, for
  ## example, the next rune is the beginning of a folded line wrap. In this
  ## case the internal buffer position will advance past that line wrap.

  if vcl.atEnd: vcl.fillBuffer()

  if vcl.isLineWrap:
    vcl.pos += 3
    vcl.lineNumber += 1
    vcl.lineStart = vcl.pos
    vcl.lineVal = newStringOfCap(84)
    if vcl.atEnd: vcl.fillBuffer()

  elif vcl.buffer[vcl.pos] == '\n':
    vcl.lineNumber += 1
    vcl.lineStart = wrappedIdx(vcl.pos + 1)
    vcl.lineVal = newStringOfCap(84)

  result = vcl.buffer.runeAt(vcl.pos)
  if not peek:
    for idx in 0..<vcl.bookmarkVal.len: vcl.bookmarkVal[idx].add(result)
    vcl.lineVal.add(result)
    vcl.pos += vcl.buffer.runeLenAt(vcl.pos)

proc readRunesLen*(vcl: var VCardLexer, runesToRead: int, peek = false): string =
  ## Convenience procedure to read multiple runes (if able) and return the
  ## value read.
  result = newStringOfCap(runesToRead * 4)
  for i in 0..<runesToRead: result.add(vcl.readRune)

proc peek*(vcl: var VCardLexer): char =
  ## Convenience method to call `read(peek = true)`
  return vcl.read(peek = true)

proc peekRune*(vcl: var VCardLexer): Rune =
  ## Convenience method to call `read(peek = true)`
  return vcl.readRune(peek = true)

proc getColNumber*(vcl: VCardLexer, pos: int): int =
  ## Calculate the column number of the lexer's current read position relative
  ## to the start of the most recent line.
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
      bookmark: @[],
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

  # "handles wrapped lines with tabs":
  block:
    var l: VCardLexer
    l.open(newStringStream("line\r\n\twrap\r\nline 2"), 3)

    assert l.readExpected("linewrap\r\nline 2")

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
    assert l.bookmark == @[]
    assert l.readExpected(longTestString[0..<5])
    assert not l.isFull
    assert not l.atEnd
    assert l.pos == 5

    l.setBookmark
    # read enough to require us to refill the buffer.
    assert l.bookmark == @[5]
    assert l.readExpected(longTestString[5..<10])
    assert l.pos == 3
    assert newStartIdx(l) == 5
    assert l.buffer.len == 7

    l.returnToBookmark
    assert l.bookmark == @[]
    assert l.pos == 5

  # "can set and unset multiple bookmarks"
  block:
    var l: VCardLexer
    l.open(newStringStream(longTestString))
    assert l.pos == 0
    assert l.bookmark == @[]
    assert l.readExpected("This is my ")

    l.setBookmark
    assert l.bookmark == @[11]
    assert l.bookmarkVal == @[""]

    assert l.readExpected("test string")
    assert l.bookmark == @[11]
    assert l.bookmarkVal == @["test string"]
    assert l.readSinceBookmark == "test string"

    l.setBookmark
    assert l.bookmark == @[11, 22]
    assert l.bookmarkVal == @["test string", ""]

    assert l.readExpected(". There are many")
    assert l.bookmarkVal == @["test string. There are many", ". There are many"]
    assert l.readSinceBookmark == ". There are many"
    assert l.pos == 38

    l.unsetBookmark
    assert l.pos == 38
    assert l.bookmark == @[11]
    assert l.bookmarkVal == @["test string. There are many"]
    assert l.readSinceBookmark == "test string. There are many"

    l.unsetBookmark
    assert l.pos == 38
    assert l.bookmark == @[]
    assert l.bookmarkVal == @[]
    assert l.readSinceBookmark == ""

  # "can set and return to multiple bookmarks"
  block:
    var l: VCardLexer
    l.open(newStringStream(longTestString))
    assert l.pos == 0
    assert l.bookmark == @[]
    assert l.readExpected("This is my ")

    l.setBookmark
    assert l.readExpected("test string")
    l.setBookmark
    assert l.bookmark == @[11, 22]
    assert l.readExpected(". There are many")
    assert l.bookmarkVal == @["test string. There are many", ". There are many"]
    assert l.pos == 38

    l.returnToBookmark
    assert l.pos == 22
    assert l.bookmark == @[11]
    assert l.bookmarkVal == @["test string"]
    assert l.readSinceBookmark == "test string"

    l.returnToBookmark
    assert l.pos == 11
    assert l.bookmark == @[]
    assert l.bookmarkVal == @[]

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

when isMainModule: runVcardLexerPrivateTests()
