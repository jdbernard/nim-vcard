# vCard 3.0 and 4.0 Nim implementation
# © 2022 Jonathan Bernard

## The `vcard` module implements a high-performance vCard parser for both
## versions 3.0 (defined by RFCs 2425_ and  2426_) and 4.0 (defined by RFC
## 6350_)
##
## .. _2425: https://tools.ietf.org/html/rfc2425
## .. _2426: https://tools.ietf.org/html/rfc2426
## .. _6350: https://tools.ietf.org/html/rfc6350
import std/[streams, unicode]

import ./vcard/private/[internals, lexer]
import ./vcard/[common, vcard3, vcard4]

export vcard3, vcard4
export common.VC_Param,
       common.VC_XParam,
       common.VCard,
       common.VCardParsingError,
       common.VCardVersion,
       common.allPropsOfType,
       common.getMultipleValues,
       common.getSingleValue

proc add*[T](vc: VCard, content: varargs[T]): void =
  if vc.parsedVersion == VCardV3: add(cast[VCard3](vc), content)
  else: add(cast[VCard4](vc), content)

proc readVCard*(p: var VCardParser): VCard =
  # Read the preamble
  discard p.readGroup
  p.expect("begin:vcard" & CRLF)

  # Look for the version tag
  p.setBookmark
  discard p.readGroup
  if p.isNext("version:4.0"):
    result = VCard4()
    result.parsedVersion = VCardV4
  else:
    result = VCard3()
    result.parsedVersion = VCardV3
  p.returnToBookmark

  # VCard3 3.0 allows arbitrarily many empty lines after BEGIN and END
  if result.parsedVersion == VCardV3:
    while (p.skip(CRLF, true)): discard
    for content in vcard3.parseContentLines(p): result.add(content)
    while (p.skip(CRLF, true)): discard

  else:
    for content in vcard4.parseContentLines(p): result.add(content)

  if result.parsedVersion == VCardV3:
    while (p.skip(CRLF, true)): discard

proc initVCardParser*(input: Stream, filename = "input"): VCardParser =
  result.filename = filename
  lexer.open(result, input)

proc initVCardParser*(content: string, filename = "input"): VCardParser =
  initVCardParser(newStringStream(content), filename)

proc initVCardParserFromFile*(filepath: string): VCardParser =
  initVCardParser(newFileStream(filepath, fmRead), filepath)

proc parseVCards*(input: Stream, filename = "input"): seq[VCard] =
  var p = initVCardParser(input, filename)
  while p.peek != '\0': result.add(p.readVCard)

proc parseVCards*(content: string, filename = "input"): seq[VCard] =
  parseVCards(newStringStream(content), filename)

proc parseVCardsFromFile*(filepath: string): seq[VCard] =
  parseVCards(newFileStream(filepath, fmRead), filepath)
