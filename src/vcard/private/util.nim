import sequtils, strutils, times

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

const ALL_FMTS = DATE_TIME_FMTS.toSeq & DATE_FMTS.toSeq

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
  parseDateTimeStr(dateStr, ALL_FMTS)

func foldContentLine*(s: string): string =
  result = ""
  var rem = s
  while rem.len > 75: # TODO: unicode multi-byte safe?
    result &= rem[0..<75] & "\r\n "
    rem = rem[75..^1]
  result &= rem

func unfoldContentLine*(s: string): string =
  return s.multiReplace([("\r\n ", "")])

template indexOfIt*(s, pred: untyped): int =
  var result = -1
  for idx, it {.inject.} in pairs(s):
    if pred:
      result = idx
      break
  result
