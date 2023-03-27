import strutils

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
