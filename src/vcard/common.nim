# Common Functionality
# © 2022-2023 Jonathan Bernard

## This module contains type definitions and func/proc definitions that are
## used in common between both the 3.0 and 4.0 parser implementations.
import std/[options, sequtils]
import zero_functional
import ./private/lexer

type
  VC_Param* = tuple[name: string, values: seq[string]]
    ## Representation of vCard parameter and its values.

  VCardVersion* = enum
    ## enum used to differentiate VCard3 and VCard4 versions.
    VCardV3 = "3.0", VCardV4 = "4.0"

  VCardParser* = object of VCardLexer
    ## Common vCard parser object
    filename*: string

  VCardParsingError* = object of ValueError
    ## Error raised when invalid input is detected while parsing a vCard

  VC_XParam* = tuple[name, value: string]
    ## Representation of vCard extended parameters (starting with "X-").
    ## Because the meaning of these parameters is implementation-specific, no
    ## parsing of the parameter value is performed, it is returned verbatim.

  VCard* = ref object of RootObj
    ## Abstract base class for all vCards. `parsedVersion` can be used to
    ## interrogate any concrete instance of this class. `asVCard3` and
    ## `asVCard4` exist as convenience functions to cast an instance to one of
    ## the subclasses depending on the value of `parsedVersion`.
    parsedVersion*: VCardVersion

proc getMultipleValues*(
    params: openarray[VC_Param],
    name: string
  ): seq[string] =

  ## Get all of the values for a given parameter in a single list. There are
  ## two patterns for multi-valued parameters defined in the vCard 3.0 RFCs:
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

func allPropsOfType*[T, VC: VCard](vc: VC): seq[T] =
  ## Get all instances of the requested property type present on the given
  ## vCard.
  ##
  ## This can be useful when there is some logic that hides multiple instances
  ## of a property, or returns a limited subset. For example, on 3.0 versions
  ## of vCards, this library assumes that there will only be one instance of
  ## the NAME property. The 3.0 spec implies that the NAME property should only
  ## be present at most once, but does not explicitly state this. It is
  ## possible for a 3.0 vCard to contain multiple NAME properties. using
  ## `vc3.name` will only return the first. This function allows a caller to
  ## retrieve all instances for any given property type. For example:
  ##
  ## .. code-block:: nim
  ##   let vc3 = parseVCards(...)
  ##   let allNames = allPropsOfType[VC3_Name](vc3)
  vc.content.filterIt(it of typeof(T)).mapIt(cast[T](it))
