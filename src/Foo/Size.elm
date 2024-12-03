module Foo.Size exposing (Size, empty, match, fromInt, fromString, toString, decoder, encode)

import Json.Decode as D
import Json.Encode as E

-- Why can not expose variants?
-- Size
type Size
  = Empty
  | NotDigits String
  | OutOfRange Int
  | Ok Int

empty : Size
empty = Empty

match : Size
  -> (a)
  -> (String -> a)
  -> (Int -> a)
  -> (Int -> a)
  -> a
match
  size
  whenEmpty
  whenNotDigits
  whenOutOfRange
  whenOk
  = case size of
    Empty -> (whenEmpty)
    NotDigits string -> (whenNotDigits string)
    OutOfRange integer -> (whenOutOfRange integer)
    Ok integer -> (whenOk integer)

fromInt : Int -> Size
fromInt integer =
  if 0 < integer && integer < 10 then
    Ok integer
  else
    OutOfRange integer

fromString : String -> Size
fromString string =
  if (String.isEmpty string) then
    Empty
  else case (String.toInt string) of
    Nothing -> NotDigits string
    Just integer -> fromInt(integer)

toString : Size -> String
toString size =
  case size of
    Empty -> ""
    NotDigits string -> string
    OutOfRange integer -> (String.fromInt integer)
    Ok integer -> (String.fromInt integer)

decoder : D.Decoder Size
decoder =
  D.oneOf
    [
      D.map
        (\maybe ->
          case maybe of
            Nothing -> Empty
            Just integer -> (fromInt integer)
        )
        (D.nullable D.int)
    , D.map
        fromString
        (D.string)
    ]

encode : Size -> E.Value
encode size =
  case size of
    Empty -> E.null
    NotDigits string -> E.string string
    OutOfRange integer -> E.int integer
    Ok integer -> E.int integer
