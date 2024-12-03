module Baz exposing (Baz, view, decoder, encode, default, className)

import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- CLASSNAME
className : String
className = "Baz"

-- Baz
type alias Baz =
  { className : String
  , value : String
  }

-- DEFAULT
default : Baz
default =
  (Baz className "")

-- HTML
view : Baz -> ((String -> Baz) -> (String -> msg)) -> Html msg
view self valueChanged =
  div [] 
    [ h3 [] 
        [ text self.className ]
    , div []
        [ 
          label []
            [ text "value" 
            , input
                [
                  type_ "text"
                , name "value"
                , value self.value
                , onInput (valueChanged (\text -> { self | value = text }))
                ]
                [
                ]
            ]
        ]
    ]

-- DECODER
decoder : D.Decoder Baz
decoder =
  D.map2
    Baz
    (D.field "className" D.string)
    (D.field "value" D.string)

-- ENCODE
encode : Baz -> E.Value
encode self =
  E.object
    [ ("className", (E.string self.className))
    , ("value", (E.string self.value))
    ]
