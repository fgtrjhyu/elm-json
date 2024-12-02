module Baz exposing (Baz, view, decoder, encode, default, className)

import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Name
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
view self textChanged =
  Html.div [] 
    [ Html.h3 [] 
        [ Html.text self.className ]
    , Html.div []
        [ 
          Html.label []
            [ text "value" 
            , Html.input
                [
                  type_ "text"
                , name "value"
                , value self.value
                , onInput (textChanged (\text -> { self | value = text }))
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
