module Foo exposing (Foo, view, decoder, encode, default, className)

import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Name
className : String
className = "Foo"

-- Foo
type alias Foo =
  { className : String
  , foo : String
  }

-- DEFAULT
default : Foo
default =
  (Foo className "")

-- HTML
view : Foo -> ((String -> Foo) -> (String -> msg)) -> Html msg
view self textChanged =
  div [] 
    [ h3 [] 
        [ text self.className ]
    , div []
        [ 
          label []
            [ text "foo" 
            , input
                [
                  type_ "text"
                , name "foo"
                , value self.foo
                , onInput (textChanged (\text -> { self | foo = text }))
                ]
                [
                ]
            ]
        ]
    ]

-- DECODER
decoder : D.Decoder Foo
decoder =
  D.map2
    Foo
    (D.field "className" D.string)
    (D.field "foo" D.string)

-- ENCODE
encode : Foo -> E.Value
encode self =
  E.object
    [ ("className", (E.string self.className))
    , ("foo", (E.string self.foo))
    ]
