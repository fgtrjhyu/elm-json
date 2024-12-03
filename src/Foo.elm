module Foo exposing (Foo, view, decoder, encode, default, className)

import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Foo.Size as S

-- CLASSNAME
className : String
className = "Foo"

-- DATA MODEL
type alias Foo =
  { className : String
  , size : S.Size
  }

-- DEFAULT
default : Foo
default =
  (Foo className S.empty)

-- HTML
view : Foo -> ((String -> Foo) -> (String -> msg)) -> Html msg
view self valueChanged =
  div [] 
    [ h3 [] 
        [ text self.className ]
    , div []
        [ label
            [ style "color" (colorOfSize self.size)
            ]
            [ text "foo" 
            , input
                [
                  type_ "text"
                , name "size"
                , value (S.toString self.size)
                , onInput (valueChanged (\text -> { self | size = (S.fromString text) }))
                , style "border-color" (borderColorOfSize self.size)
                ]
                [
                ]
            ]
        , (messageOfSize self.size)
        ]
    ]

colorOfSize : S.Size -> String
colorOfSize size =
  (S.match
    size
    ("red")
    (\_ -> "red")
    (\_ -> "red")
    (\_ -> "green")
  )

borderColorOfSize : S.Size -> String
borderColorOfSize size =
  (S.match
    size
    ("red")
    (\_ -> "red")
    (\_ -> "red")
    (\_ -> "green")
  )

messageLabelOfSize : String -> String -> String -> Html msg
messageLabelOfSize message color backgroundColor =
  (label
    [ style "color" color
    , style "background-color" backgroundColor
    , style "padding" "2px 6px 5px 6px"
    ]
    [ text message ]
  )

messageOfSize : S.Size -> Html msg
messageOfSize size =
  (S.match
    size
    (messageLabelOfSize "empty" "white" "red")
    (\string ->
      (messageLabelOfSize (string ++ " is not digits") "white" "red")
    )
    (\integer ->
      (messageLabelOfSize ((String.fromInt integer) ++ " is out of range") "white" "red")
    )
    (\integer ->
      (messageLabelOfSize ((String.fromInt integer) ++ " is Ok") "white" "green")
    )
  )

-- DECODER
decoder : D.Decoder Foo
decoder =
  D.map2
    Foo
    (D.field "className" D.string)
    (D.field "size" S.decoder)

-- ENCODE
encode : Foo -> E.Value
encode self =
  E.object
    [ ("className", (E.string self.className))
    , ("size", (S.encode self.size))
    ]

