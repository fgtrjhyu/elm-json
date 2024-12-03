module Bar exposing (Bar, view, decoder, encode, default, className)

import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- CLASSNAME
className : String
className = "Bar"

-- Bar
type alias Bar =
  { className : String
  , cmd : String
  , files : String
  }

-- DEFAULT
default : Bar
default =
  (Bar className "" "")

-- HTML
view : Bar -> ((String -> Bar) -> (String -> msg)) -> Html msg
view self valueChanged =
  div [] 
    [ h3 [] 
        [ text self.className ]
    , div []
        [ 
          label []
            [ text "cmd" 
            , input
                [
                  type_ "text"
                , name "cmd"
                , value self.cmd
                , onInput (valueChanged (\text -> { self | cmd = text }))
                ]
                [
                ]
            ]
        ]
    , div []
        [ 
          label []
            [ text "files" 
            , input
                [
                  type_ "text"
                , name "files"
                , value self.files
                , onInput (valueChanged (\text -> { self | files = text }))
                ]
                [
                ]
            ]
        ]
    ]

-- DECODER
decoder : D.Decoder Bar
decoder =
  D.map3
    Bar
    (D.field "className" D.string)
    (D.field "cmd" D.string)
    (D.field "files" D.string)

-- ENCODE
encode : Bar -> E.Value
encode self =
  E.object
    [ ("className", (E.string self.className))
    , ("cmd", (E.string self.cmd))
    , ("files", (E.string self.files))
    ]
