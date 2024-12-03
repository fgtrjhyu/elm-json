module Baz exposing (Baz, view, decoder, encode, default, className)

import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- CLASSNAME
className : String
className = "Baz"

-- DATA MODEL
type alias Baz =
  { className : String
  , text : String
  , bool : Bool
  }

-- DEFAULT
default : Baz
default =
  (Baz className "first" False)

options =
  [ ("1st.", "first")
  , ("2nd.", "second")
  ]

-- HTML

viewOption : String -> (String, String) -> Html msg
viewOption selectedValue (optionLabel, optionValue) =
  option
    [ value (optionValue)
    , selected (selectedValue == optionValue)
    ]
    [ text (optionLabel)
    ]

viewOptions : String -> List (String, String) -> List (Html msg)
viewOptions selected list =
  (List.map (viewOption selected) list)

view : Baz
  -> ((String -> Baz) -> (String -> msg))
  -> ((Bool -> Baz) -> (Bool -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> ((a -> Baz) -> (a -> msg))
  -> Html msg
view self a b _ _ _ _ _ _ _ _ =
  let
    getTargetValue = (D.at ["target", "value"] D.string)
    getTargetChecked = (D.at ["target", "checked"] D.bool)

    setText = (\value -> { self | text = value })
    setBool = (\value -> { self | bool = value })

    updateText = (D.map (a setText) getTargetValue)
    updateBool = (D.map (b setBool) getTargetChecked)
  in
    div
      [] 
      [ h3
          [] 
          [ text self.className ]
      , div
          []
          [ label
              []
              [ select
                  [ name "text"
                  , value self.text
                  , on "change" updateText
                  ]
                  (viewOptions self.text options)
              , text (self.text)
              ]
          ]
      , div
          []
          [ label
              []
              [ input
                  [ type_ "checkbox"
                  , name "bool"
                  , checked self.bool
                  , on "change" updateBool
                  ]
                  []
              , text (if self.bool then "True" else "False")
              ]
          ]
      ]

-- DECODER
decoder : D.Decoder Baz
decoder =
  D.map3
    Baz
    (D.field "className" D.string)
    (D.field "text" D.string)
    (D.field "bool" D.bool)

-- ENCODE
encode : Baz -> E.Value
encode self =
  E.object
    [ ("className", (E.string self.className))
    , ("text", (E.string self.text))
    , ("bool", (E.bool self.bool))
    ]
