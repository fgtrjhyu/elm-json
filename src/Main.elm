port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- PORT
port store : E.Value -> Cmd msg
port remove : () -> Cmd msg

-- MODEL

type Value
  = Empty String
  | NotInteger String
  | Negative Int
  | Limit Int
  | DecodeErr
  | Succeed Int

type alias Model =
  { value : Value
  }


init : E.Value -> ( Model, Cmd Msg )
init flags =
  (
    case (D.decodeValue modelDecoder flags) of
      Ok model -> model
      Err _ -> { value = DecodeErr } 
  , Cmd.none
  )

-- UPDATE

type Msg
  = ValueChanged String
  | Remove

valueFromInt : Int -> Value
valueFromInt integer =
  if integer < 0 then
    Negative integer
  else if integer > 100 then
    Limit integer
  else
    Succeed integer

valueFromString : String -> Value
valueFromString source =
  if (String.length source) == 0 then
    Empty source
  else
    case (String.toInt source) of
      Nothing ->
        NotInteger source
      Just integer ->
        (valueFromInt integer)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ValueChanged newValue ->
      let
        newModel = { model | value = (valueFromString newValue) }
      in
        ( newModel
        , (store (encode newModel))
        )
    Remove ->
      ( model
      , (remove ())
      )


type alias InputView =
  {
    viewValue : String
  , succeed : Bool
  , reason : Maybe String
  }

toInputView : Value -> InputView
toInputView value =
  case value of
    Empty string -> (InputView string False (Just "empty"))
    NotInteger string -> (InputView string False (Just "not integer"))
    Negative integer -> (InputView (String.fromInt integer) False (Just "negative"))
    Limit integer -> (InputView (String.fromInt integer) False (Just "limit"))
    Succeed integer -> (InputView (String.fromInt integer) True (Just "Ok"))
    DecodeErr -> (InputView "" False (Just "decode error"))

reasonToHtml : InputView -> Html Msg
reasonToHtml inputView =
  case inputView.reason of
    Nothing ->
      Html.text ""
    Just string ->
      Html.div
        [ style "color" (if inputView.succeed then "green" else "red")
        ]
        [ Html.text string
        ]

view : Model -> Html Msg
view model =
  let
    inputView = (toInputView model.value)
  in
    div
      [ style "padding" "1em"
      , style "width" "256px"
      ]
      [ div
          []
          [ reasonToHtml inputView
          ]
      , div
          []
          [ input
              [ type_ "text"
              , onInput ValueChanged
              , value inputView.viewValue
              , style "font-size" "large"
              , style "padding-top" "0.5em"
              , style "padding-bottom" "0.5em"
              , style "border-color" (if inputView.succeed then "green" else "red")
              , style "border-width" "3px"
              ]
              []
          ]
      , div []
          [ button
              [ type_ "button"
              , onClick Remove
              ]
              [ text "remove" ]
          ]
      ]

encodeValue : Value -> E.Value
encodeValue value =
  case value of
    Empty string -> E.string string
    NotInteger string -> E.string string
    Negative integer -> E.int integer
    Limit integer -> E.int integer
    Succeed integer -> E.int integer
    DecodeErr -> E.null

encode : Model -> E.Value
encode model =
  E.object
    [ ("value", (encodeValue model.value))
    ]

valueDecoder : D.Decoder Value
valueDecoder =
  (D.oneOf
    [ (D.map
        (\maybe ->
          case maybe of
            Nothing -> Empty ""
            Just integer ->
              (valueFromInt integer)
        )
        (D.nullable D.int)
      )
    , (D.map
        valueFromString
        D.string
      )
    ]
  )

modelDecoder : D.Decoder Model
modelDecoder =
  D.map
    Model
    (D.field "value" valueDecoder)

