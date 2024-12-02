port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Baz exposing (Baz)
import Foo exposing (Foo)
import Bar exposing (Bar)

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

type CType
  = CTFoo Foo
  | CTBar Bar
  | CTBaz Baz

type alias Model =
  { value : Value
  , children : List CType
  }

init : E.Value -> ( Model, Cmd Msg )
init flags =
  (
    case (D.decodeValue modelDecoder flags) of
      Ok model -> model
      Err _ -> { value = DecodeErr, children = [] } 
  , Cmd.none
  )

-- UPDATE

type Msg
  = ValueChanged String
  | Remove
  | AddCType String
  | RemoveChildAt Int
  | ReplaceChildAt Int CType

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
    AddCType className ->
      let
          maybe =
            case className of
              "Foo" -> (Just (CTFoo (Foo.default)))
              "Bar" -> (Just (CTBar (Bar.default)))
              "Baz" -> (Just (CTBaz (Baz.default)))
              _ -> Nothing

          newChildren =
            case maybe of
              Just child ->
                (List.append model.children [ child ])
              Nothing ->
                model.children

          newModel = { model | children = newChildren }
      in
      (
        newModel
      , (store (encode newModel))
      )
    RemoveChildAt index ->
      let
        children = model.children
        newChildren =
          (List.take index children)
          ++ (List.drop (index + 1) children)
        newModel = { model | children = newChildren }
      in
      (
        newModel
      , (store (encode newModel))
      )
    ReplaceChildAt position newCType ->
      let
        children = model.children
        newChildren = (List.indexedMap (replaceAt position newCType) children)
        newModel = { model | children = newChildren }
      in
      (
        newModel
      , (store (encode newModel))
      )

replaceAt :  Int -> CType -> Int -> CType -> CType
replaceAt position newElem index curElem =
  if position == index then
    newElem
  else
    curElem

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
      , div []
          [ button
              [ type_ "button"
              , onClick (AddCType "Foo")
              ]
              [ text "add Foo" ]
          , button
              [ type_ "button"
              , onClick (AddCType "Bar")
              ]
              [ text "add Bar" ]
          , button
              [ type_ "button"
              , onClick (AddCType "Baz")
              ]
              [ text "add Baz" ]
          ]
      , div []
          (model.children |> List.indexedMap displayCType)
      ]

displayCType : Int -> CType -> Html Msg
displayCType index child =
  let
    textChangedAt = (\ch cons text -> (ReplaceChildAt index (ch (cons text))))
    docHtml =
      case child of
        CTFoo content -> (Foo.view content (textChangedAt CTFoo))
        CTBar content -> (Bar.view content (textChangedAt CTBar))
        CTBaz content -> (Baz.view content (textChangedAt CTBaz))
  in
    div
      [ style "border-top" "1px solid black"
      , style "margin-top" "1em"
      ]
      [ div
          [ style "padding-top" "1em"
          ]
          [
            label
              [
                style "padding-right" "1em"
              ]
              [ text (String.fromInt index) ]
          , button
              [ type_ "button"
              , onClick (RemoveChildAt index)
              ]
              [ text "remove" ]
          ]
      , div
          []
          [ (docHtml) ]
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

encodeCType : CType -> E.Value
encodeCType child =
  case child of
    CTFoo content -> (Foo.encode content)
    CTBar content -> (Bar.encode content)
    CTBaz content -> (Baz.encode content)

encodeChildren : List CType -> E.Value
encodeChildren =
  E.list encodeCType

encode : Model -> E.Value
encode model =
  E.object
    [ ("value", (encodeValue model.value))
    , ("children", (encodeChildren model.children))
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

checkClassName : String -> D.Decoder CType -> D.Decoder CType
checkClassName className decoder =
    D.andThen
      (\child ->
        case child of
          CTFoo content ->
            if content.className == className then
              (D.succeed child)
            else
              (D.fail className)
          CTBar content ->
            if content.className == className then
              (D.succeed child)
            else
              (D.fail className)
          CTBaz content ->
            if content.className == className then
              (D.succeed child)
            else
              (D.fail className)
      )
      decoder

childDecoder : D.Decoder CType
childDecoder =
  D.oneOf
    [ (checkClassName Foo.className (D.map CTFoo Foo.decoder))
    , (checkClassName Bar.className (D.map CTBar Bar.decoder))
    , (checkClassName Baz.className (D.map CTBaz Baz.decoder))
    ]

modelDecoder : D.Decoder Model
modelDecoder =
  D.map2
    Model
    (D.field "value" valueDecoder)
    (D.field "children" (D.list childDecoder))

