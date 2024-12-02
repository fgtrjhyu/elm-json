port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Array exposing (fromList, toList)

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

type alias Foo =
  { name : String
  , foo : String
  }

type alias Bar =
  { name : String
  , cmd : String
  , files : String
  }

type Child
  = Child1 Foo
  | Child2 Bar

type alias Model =
  { value : Value
  , children : List Child
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
  | AddChild String
  | RemoveChildAt Int
  | ReplaceChildAt Int Child

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
    AddChild name ->
      let
          maybe = case name of
            "Foo" -> (Just (Child1 (Foo name "")))
            "Bar" -> (Just (Child2 (Bar name "" "")))
            _ -> Nothing
          newModel = case maybe of
            Just child ->
              { model | children = (List.append model.children [ child ]) }
            Nothing ->
              model
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
    ReplaceChildAt position newChild ->
      let
        children = model.children
        newChildren = (List.indexedMap (replaceAt position newChild) children)
        newModel = { model | children = newChildren }
      in
      (
        newModel
      , (store (encode newModel))
      )

replaceAt :  Int -> Child -> Int -> Child -> Child
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
              , on "click" (addChild "Foo")
              ]
              [ text "add Foo" ]
          , button
              [ type_ "button"
              , on "click" (addChild "Bar")
              ]
              [ text "add Bar" ]
          ]
      , div []
          (model.children |> List.indexedMap displayChild)
      ]

displayFoo : Int -> Foo -> ((String -> Foo) -> (String -> Msg)) -> Html Msg
displayFoo index doc textChanged =
  Html.div [] 
    [ Html.h3 [] 
        [ Html.text doc.name ]
    , Html.div []
        [ 
          Html.label []
            [ text "foo" 
            , Html.input
                [
                  type_ "text"
                , name "foo"
                , value doc.foo
                , onInput (textChanged (\text -> { doc | foo = text}))
                ]
                [
                ]
            ]
        ]
    ]

displayBar : Int -> Bar -> ((String -> Bar) -> (String -> Msg)) -> Html Msg
displayBar index doc textChanged =
  Html.div [] 
    [ Html.h3 [] 
        [ Html.text doc.name ]
    , Html.div []
        [ Html.label []
            [ text "cmd"
            , Html.input
                [
                  type_ "text"
                , name "cmd"
                , value doc.cmd
                , onInput (textChanged (\text -> { doc | cmd = text}))
                ]
                [
                ]
            ]
        ]
    , Html.div []
        [ Html.label []
            [ text "files"
            , Html.input
                [
                  type_ "text"
                , name "files"
                , value doc.files
                , onInput (textChanged (\text -> { doc | files = text}))
                ]
                [
                ]
            ]
        ]
    ]

displayChild : Int -> Child -> Html Msg
displayChild index child =
  let
    textChangedAt = (\ch -> (\cons -> (\text -> (ReplaceChildAt index (ch (cons text))))))
    docHtml =
      case child of
        Child1 doc -> (displayFoo index doc (textChangedAt Child1))
        Child2 doc -> (displayBar index doc (textChangedAt Child2))
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
              , on "click" (removeChild index)
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

encodeFoo : Foo -> E.Value
encodeFoo doc =
  E.object
    [ ("name", (E.string doc.name))
    , ("foo", (E.string doc.foo))
    ]
    
encodeBar : Bar -> E.Value
encodeBar doc =
  E.object
    [ ("name", (E.string doc.name))
    , ("cmd", (E.string doc.cmd))
    , ("files", (E.string doc.files))
    ]

encodeChild : Child -> E.Value
encodeChild child =
  case child of
    Child1 doc -> (encodeFoo doc)
    Child2 doc -> (encodeBar doc)

encodeChildren : List Child -> E.Value
encodeChildren =
  E.list encodeChild

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

addChild : String -> D.Decoder Msg
addChild name =
  D.succeed (AddChild name)

removeChild : Int -> D.Decoder Msg
removeChild index =
  D.succeed (RemoveChildAt index)

fooDecoder : D.Decoder Foo
fooDecoder =
  D.map2
    Foo
    (D.field "name" D.string)
    (D.field "foo" D.string)

barDecoder : D.Decoder Bar
barDecoder =
  D.map3
    Bar
    (D.field "name" D.string)
    (D.field "cmd" D.string)
    (D.field "files" D.string)

checkName : String -> D.Decoder Child -> D.Decoder Child
checkName name decoder =
    D.andThen
      (\doc ->
        case doc of
          Child1 child ->
            if child.name == name then
              (D.succeed doc)
            else
              (D.fail name)
          Child2 child ->
            if child.name == name then
              (D.succeed doc)
            else
              (D.fail name)
      )
      decoder

childDecoder : D.Decoder Child
childDecoder =
  D.oneOf
    [ (checkName "Foo" (D.map Child1 fooDecoder))
    , (checkName "Bar" (D.map Child2 barDecoder))
    ]

modelDecoder : D.Decoder Model
modelDecoder =
  D.map2
    Model
    (D.field "value" valueDecoder)
    (D.field "children" (D.list childDecoder))

