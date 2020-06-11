module Main exposing (main)

import Html exposing (..)
import Html.Events as E
import Html.Attributes as A
import Browser

import Types exposing (..)
import Unify exposing (unify, toString)
import TParser exposing (parse)
import Lib exposing (..)
import Parser

main = Browser.sandbox {init=init, update=update, view=view}

type alias Model = {
        type1Text : String,
        type2Text : String,
        type1 : Maybe Type,
        type2 : Maybe Type,
        parseErrorT1 : Maybe String,
        parseErrorT2 : Maybe String,
        unifs : Unifs
    }

init : Model
init = {
        type1Text = "",
        type2Text = "",
        type1 = Nothing,
        type2 = Nothing,
        parseErrorT1 = Nothing,
        parseErrorT2 = Nothing,
        unifs = unify (Constr "Point" [Var "a", Var "b", Var "c"])
                      (Constr "Point" [Var "c", Constr "Either" [Var "d", Var "d"], Var "c"])
    }

type Msg = UpdateType1Text String
         | UpdateType2Text String

update : Msg -> Model -> Model
update msg model = case msg of
    UpdateType1Text text -> updateUnif <| case parse text of
        Ok t   -> {model|type1Text=text, type1=Just t, parseErrorT1 = Nothing}
        Err es -> {model|type1Text=text, parseErrorT1=Just <| Parser.deadEndsToString es, type1 = Nothing}
    UpdateType2Text text -> updateUnif <| case parse text of
        Ok t   -> {model|type2Text=text, type2=Just t, parseErrorT2 = Nothing}
        Err es -> {model|type2Text=text, parseErrorT2=Just <| Parser.deadEndsToString es, type2 = Nothing}

updateUnif : Model -> Model
updateUnif model = {model|unifs=Maybe.withDefault [] <| Maybe.map2 unify model.type1 model.type2}

view : Model -> Html Msg
view model = div [A.id "main"] [
        Html.node "link" [A.rel "stylesheet", A.href "main.css"] [],
        Html.node "link" [A.rel "stylesheet", A.href "../main.css"] [],
        input [A.placeholder "Typ1", A.value model.type1Text, E.onInput UpdateType1Text] [],
        input [A.placeholder "Typ2", A.value model.type2Text, E.onInput UpdateType2Text] [],

        table [] (List.map (viewUnif model) <| List.reverse model.unifs)
    ]

viewUnif : Model -> Unif -> Html Msg
viewUnif model unif_ = case unif_ of
    Unif unif -> tr [] [
            td [] (ifEmpty (text "∅") <| List.map (\(x, y) -> text <| toString x ++ " = " ++ toString y ++ ", ") unif.e),
            td [] (ifEmpty (text "∅") <| List.map (\(x, y) -> text <| toString x ++ " → " ++ toString y ++ ", ") unif.yE),
            td [] [text <| unif.rule]
        ]
    OccursCheck t1 t2 -> tr [] [b [] [text <| "Occurs check on " ++ toString t1 ++ " = " ++ toString t2]]
    NoRulesMatched -> tr [] [b [] [text <| "No rules matched!"]]

--text <| Debug.toString (unify (Constr "Point" [Var "a", Var "b", Var "c"])
--                   (Constr "Point" [Var "c", Constr "Either" [Var "d", Var "d"], Var "c"]))