module Main exposing (main)

import Html exposing (..)
import Html.Events as E
import Html.Attributes as A
import Browser

import Types exposing (..)
import Unify exposing (unify, toString)
import Lib exposing (..)

main = Browser.sandbox {init=init, update=update, view=view}

type alias Model = {
        type1Text : String,
        type2Text : String,
        type1 : Maybe Type,
        type2 : Maybe Type,
        unifs : Unifs
    }

init : Model
init = {
        type1Text = "",
        type2Text = "",
        type1 = Nothing,
        type2 = Nothing,
        unifs = unify (Constr "Point" [Var "a", Var "b", Var "c"])
                      (Constr "Point" [Var "c", Constr "Either" [Var "d", Var "d"], Var "c"])
    }

type Msg = UpdateType1Text String
         | UpdateType2Text String

update : Msg -> Model -> Model
update msg model = case msg of
    UpdateType1Text t -> {model|type1Text=t}
    UpdateType2Text t -> {model|type2Text=t}

view : Model -> Html Msg
view model = div [] [
        input [A.placeholder "Typ1", A.value model.type1Text, E.onInput UpdateType1Text] [],
        input [A.placeholder "Typ2", A.value model.type2Text, E.onInput UpdateType2Text] [],

        table [] (List.map (viewUnif model) <| List.reverse model.unifs)
    ]

viewUnif : Model -> Unif -> Html Msg
viewUnif model unif_ = case unif_ of
    Unif unif -> tr [] [
            td [] (ifEmpty (text "∅") <| List.map (\(x, y) -> text <| toString x ++ " = " ++ toString y ++ ", ") unif.e),
            td [] (ifEmpty (text "∅") <| List.map (\(x, y) -> text <| toString x ++ " → " ++ toString y ++ ", ") unif.yE)
        ]
    OccursCheck t -> tr [] [b [] [text <| "Occurs check on type " ++ toString t]]
    NoRulesMatched -> tr [] [b [] [text <| "No rules matched!"]]

--text <| Debug.toString (unify (Constr "Point" [Var "a", Var "b", Var "c"])
--                   (Constr "Point" [Var "c", Constr "Either" [Var "d", Var "d"], Var "c"]))