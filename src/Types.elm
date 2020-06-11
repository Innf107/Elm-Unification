module Types exposing (..)

type Type = Constr String (List Type)
          | Var String


type Unif = Unif {
                e:List (Type, Type),
                yE: List (Type, Type),
                rule: String
            }
    | OccursCheck Type Type
    | NoRulesMatched

type alias Unifs = List Unif

