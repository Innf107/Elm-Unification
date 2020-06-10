module Unify exposing (..)
import Lib exposing (find)
import List exposing (any, concatMap, filter, foldr, head, length, map)
import Types exposing (..)

toString : Type -> String
toString t = case t of
    Var x -> x
    Constr x [] -> x
    Constr x ts -> "(" ++ foldr (\v a -> a ++ " " ++ toString v) x ts ++ ")"


unify : Type -> Type -> Unifs
unify t1 t2 = unify_ [(t1, t2)] []

unify_ : List (Type, Type) -> List (Type, Type) -> Unifs
unify_ a b = case (a, b) of
    ([], yE) -> [Unif {e=[], yE=yE, rule="-"}]
    (e_, yE_)  -> case (tryRules [ruleE, ruleV, ruleZ, ruleK] e_ yE_) of
                    ((Unif {rule})::xs, e, yE) -> unify_ e yE ++ Unif{e=e, yE=yE, rule=rule}::xs
                    ((OccursCheck t)::xs, _, _) -> (OccursCheck t)::xs
                    ((NoRulesMatched)::xs, _, _) -> (NoRulesMatched)::xs
                    _ -> [NoRulesMatched, NoRulesMatched, NoRulesMatched]

tryRules : List (List (Type, Type) -> List (Type, Type) -> Maybe (Unifs, List (Type, Type), List (Type, Type))) -> List (Type, Type) -> List (Type, Type) -> (Unifs, List (Type, Type), List (Type, Type))
tryRules a b c = case (a, b, c) of
    ([], e, yE) -> ([NoRulesMatched, Unif {e=e, yE=yE, rule="-"}], e, yE)
    (r::rs, e, yE) -> case r e yE of
        Just (us, e_, yE_) -> (us, e_, yE_)
        Nothing -> tryRules rs e yE

ruleE : List (Type, Type) -> List (Type, Type) -> Maybe (Unifs, List (Type, Type), List (Type, Type))
ruleE e yE = let e_ = filter differentParts e
                 differentParts (t1, t2) = t1 /= t2
             in if e_ == e then Nothing else Just ([Unif {e=e, yE=yE, rule="E"}], e_, yE)

ruleV : List (Type, Type) -> List (Type, Type) -> Maybe (Unifs, List (Type, Type), List (Type, Type))
ruleV e yE = let e_ = map swap e
                 swap x = case x of
                     (Constr a ts, Var b) -> (Var b, Constr a ts)
                     _ -> x
             in if e_ == e then Nothing else Just ([Unif {e=e, yE=yE, rule="V"}], e_, yE)

ruleZ : List (Type, Type) -> List (Type, Type) -> Maybe (Unifs, List (Type, Type), List (Type, Type))
ruleZ e yE = let e_ = concatMap deconstruct e
                 deconstruct x = case x of
                     (Constr a ts1, Constr b ts2) ->
                         if length ts1 == length ts2 && a == b then
                             List.map2 Tuple.pair ts1 ts2
                         else [(Constr a ts1, Constr b ts2)]
                     _ -> [x]
             in if e_ == e then Nothing else Just ([Unif {e=e, yE=yE, rule="Z"}], e_, yE)

ruleK : List (Type, Type) -> List (Type, Type) -> Maybe (Unifs, List (Type, Type), List (Type, Type))
ruleK e yE = let (e_, yE_) = construct e yE
                 construct : List (Type, Type) -> List (Type, Type) -> (List (Type, Type), List (Type, Type))
                 construct x b = case (x, b) of
                    ([], yE2) -> ([], yE2)
                    (((Var a, t)::es), yE2) -> if not <| contains a t then (map (replace a t) es, (Var a, t)::(map (replace a t) yE2))
                        else (e, yE)
                    (e2::es, yE2) -> let (e2_, yE2_) = construct es yE2 in (e2::e2_, yE2_)
             in case find occursCheck e of
                    Just (_, t) -> Just ([OccursCheck t], e, yE)
                    Nothing -> if e_ == e && yE_ == yE then Nothing else Just ([Unif {e=e, yE=yE, rule="Z"}], e_, yE_)

occursCheck : (Type, Type) -> Bool
occursCheck (t1, t2) = case t1 of
    Var a -> contains a t2
    _ -> False

replace : String -> Type -> (Type,  Type) -> (Type, Type)
replace a t (b, c) = (replace_ a t b, replace_ a t c)


replace_ : String -> Type -> Type -> Type
replace_ a t v = case v of
    (Var b) -> if a == b then t else Var b
    (Constr c ts) -> Constr c <| map (replace_ a t) ts

contains : String -> Type -> Bool
contains a x = case x of
    (Var b) -> a == b
    (Constr _ ts) -> any (contains a) ts

