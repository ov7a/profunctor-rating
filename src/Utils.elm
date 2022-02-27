module Utils exposing (..)

import Dict exposing (Dict)
import List
import Result.Extra as Result


single : List a -> Maybe a
single list =
    case list of
        x :: [] ->
            Just x

        _ ->
            Nothing


associate : (item -> comparable) -> (item -> value) -> List item -> Dict comparable value
associate keyExtractor valueExtrator items =
    items
        |> List.map (\item -> Tuple.pair (keyExtractor item) (valueExtrator item))
        |> Dict.fromList


with : s -> (f -> s -> r) -> f -> r
with second func first =
    func first second

flatMapMaybe : e -> (a -> Maybe b) -> Result e a -> Result e b
flatMapMaybe error func result =
    result
        |> Result.map (func >> Result.fromMaybe error)
        |> Result.join


span : (a -> Bool) -> List a -> ( List a, List a )
span predicate list =
    let
        span_ acc curr =
            case curr of
                [] ->
                    ( List.reverse acc, curr )

                x :: xs ->
                    if predicate x then
                        span_ (x :: acc) xs

                    else
                        ( List.reverse acc, curr )
    in
    span_ [] list
