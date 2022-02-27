module Reader exposing (..)

import Result
import Result.Extra as Result
import Tuple
import Utils exposing (span)


type alias ReaderResult error result item =
    Result error ( result, List item )


type alias ReaderConsumer error input output item =
    input -> item -> List item -> ReaderResult error output item


type alias ReaderOptionalConsumer error result item =
    result -> item -> List item -> ReaderResult error result item -> ReaderResult error result item


success : result -> List item -> ReaderResult error result item
success result items =
    Ok ( result, items )


error : error -> ReaderResult error result item
error err =
    Err err


map : (a -> b) -> ReaderResult error a item -> ReaderResult error b item
map func value =
    value
        |> Result.map (Tuple.mapFirst func)


flatMap : (a -> Result error b) -> ReaderResult error a item -> ReaderResult error b item
flatMap func value =
    value
        |> Result.andThen (Result.combineMapFirst func)


consumeNext : error -> ReaderConsumer error a b item -> ReaderResult error a item -> ReaderResult error b item
consumeNext err consumer currentResult =
    let
        nextConsumer ( value, items ) =
            case items of
                [] ->
                    error err

                head :: tail ->
                    consumer value head tail
    in
    currentResult
        |> Result.andThen nextConsumer


tryConsumeNext : ReaderOptionalConsumer error result item -> ReaderResult error result item -> ReaderResult error result item
tryConsumeNext consumer currentResult =
    let
        nextConsumer : ( result, List item ) -> ReaderResult error result item
        nextConsumer ( value, items ) =
            case items of
                [] ->
                    currentResult

                head :: tail ->
                    consumer value head tail currentResult
    in
    currentResult
        |> Result.andThen nextConsumer


consumeAll : (item -> Bool) -> (result -> List item -> b) -> ReaderResult error result item -> ReaderResult error b item
consumeAll predicate combiner currentResult =
    let
        nextConsumer ( value, items ) =
            span predicate items
                |> Ok
                |> map (combiner value)
    in
    currentResult
        |> Result.andThen nextConsumer


finish : error -> ReaderResult error result item -> Result error result
finish err readerResult =
    let
        assertAllConsumed ( result, items ) =
            case items of
                [] ->
                    Ok result

                _ ->
                    Err err
    in
    readerResult
        |> Result.andThen assertAllConsumed
