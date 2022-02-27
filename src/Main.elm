module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DataLoader exposing (makeRequest)
import Html exposing (..)
import Maybe.Extra as Maybe
import Model exposing (..)
import Random
import Result.Extra as Result
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query
import Utils exposing (with)
import View exposing (view)


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url navigation =
    { expression = extractExpression url
    , usersData = Loading
    , expressionError = Nothing
    , rating = Nothing
    , navigation = navigation
    }
        |> with (makeRequest processResponse) Tuple.pair


{-| see issue: <https://github.com/elm/url/issues/17>
-}
extractExpression : Url.Url -> String
extractExpression location =
    { location | path = "" }
        |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string expressionQueryParam))
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault defaultExpression


expressionToUrl : String -> String
expressionToUrl expression =
    Url.Builder.relative [] [ Url.Builder.string expressionQueryParam expression ]


processResponse : Result String (List UserData) -> Message
processResponse response =
    response
        |> Result.map DataLoaded
        |> Result.mapError DataLoadingError
        |> Result.merge


mockDataGenerator : Random.Generator (List UserData)
mockDataGenerator =
    let
        randSmall =
            Random.int 1 100

        randLarge =
            Random.int 0 4000

        dataGenerator =
            Random.map3 generateUser randSmall randLarge randLarge

        generateUser i d u p =
            UserData ("user" ++ String.fromInt i) p u d

        apply index indexFun =
            indexFun index
    in
    Random.list 100 dataGenerator
        |> Random.map (List.indexedMap apply)


generatedMockData : List UserData
generatedMockData =
    Random.initialSeed 4276215469
        |> Random.step mockDataGenerator
        |> Tuple.first


updateRating : Model -> Model
updateRating model =
    let
        ratingFunc =
            model.expression
                |> parseExpressionFunction

        recalc data =
            ratingFunc
                |> Result.toMaybe
                |> Maybe.map (with data calcRating)
                |> Maybe.orElse model.rating
    in
    case model.usersData of
        Loaded usersData ->
            { model | rating = recalc usersData, expressionError = Result.error ratingFunc }

        Loading ->
            { model | rating = Nothing, expressionError = Nothing }

        LoadingError error mockData ->
            { model | rating = recalc mockData, expressionError = Result.error ratingFunc }


noCmd : a -> ( a, Cmd msg )
noCmd model =
    ( model, Cmd.none )


update : Message -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        DataLoaded usersData ->
            { model | usersData = Loaded usersData }
                |> updateRating
                |> noCmd

        DataLoadingError error ->
            { model | usersData = LoadingError (error ++ "... so mock data is loaded instead") generatedMockData }
                |> updateRating
                |> noCmd

        ExpressionUpdated input ->
            { model | expression = input }
                |> noCmd

        Calculate ->
            ( model, Nav.pushUrl model.navigation (model.expression |> String.trim |> expressionToUrl) )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navigation (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            { model | expression = extractExpression url }
                |> updateRating
                |> noCmd


main : Program () Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
