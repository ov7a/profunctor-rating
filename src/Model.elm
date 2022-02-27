module Model exposing (Message(..), Model, UserData, UserWithRating, UsersDataState(..), calcRating, defaultExpression, expressionQueryParam, parseExpressionFunction)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Expression.Main exposing (..)
import Expression.Types exposing (..)
import Result
import Result.Extra as Result
import Url exposing (Url)
import Utils exposing (with)


type alias UserData =
    { username : String, posts : Int, upvotes : Int, downvotes : Int }


type alias UserWithRating =
    { userData : UserData, rating : Float }


type alias Model =
    { expression : String
    , usersData : UsersDataState
    , expressionError : Maybe String
    , rating : Maybe (List UserWithRating)
    , navigation : Nav.Key
    }


type UsersDataState
    = Loading
    | LoadingError String (List UserData)
    | Loaded (List UserData)


type Message
    = ExpressionUpdated String
    | DataLoaded (List UserData)
    | DataLoadingError String
    | Calculate
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


expressionQueryParam : String
expressionQueryParam =
    "expression"


defaultExpression : String
defaultExpression =
    "floor(p * u / sqrt(d))"


expressionOptions : ExpressionOptions UserData
expressionOptions =
    { functions =
        [ variable "p" (.posts >> toFloat)
        , variable "u" (.upvotes >> toFloat)
        , variable "d" (.downvotes >> toFloat)
        , function "max" (AtLeast 2) List.maximum
        , function "min" (AtLeast 2) List.minimum
        , unaryFunction "abs" (abs >> Just)
        , unaryFunction "sqrt" (sqrt >> Just)
        , unaryFunction "ln" (logBase e >> Just)
        , variable "e" (\_ -> e)
        , unaryFunction "floor" (floor >> toFloat >> Just)
        , unaryFunction "round" (round >> toFloat >> Just)
        , unaryFunction "ceil" (ceiling >> toFloat >> Just)
        ]
    , operators =
        [ { symbol = '+', leftBindingPower = 10, rightBindingPower = 11, prefixBindingPower = 25, evaluation = singleOrBinaryOperator "plus" identity (+) }
        , { symbol = '-', leftBindingPower = 10, rightBindingPower = 11, prefixBindingPower = 26, evaluation = singleOrBinaryOperator "minus" (\x -> -x) (-) }
        , { symbol = '*', leftBindingPower = 20, rightBindingPower = 21, prefixBindingPower = -1, evaluation = binaryOperator "multiplication" (*) }
        , { symbol = '/', leftBindingPower = 20, rightBindingPower = 21, prefixBindingPower = -1, evaluation = binaryOperator "division" (/) }
        , { symbol = '^', leftBindingPower = 31, rightBindingPower = 31, prefixBindingPower = -1, evaluation = binaryOperator "power" (^) }
        ]
    }


parseExpressionFunction : String -> Result String (UserData -> Result String Float)
parseExpressionFunction input =
    parse expressionOptions input
        |> Result.map evaluate


getRatedData : (UserData -> Result String Float) -> UserData -> UserWithRating
getRatedData ratingFunc userData =
    userData
        |> ratingFunc
        |> Result.withDefault nan
        |> UserWithRating userData


calcRating : (UserData -> Result String Float) -> List UserData -> List UserWithRating
calcRating ratingFunc usersData =
    usersData
        |> List.map (getRatedData ratingFunc)
        |> List.sortWith descendingRating


nan : Float
nan =
    0 / 0


naNasInf : Float -> Float
naNasInf x =
    if isNaN x then
        -1 / 0

    else
        x


descendingRating : UserWithRating -> UserWithRating -> Order
descendingRating a b =
    case compare (naNasInf a.rating) (naNasInf b.rating) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
