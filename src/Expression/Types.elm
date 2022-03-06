module Expression.Types exposing (..)

import Maybe.Extra as Maybe
import Result.Extra as Result
import Utils exposing (..)


type alias ExpressionOptions context =
    { functions : List (Function context)
    , operators : List (Operator context)
    }


type alias Function context =
    { name : String
    , arity : Arity
    , evaluation : Evaluation context
    }


type alias Operator context =
    { symbol : Char
    , leftBindingPower : Int
    , rightBindingPower : Int

    -- negative to disable
    , prefixBindingPower : Int
    , evaluation : Evaluation context
    }


type Arity
    = Exactly Int
    | AtLeast Int


type alias Evaluation context =
    context -> List Float -> Result String Float


type Expression context
    = NumberExpression Float
    | FunctionExpression String (Evaluation context) (List (Expression context))
    | OperatorExpression Char (Evaluation context) (List (Expression context))


checkArity : String -> Arity -> List a -> Result String (List a)
checkArity name expectedArity args =
    let
        argumentsCount =
            List.length args

        errorMessage conditionStr count =
            "Function " ++ name ++ " should have " ++ conditionStr ++ " " ++ String.fromInt count ++ " arguments, but got " ++ String.fromInt argumentsCount
    in
    case expectedArity of
        Exactly count ->
            if count == argumentsCount then
                Ok args

            else
                Err <| errorMessage "exactly" count

        AtLeast threshold ->
            if argumentsCount >= threshold then
                Ok args

            else
                Err <| errorMessage "at least" threshold


noContext : (List Float -> Result String Float) -> Evaluation context
noContext func context list =
    func list


variable : String -> (context -> Float) -> Function context
variable name getter =
    { name = name, arity = Exactly 0, evaluation = \context -> \_ -> getter context |> Ok }


function : String -> Arity -> (List Float -> Maybe Float) -> Function context
function name arity func =
    let
        errorMsg : List Float -> String
        errorMsg args =
            "Function " ++ name ++ " hasn't returned a result for [" ++ (args |> List.map String.fromFloat |> String.join ", ") ++ "]"

        evaluate : List Float -> Result String Float
        evaluate args =
            checkArity name arity args
                |> flatMapMaybe (errorMsg args) func
    in
    { name = name, arity = arity, evaluation = noContext <| evaluate }


unaryFunction : String -> (Float -> Maybe Float) -> Function contextmenu
unaryFunction name func =
    function name (Exactly 1) (List.head >> Maybe.andThen func)


binaryOperator : String -> (Float -> Float -> Float) -> Evaluation context
binaryOperator name op context args =
    evaluateBinaryOperator op args
        |> Result.fromMaybe (name ++ " should have exactly two arguments")


unaryOperator : String -> (Float -> Float) -> Evaluation context
unaryOperator name op context args =
    evaluateUnaryOperator op args
        |> Result.fromMaybe (name ++ " should have exactly one argument")


singleOrBinaryOperator : String -> (Float -> Float) -> (Float -> Float -> Float) -> Evaluation context
singleOrBinaryOperator name unaryOp binaryOp context args =
    evaluateUnaryOperator unaryOp args
        |> Maybe.orElse (evaluateBinaryOperator binaryOp args)
        |> Result.fromMaybe (name ++ " should have one or two arguments")


evaluateBinaryOperator : (Float -> Float -> Float) -> List Float -> Maybe Float
evaluateBinaryOperator op args =
    case args of
        x :: y :: [] ->
            Just (op x y)

        _ ->
            Nothing


evaluateUnaryOperator : (Float -> Float) -> List Float -> Maybe Float
evaluateUnaryOperator op args =
    single args
        |> Maybe.map op
