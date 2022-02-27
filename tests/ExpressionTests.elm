module ExpressionTests exposing (..)

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Expression.Main exposing (..)
import Expression.Types exposing (..)
import Test exposing (..)
import Utils exposing (single, with)


type alias SampleContext =
    { a : Int, b : Float }


someContext : SampleContext
someContext =
    { a = 10, b = 0.5 }


expressionOptions : ExpressionOptions SampleContext
expressionOptions =
    { functions =
        [ variable "a" (.a >> toFloat)
        , variable "b" .b
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


parseAndEvaluate : String -> SampleContext -> Result String Float
parseAndEvaluate expr context =
    parse expressionOptions expr
        |> Result.andThen (with context evaluate)


assertSuccess : String -> Float -> () -> Expectation
assertSuccess expr number =
    \() ->
        case parseAndEvaluate expr someContext of
            Ok result ->
                Expect.within (Absolute 0.000000001) number result

            Err message ->
                Expect.fail message


assertFailure : String -> () -> Expectation
assertFailure expr =
    \() ->
        parseAndEvaluate expr someContext
            |> Expect.err


basicTests : List Test
basicTests =
    [ test "just number" <|
        assertSuccess "11.55345" 11.55345
    , test "plus" <|
        assertSuccess "2 +3" 5
    , test "unary plus" <|
        assertSuccess "+3.4" 3.4
    , test "minus" <|
        assertSuccess "4-5.3" -1.3
    , test "unary minus" <|
        assertSuccess "4 + -5.3" -1.3
    , test "unary minus in brackets" <|
        assertSuccess "4 + (-(5.3))" -1.3
    , test "multiplication" <|
        assertSuccess "-2.5*3.3" -8.25
    , test "division" <|
        assertSuccess "-2.5/3.3" -0.7575757575757576
    , test "power" <|
        assertSuccess "-2.5 ^ 3.5" -24.705294220065465
    , test "sqrt" <|
        assertSuccess "sqrt(2.5)" 1.5811388300841898
    , test "sqrt negative" <|
        assertFailure "sqrt(-2.5)"
    , test "max 1 arg" <|
        assertFailure "max(1.2)"
    , test "max 2 args" <|
        assertSuccess "max(1.2, 2)" 2
    , test "max several args" <|
        assertSuccess "max(-1, 1.2, 5, 3)" 5
    , test "min 1 arg" <|
        assertFailure "min(1.2)"
    , test "min 2 args" <|
        assertSuccess "min(1.2, 2)" 1.2
    , test "min several args" <|
        assertSuccess "min(1.2, -1, 5, 3)" -1
    , test "abs negative" <|
        assertSuccess "abs(-1.2)" 1.2
    , test "abs zero" <|
        assertSuccess "abs(0)" 0
    , test "abs positive" <|
        assertSuccess "abs(4.2)" 4.2
    , test "ln" <|
        assertSuccess "ln(1000)/ln(10)" 3
    , test "ln e" <|
        assertSuccess "ln(e)" 1
    , test "floor low" <|
        assertSuccess "floor(12.3)" 12
    , test "floor high" <|
        assertSuccess "floor(12.7)" 12
    , test "round low" <|
        assertSuccess "round(12.3)" 12
    , test "round high" <|
        assertSuccess "round(12.7)" 13
    , test "ceil low" <|
        assertSuccess "ceil(12.3)" 13
    , test "ceil high" <|
        assertSuccess "ceil(12.7)" 13
    , test "abs two args" <|
        assertFailure "abs(1, 2)"
    , test "empty" <|
        assertFailure ""
    , test "garbage" <|
        assertFailure "!@#$%"
    , test "unknown function" <|
        assertFailure "ololos(1,2,3)"
    ]


smokeTests : List Test
smokeTests =
    List.indexedMap (\i v -> test ("smoke" ++ String.fromInt i) <| v)
        [ assertSuccess "1 + 2 * 5 /40-3/6^4^7+-5" -3.75
        , assertSuccess "3^((-(1+2.2 )*6)^4)" (1 / 0)
        , assertSuccess "3^((-(1+2.2 )*6)^5) + 5.5" 5.5
        , assertSuccess "-6^4" -1296
        , assertSuccess "(-6)^4" 1296
        , assertSuccess "max (4, 5, abs(a - 6 ) )" 5
        , assertFailure "("
        , assertFailure "1 + 2)"
        , assertSuccess "a * b" 5
        ]


suite : Test
suite =
    Test.concat <| basicTests ++ smokeTests
