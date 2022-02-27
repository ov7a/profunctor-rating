module Expression.Main exposing (evaluate, parse)

import Expression.InternalTypes exposing (TokenizerOptions)
import Expression.Parser as Parser
import Expression.Tokenizer as Tokenizer
import Expression.Types exposing (..)
import Result
import Result.Extra as Result
import Set
import Utils exposing (associate, single)


parse : ExpressionOptions context -> String -> Result String (Expression context)
parse expressionOptions input =
    let
        tokenizerOptions : TokenizerOptions
        tokenizerOptions =
            { functions = expressionOptions.functions |> List.map .name |> Set.fromList
            , operators = expressionOptions.operators |> List.map .symbol |> Set.fromList
            }
    in
    Tokenizer.tokenize tokenizerOptions input
        |> Result.andThen (Parser.parseTokens expressionOptions)


evaluate : Expression context -> context -> Result String Float
evaluate expression context =
    let
        evaluateWithContext : Expression context -> Result String Float
        evaluateWithContext expr =
            evaluate expr context

        result =
            case expression of
                NumberExpression number ->
                    Ok number

                OperatorExpression operator evaluation expressions ->
                    Result.combineMap evaluateWithContext expressions |> Result.andThen (evaluation context)

                FunctionExpression name evaluation expressions ->
                    Result.combineMap evaluateWithContext expressions |> Result.andThen (evaluation context)
    in
    result |> Result.filter "got NaN" (not << isNaN)
