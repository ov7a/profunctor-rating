module Expression.Parser exposing (..)

import Dict exposing (Dict)
import Expression.Debug exposing (..)
import Expression.InternalTypes exposing (..)
import Expression.Types exposing (..)
import Reader exposing (..)
import Utils exposing (associate, single, with)


parseTokens : ExpressionOptions context -> List Token -> Result String (Expression context)
parseTokens expressionOptions inputTokens =
    let
        functions : Dict String (Function context)
        functions =
            expressionOptions.functions |> associate .name identity

        operators : Dict Char (Operator context)
        operators =
            expressionOptions.operators |> associate .symbol identity

        parse_ : List Token -> Int -> ParserResult (Expression context)
        parse_ tokens minRightBindingPower =
            Reader.success Nothing tokens
                |> Reader.consumeNext "No tokens to consume, expected atom" consumeExpression
                |> Reader.tryConsumeNext (consumeRightExpression minRightBindingPower)

        consumeExpression : ParserConsumer nothing (Expression context)
        consumeExpression _ head tail =
            case head of
                NumberToken number ->
                    Reader.success (NumberExpression number) tail

                OpeningBracketToken ->
                    parseBracketsExpressions tail
                        |> Reader.flatMap (single >> Result.fromMaybe "Expected single expression in brackets")

                OperatorToken symbol ->
                    parsePrefixOperator symbol tail

                FunctionToken name ->
                    parseFunction name tail

                _ ->
                    Reader.error ("Bad atom token: " ++ tokenToString head ++ " expected atom or prefix operator at: " ++ tokensToString (head :: tail))

        consumeRightExpression : Int -> ParserOptionalConsumer (Expression context)
        consumeRightExpression minRightBindingPower leftExpr head tail dontConsume =
            case head of
                OperatorToken symbol ->
                    consumeOperator minRightBindingPower symbol leftExpr head tail dontConsume

                ClosingBracketToken ->
                    dontConsume

                CommaToken ->
                    dontConsume

                _ ->
                    Reader.error ("Bad operator token " ++ tokenToString head ++ " expected operator at: " ++ tokensToString (head :: tail))

        parseBracketsExpressions : List Token -> ParserResult (List (Expression context))
        parseBracketsExpressions tokens =
            parse_ tokens 0
                |> consumeNext "Expected closing bracket or comma" consumeBracketsExpressions

        consumeBracketsExpressions : ParserConsumer (Expression context) (List (Expression context))
        consumeBracketsExpressions current head tail =
            case head of
                ClosingBracketToken ->
                    Reader.success [ current ] tail

                CommaToken ->
                    parseBracketsExpressions tail
                        |> Reader.map ((::) current)

                _ ->
                    Reader.error "Expected closing bracket or comma"

        parsePrefixOperator : Char -> List Token -> ParserResult (Expression context)
        parsePrefixOperator symbol tokens =
            case Dict.get symbol operators of
                Just operator ->
                    if operator.prefixBindingPower >= 0 then
                        parse_ tokens operator.prefixBindingPower
                            |> Reader.map (\rightExpr -> OperatorExpression symbol operator.evaluation [ rightExpr ])

                    else
                        Reader.error ("Bad operator: " ++ String.fromChar symbol ++ " - expected prefix operator")

                _ ->
                    Reader.error ("Unknown operator: " ++ String.fromChar symbol)

        consumeOperator : Int -> Char -> ParserOptionalConsumer (Expression context)
        consumeOperator minRightBindingPower symbol leftExpr _ tail dontConsume =
            case Dict.get symbol operators of
                Just operator ->
                    if operator.leftBindingPower < minRightBindingPower then
                        dontConsume

                    else
                        parse_ tail operator.rightBindingPower
                            |> Reader.map (\rightExpr -> OperatorExpression symbol operator.evaluation [ leftExpr, rightExpr ])
                            |> Reader.tryConsumeNext (consumeRightExpression minRightBindingPower)

                _ ->
                    Reader.error ("Unknown operator: " ++ String.fromChar symbol)

        parseFunction : String -> List Token -> ParserResult (Expression context)
        parseFunction name tokens =
            case Dict.get name functions of
                Just function ->
                    case function.arity of
                        Exactly 0 ->
                            Reader.success (FunctionExpression name function.evaluation []) tokens

                        functionArity ->
                            case tokens of
                                OpeningBracketToken :: tail ->
                                    parseBracketsExpressions tail
                                        |> Reader.flatMap (checkArity name functionArity)
                                        |> Reader.map (FunctionExpression name function.evaluation)

                                _ ->
                                    Reader.error "Expected arguments in brackets"

                _ ->
                    Reader.error ("Unknown function" ++ name)
    in
    parse_ inputTokens 0
        |> Reader.finish "Some tokens hasn't been processed"
