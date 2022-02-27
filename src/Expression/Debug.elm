module Expression.Debug exposing (..)

import Expression.InternalTypes exposing (..)


tokenToString : Token -> String
tokenToString token =
    case token of
        NumberToken number ->
            "number " ++ String.fromFloat number

        FunctionToken name ->
            "function " ++ name

        OperatorToken symbol ->
            "operator " ++ String.fromChar symbol

        OpeningBracketToken ->
            "("

        ClosingBracketToken ->
            ")"

        CommaToken ->
            ","


tokensToString : List Token -> String
tokensToString list =
    list
        |> List.map tokenToString
        |> String.join ","
