module Expression.Tokenizer exposing (..)

import Char exposing (isAlpha)
import Expression.InternalTypes exposing (..)
import Reader exposing (..)
import Set exposing (Set)
import Utils exposing (with)


tokenize : TokenizerOptions -> String -> Result String (List Token)
tokenize tokenizerOptions input =
    let
        getSimpleToken : Char -> Maybe Token
        getSimpleToken char =
            if Set.member char tokenizerOptions.operators then
                Just <| OperatorToken char

            else
                case char of
                    '(' ->
                        Just OpeningBracketToken

                    ')' ->
                        Just ClosingBracketToken

                    ',' ->
                        Just CommaToken

                    _ ->
                        Nothing

        isNameChar : Char -> Bool
        isNameChar char =
            Char.isAlpha char

        isNumberChar : Char -> Bool
        isNumberChar char =
            Char.isDigit char || char == '.'

        parseName : String -> Result TokenizerError Token
        parseName name =
            if Set.member name tokenizerOptions.functions then
                Ok (FunctionToken name)

            else
                Err ("Unknown name: " ++ name)

        parseNumber : String -> Result TokenizerError Token
        parseNumber str =
            str
                |> String.toFloat
                |> Result.fromMaybe ("Invalid number: " ++ str)
                |> Result.map NumberToken

        consumeString : (Char -> Bool) -> (String -> Result TokenizerError result) -> TokenizerResult Char -> TokenizerResult result
        consumeString predicate parser result =
            result
                |> Reader.consumeAll predicate (::)
                |> Reader.map String.fromList
                |> Reader.flatMap parser

        getTokens : TokenizerOptionalConsumer (List Token)
        getTokens current head tail _ =
            let
                consume =
                    case getSimpleToken head of
                        Just token ->
                            Reader.success (token :: current) tail

                        Nothing ->
                            if Char.isDigit head then
                                Reader.success head tail
                                    |> consumeString isNumberChar parseNumber
                                    |> Reader.map (with current (::))

                            else if Char.isAlpha head then
                                Reader.success head tail
                                    |> consumeString isNameChar parseName
                                    |> Reader.map (with current (::))

                            else if head == ' ' || head == '\t' || head == '\u{000D}' || head == '\n' then
                                -- formatter, wtf? \u{000D} is \r
                                Reader.success current tail

                            else
                                Reader.error ("Unknown symbol: " ++ String.fromChar head)
            in
            consume
                |> Reader.tryConsumeNext getTokens
    in
    Reader.success [] (String.toList input)
        |> Reader.tryConsumeNext getTokens
        |> Reader.map List.reverse
        |> Reader.finish "Some characters hasn't been processed"
