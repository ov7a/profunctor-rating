module Expression.InternalTypes exposing (..)

import Html exposing (output)
import Reader exposing (..)
import Set exposing (Set)


type alias ParserError =
    String


type alias ParserResult result =
    ReaderResult ParserError result Token


type alias ParserConsumer input output =
    ReaderConsumer ParserError input output Token


type alias ParserOptionalConsumer result =
    ReaderOptionalConsumer ParserError result Token


type alias TokenizerOptions =
    { functions : Set String
    , operators : Set Char
    }


type Token
    = NumberToken Float
    | FunctionToken String
    | OperatorToken Char
    | OpeningBracketToken
    | ClosingBracketToken
    | CommaToken


type alias TokenizerError =
    String


type alias TokenizerResult result =
    ReaderResult TokenizerError result Char


type alias TokenizerOptionalConsumer result =
    ReaderOptionalConsumer TokenizerError result Char
