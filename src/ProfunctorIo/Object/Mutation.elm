-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module ProfunctorIo.Object.Mutation exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import ProfunctorIo.InputObject
import ProfunctorIo.Interface
import ProfunctorIo.Object
import ProfunctorIo.Scalar
import ProfunctorIo.ScalarCodecs
import ProfunctorIo.Union


logout :
    SelectionSet decodesTo ProfunctorIo.Object.LogoutResponse
    -> SelectionSet (Maybe decodesTo) ProfunctorIo.Object.Mutation
logout object____ =
    Object.selectionForCompositeField "logout" [] object____ (Basics.identity >> Decode.nullable)


type alias TgAuthOptionalArguments =
    { input : OptionalArgument ProfunctorIo.InputObject.TGAuthRequestInput }


tg_auth :
    (TgAuthOptionalArguments -> TgAuthOptionalArguments)
    -> SelectionSet decodesTo ProfunctorIo.Object.AuthPayload
    -> SelectionSet (Maybe decodesTo) ProfunctorIo.Object.Mutation
tg_auth fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { input = Absent }

        optionalArgs____ =
            [ Argument.optional "input" filledInOptionals____.input ProfunctorIo.InputObject.encodeTGAuthRequestInput ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tg_auth" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


tg_auth_refresh_jwt_token :
    SelectionSet decodesTo ProfunctorIo.Object.AuthPayload
    -> SelectionSet (Maybe decodesTo) ProfunctorIo.Object.Mutation
tg_auth_refresh_jwt_token object____ =
    Object.selectionForCompositeField "tg_auth_refresh_jwt_token" [] object____ (Basics.identity >> Decode.nullable)
