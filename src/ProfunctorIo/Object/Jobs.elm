-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module ProfunctorIo.Object.Jobs exposing (..)

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


about_brief : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
about_brief =
    Object.selectionForField "(Maybe String)" "about_brief" [] (Decode.string |> Decode.nullable)


about_long : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
about_long =
    Object.selectionForField "(Maybe String)" "about_long" [] (Decode.string |> Decode.nullable)


approved : SelectionSet Bool ProfunctorIo.Object.Jobs
approved =
    Object.selectionForField "Bool" "approved" [] Decode.bool


cash_max : SelectionSet Int ProfunctorIo.Object.Jobs
cash_max =
    Object.selectionForField "Int" "cash_max" [] Decode.int


cash_min : SelectionSet Int ProfunctorIo.Object.Jobs
cash_min =
    Object.selectionForField "Int" "cash_min" [] Decode.int


company_industry : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
company_industry =
    Object.selectionForField "(Maybe String)" "company_industry" [] (Decode.string |> Decode.nullable)


company_name : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
company_name =
    Object.selectionForField "(Maybe String)" "company_name" [] (Decode.string |> Decode.nullable)


contact_email : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
contact_email =
    Object.selectionForField "(Maybe String)" "contact_email" [] (Decode.string |> Decode.nullable)


contact_telegram : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
contact_telegram =
    Object.selectionForField "(Maybe String)" "contact_telegram" [] (Decode.string |> Decode.nullable)


created_at : SelectionSet ProfunctorIo.ScalarCodecs.Timestamptz ProfunctorIo.Object.Jobs
created_at =
    Object.selectionForField "ScalarCodecs.Timestamptz" "created_at" [] (ProfunctorIo.ScalarCodecs.codecs |> ProfunctorIo.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


hashtags : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
hashtags =
    Object.selectionForField "(Maybe String)" "hashtags" [] (Decode.string |> Decode.nullable)


id : SelectionSet String ProfunctorIo.Object.Jobs
id =
    Object.selectionForField "String" "id" [] Decode.string


legacy : SelectionSet Int ProfunctorIo.Object.Jobs
legacy =
    Object.selectionForField "Int" "legacy" [] Decode.int


level : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
level =
    Object.selectionForField "(Maybe String)" "level" [] (Decode.string |> Decode.nullable)


location_city : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
location_city =
    Object.selectionForField "(Maybe String)" "location_city" [] (Decode.string |> Decode.nullable)


location_country : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
location_country =
    Object.selectionForField "(Maybe String)" "location_country" [] (Decode.string |> Decode.nullable)


location_office : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
location_office =
    Object.selectionForField "(Maybe String)" "location_office" [] (Decode.string |> Decode.nullable)


perks : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
perks =
    Object.selectionForField "(Maybe String)" "perks" [] (Decode.string |> Decode.nullable)


premium : SelectionSet Bool ProfunctorIo.Object.Jobs
premium =
    Object.selectionForField "Bool" "premium" [] Decode.bool


remote : SelectionSet (Maybe Bool) ProfunctorIo.Object.Jobs
remote =
    Object.selectionForField "(Maybe Bool)" "remote" [] (Decode.bool |> Decode.nullable)


scope : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
scope =
    Object.selectionForField "(Maybe String)" "scope" [] (Decode.string |> Decode.nullable)


stack : SelectionSet (Maybe String) ProfunctorIo.Object.Jobs
stack =
    Object.selectionForField "(Maybe String)" "stack" [] (Decode.string |> Decode.nullable)


updated_at : SelectionSet ProfunctorIo.ScalarCodecs.Timestamptz ProfunctorIo.Object.Jobs
updated_at =
    Object.selectionForField "ScalarCodecs.Timestamptz" "updated_at" [] (ProfunctorIo.ScalarCodecs.codecs |> ProfunctorIo.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)
