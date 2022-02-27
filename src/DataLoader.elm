module DataLoader exposing (makeRequest)

import Dict
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Model exposing (UserData)
import ProfunctorIo.Object
import ProfunctorIo.Object.Downvotes_by_submitters as DownvotesInfo
import ProfunctorIo.Object.Submits_by_users as SubmitsInfo
import ProfunctorIo.Object.Upvotes_by_submitters as UpvotesInfo
import ProfunctorIo.Query as Query
import ProfunctorIo.Scalar
import Utils exposing (associate)


type alias Response =
    { downvotes_by_submitters : List RawUserCount
    , upvotes_by_submitters : List RawUserCount
    , submits_by_users : List RawUserCount
    }


type alias RawUserCount =
    { count : Maybe ProfunctorIo.Scalar.Bigint
    , user : Maybe String
    }


type alias UserCount =
    { user : String
    , count : Int
    }


downvotesSelection : SelectionSet RawUserCount ProfunctorIo.Object.Downvotes_by_submitters
downvotesSelection =
    SelectionSet.map2 RawUserCount DownvotesInfo.count DownvotesInfo.submitted_by_username


upvotesSelection : SelectionSet RawUserCount ProfunctorIo.Object.Upvotes_by_submitters
upvotesSelection =
    SelectionSet.map2 RawUserCount UpvotesInfo.count UpvotesInfo.submitted_by_username


submitsSelection : SelectionSet RawUserCount ProfunctorIo.Object.Submits_by_users
submitsSelection =
    SelectionSet.map2 RawUserCount SubmitsInfo.count SubmitsInfo.submitted_by_username


query : SelectionSet Response RootQuery
query =
    SelectionSet.map3 Response
        (Query.downvotes_by_submitters identity downvotesSelection)
        (Query.upvotes_by_submitters identity upvotesSelection)
        (Query.submits_by_users identity submitsSelection)


convertData : Response -> List UserData
convertData response =
    let
        getCount rawCount =
            case rawCount of
                ProfunctorIo.Scalar.Bigint value ->
                    String.toInt value

        parse : RawUserCount -> Maybe UserCount
        parse raw =
            raw.count
                |> Maybe.andThen getCount
                |> Maybe.map2 UserCount raw.user

        downvotes =
            response.downvotes_by_submitters
                |> List.filterMap parse
                |> associate .user .count

        upvotes =
            response.upvotes_by_submitters
                |> List.filterMap parse
                |> associate .user .count

        combine : UserCount -> UserData
        combine submitsData =
            { username = submitsData.user
            , posts = submitsData.count
            , downvotes = Dict.get submitsData.user downvotes |> Maybe.withDefault 0
            , upvotes = Dict.get submitsData.user upvotes |> Maybe.withDefault 0
            }
    in
    response.submits_by_users
        |> List.filterMap parse
        |> List.map combine


convertError : Graphql.Http.Error Response -> String
convertError error =
    "Error loading data from profunctor.io"


convertResponse : Result (Graphql.Http.Error Response) Response -> Result String (List UserData)
convertResponse result =
    result
        |> Result.map convertData
        |> Result.mapError convertError


makeRequest : (Result String (List UserData) -> c) -> Cmd c
makeRequest messageProducer =
    query
        |> Graphql.Http.queryRequest "https://api.profunctor.io/v1/graphql"
        |> Graphql.Http.send (convertResponse >> messageProducer)
