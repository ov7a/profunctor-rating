-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module ProfunctorIo.Scalar exposing (Bigint(..), Codecs, Id(..), Timestamptz(..), Upload(..), Uuid(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Bigint
    = Bigint String


type Id
    = Id String


type Timestamptz
    = Timestamptz String


type Upload
    = Upload String


type Uuid
    = Uuid String


defineCodecs :
    { codecBigint : Codec valueBigint
    , codecId : Codec valueId
    , codecTimestamptz : Codec valueTimestamptz
    , codecUpload : Codec valueUpload
    , codecUuid : Codec valueUuid
    }
    -> Codecs valueBigint valueId valueTimestamptz valueUpload valueUuid
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueBigint valueId valueTimestamptz valueUpload valueUuid
    ->
        { codecBigint : Codec valueBigint
        , codecId : Codec valueId
        , codecTimestamptz : Codec valueTimestamptz
        , codecUpload : Codec valueUpload
        , codecUuid : Codec valueUuid
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder :
    (RawCodecs valueBigint valueId valueTimestamptz valueUpload valueUuid -> Codec getterValue)
    -> Codecs valueBigint valueId valueTimestamptz valueUpload valueUuid
    -> getterValue
    -> Graphql.Internal.Encode.Value
unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueBigint valueId valueTimestamptz valueUpload valueUuid
    = Codecs (RawCodecs valueBigint valueId valueTimestamptz valueUpload valueUuid)


type alias RawCodecs valueBigint valueId valueTimestamptz valueUpload valueUuid =
    { codecBigint : Codec valueBigint
    , codecId : Codec valueId
    , codecTimestamptz : Codec valueTimestamptz
    , codecUpload : Codec valueUpload
    , codecUuid : Codec valueUuid
    }


defaultCodecs : RawCodecs Bigint Id Timestamptz Upload Uuid
defaultCodecs =
    { codecBigint =
        { encoder = \(Bigint raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Bigint
        }
    , codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    , codecTimestamptz =
        { encoder = \(Timestamptz raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Timestamptz
        }
    , codecUpload =
        { encoder = \(Upload raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Upload
        }
    , codecUuid =
        { encoder = \(Uuid raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Uuid
        }
    }
