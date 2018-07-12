module Feed
    exposing
        ( Feed
        , FeedConfig
        , ListConfig
        , defaultFeedConfig
        , defaultListConfig
        , feed
        , list
        )

import Data.Article as Article exposing (Article)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Feed =
    { articles : List (Article ())
    , articlesCount : Int
    }



-- SERIALIZATION


decoder : Decoder Feed
decoder =
    Decode.succeed Feed
        |> required "articles" (Decode.list Article.decoder)
        |> required "articlesCount" Decode.int



-- LIST --


type alias ListConfig =
    { tag : Maybe Tag
    , author : Maybe Username
    , favorited : Maybe Username
    , limit : Int
    , offset : Int
    }


defaultListConfig : ListConfig
defaultListConfig =
    { tag = Nothing
    , author = Nothing
    , favorited = Nothing
    , limit = 20
    , offset = 0
    }


list : ListConfig -> Maybe AuthToken -> Http.Request Feed
list config maybeToken =
    [ Maybe.map (\tag -> ( "tag", Tag.toString tag )) config.tag
    , Maybe.map (\author -> ( "author", Username.toString author )) config.author
    , Maybe.map (\favorited -> ( "favorited", Username.toString favorited )) config.favorited
    , Just ( "limit", String.fromInt config.limit )
    , Just ( "offset", String.fromInt config.offset )
    ]
        |> List.filterMap identity
        |> buildFromQueryParams "/articles"
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FEED --


type alias FeedConfig =
    { limit : Int
    , offset : Int
    }


defaultFeedConfig : FeedConfig
defaultFeedConfig =
    { limit = 10
    , offset = 0
    }


feed : FeedConfig -> AuthToken -> Http.Request Feed
feed config token =
    [ ( "limit", String.fromInt config.limit )
    , ( "offset", String.fromInt config.offset )
    ]
        |> buildFromQueryParams "/articles/feed"
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- HELPERS --


buildFromQueryParams : String -> List ( String, String ) -> RequestBuilder Feed
buildFromQueryParams url queryParams =
    url
        |> apiUrl
        |> HttpBuilder.get
        |> withExpect (Http.expectJson Feed.decoder)
        |> withQueryParams queryParams
