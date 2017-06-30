module Request.Article
    exposing
        ( FeedConfig
        , ListConfig
        , create
        , defaultFeedConfig
        , defaultListConfig
        , delete
        , feed
        , get
        , list
        , tags
        , toggleFavorite
        , update
        )

import Data.Article as Article exposing (Article, Body, Tag, slugToString)
import Data.Article.Feed as Feed exposing (Feed)
import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.User as User exposing (Username)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Request.Helpers exposing (apiUrl)
import Util exposing ((=>))


-- SINGLE --


get : Maybe AuthToken -> Article.Slug -> Http.Request (Article Body)
get maybeToken slug =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson
    in
    apiUrl ("/articles/" ++ Article.slugToString slug)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



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
    [ "tag" => Maybe.map Article.tagToString config.tag
    , "author" => Maybe.map User.usernameToString config.author
    , "favorited" => Maybe.map User.usernameToString config.favorited
    , "limit" => Just (toString config.limit)
    , "offset" => Just (toString config.offset)
    ]
        |> List.filterMap maybeVal
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
    [ "limit" => Just (toString config.limit)
    , "offset" => Just (toString config.offset)
    ]
        |> List.filterMap maybeVal
        |> buildFromQueryParams "/articles/feed"
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- TAGS --


tags : Http.Request (List Tag)
tags =
    Decode.field "tags" (Decode.list Article.tagDecoder)
        |> Http.get (apiUrl "/tags")



-- FAVORITE --


toggleFavorite : Article a -> AuthToken -> Http.Request (Article ())
toggleFavorite article authToken =
    if article.favorited then
        unfavorite article.slug authToken
    else
        favorite article.slug authToken


favorite : Article.Slug -> AuthToken -> Http.Request (Article ())
favorite =
    buildFavorite HttpBuilder.post


unfavorite : Article.Slug -> AuthToken -> Http.Request (Article ())
unfavorite =
    buildFavorite HttpBuilder.delete


buildFavorite :
    (String -> RequestBuilder a)
    -> Article.Slug
    -> AuthToken
    -> Http.Request (Article ())
buildFavorite builderFromUrl slug token =
    let
        expect =
            Article.decoder
                |> Decode.field "article"
                |> Http.expectJson
    in
    [ apiUrl "/articles", slugToString slug, "favorite" ]
        |> String.join "/"
        |> builderFromUrl
        |> withAuthorization (Just token)
        |> withExpect expect
        |> HttpBuilder.toRequest



-- CREATE --


type alias CreateConfig record =
    { record
        | title : String
        , description : String
        , body : String
        , tags : List String
    }


type alias EditConfig record =
    { record
        | title : String
        , description : String
        , body : String
    }


create : CreateConfig record -> AuthToken -> Http.Request (Article Body)
create config token =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ "title" => Encode.string config.title
                , "description" => Encode.string config.description
                , "body" => Encode.string config.body
                , "tagList" => Encode.list (List.map Encode.string config.tags)
                ]

        body =
            Encode.object [ "article" => article ]
                |> Http.jsonBody
    in
    apiUrl "/articles"
        |> HttpBuilder.post
        |> withAuthorization (Just token)
        |> withBody body
        |> withExpect expect
        |> HttpBuilder.toRequest


update : Article.Slug -> EditConfig record -> AuthToken -> Http.Request (Article Body)
update slug config token =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ "title" => Encode.string config.title
                , "description" => Encode.string config.description
                , "body" => Encode.string config.body
                ]

        body =
            Encode.object [ "article" => article ]
                |> Http.jsonBody
    in
    apiUrl ("/articles/" ++ slugToString slug)
        |> HttpBuilder.put
        |> withAuthorization (Just token)
        |> withBody body
        |> withExpect expect
        |> HttpBuilder.toRequest



-- DELETE --


delete : Article.Slug -> AuthToken -> Http.Request ()
delete slug token =
    apiUrl ("/articles/" ++ Article.slugToString slug)
        |> HttpBuilder.delete
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- HELPERS --


maybeVal : ( a, Maybe b ) -> Maybe ( a, b )
maybeVal ( key, value ) =
    case value of
        Nothing ->
            Nothing

        Just val ->
            Just (key => val)


buildFromQueryParams : String -> List ( String, String ) -> RequestBuilder Feed
buildFromQueryParams url queryParams =
    url
        |> apiUrl
        |> HttpBuilder.get
        |> withExpect (Http.expectJson Feed.decoder)
        |> withQueryParams queryParams
