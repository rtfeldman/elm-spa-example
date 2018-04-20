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

import Data.Article as Article exposing (Article, Body)
import Data.Article.Feed as Feed exposing (Feed)
import Data.Article.Slug as Slug exposing (Slug)
import Data.Article.Tag as Tag exposing (Tag)
import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.User as User
import Data.User.Username as Username exposing (Username)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParameters)
import Json.Decode as Decode
import Json.Encode as Encode
import Request.Helpers exposing (apiUrl)
import Url exposing (QueryParameter)


-- SINGLE --


get : Maybe AuthToken -> Slug -> Http.Request (Article Body)
get maybeToken slug =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson
    in
    apiUrl ("/articles/" ++ Slug.toString slug)
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
    [ Maybe.map (Tag.toString >> Url.string "tag") config.tag
    , Maybe.map (Username.toString >> Url.string "author") config.author
    , Maybe.map (Username.toString >> Url.string "favorited") config.favorited
    , Just (Url.int "limit" config.limit)
    , Just (Url.int "offset" config.offset)
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
    [ Url.int "limit" config.limit
    , Url.int "offset" config.offset
    ]
        |> buildFromQueryParams "/articles/feed"
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- TAGS --


tags : Http.Request (List Tag)
tags =
    Decode.field "tags" (Decode.list Tag.decoder)
        |> Http.get (apiUrl "/tags")



-- FAVORITE --


toggleFavorite : Article a -> AuthToken -> Http.Request (Article ())
toggleFavorite article authToken =
    if article.favorited then
        unfavorite article.slug authToken
    else
        favorite article.slug authToken


favorite : Slug -> AuthToken -> Http.Request (Article ())
favorite =
    buildFavorite HttpBuilder.post


unfavorite : Slug -> AuthToken -> Http.Request (Article ())
unfavorite =
    buildFavorite HttpBuilder.delete


buildFavorite :
    (String -> RequestBuilder a)
    -> Slug
    -> AuthToken
    -> Http.Request (Article ())
buildFavorite builderFromUrl slug token =
    let
        expect =
            Article.decoder
                |> Decode.field "article"
                |> Http.expectJson
    in
    [ apiUrl "/articles", Slug.toString slug, "favorite" ]
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
                [ ( "title", Encode.string config.title )
                , ( "description", Encode.string config.description )
                , ( "body", Encode.string config.body )
                , ( "tagList", Encode.list Encode.string config.tags )
                ]

        body =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    apiUrl "/articles"
        |> HttpBuilder.post
        |> withAuthorization (Just token)
        |> withBody body
        |> withExpect expect
        |> HttpBuilder.toRequest


update : Slug -> EditConfig record -> AuthToken -> Http.Request (Article Body)
update slug config token =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string config.title )
                , ( "description", Encode.string config.description )
                , ( "body", Encode.string config.body )
                ]

        body =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    apiUrl ("/articles/" ++ Slug.toString slug)
        |> HttpBuilder.put
        |> withAuthorization (Just token)
        |> withBody body
        |> withExpect expect
        |> HttpBuilder.toRequest



-- DELETE --


delete : Slug -> AuthToken -> Http.Request ()
delete slug token =
    apiUrl ("/articles/" ++ Slug.toString slug)
        |> HttpBuilder.delete
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- HELPERS --


buildFromQueryParams : String -> List QueryParameter -> RequestBuilder Feed
buildFromQueryParams url queryParams =
    url
        |> apiUrl
        |> HttpBuilder.get
        |> withExpect (Http.expectJson Feed.decoder)
        |> withQueryParameters queryParams
