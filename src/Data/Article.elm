module Data.Article
    exposing
        ( Article
        , Body
        , addBody
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , decoderWithBody
        )

import Data.Article.Author as Author exposing (Author)
import Data.Article.Slug as Slug exposing (Slug)
import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Markdown
import Time
import Url.Parser
import Util


{-| An article, optionally with an article body.

To see the difference between { body : body } and { body : Maybe Body },
consider the difference between the "view individual article" page (which
renders one article, including its body) and the "article feed" -
which displays multiple articles, but without bodies.

This definition for `Article` means we can write:

viewArticle : Article Body -> Html msg
viewFeed : List (Article ()) -> Html msg

This indicates that `viewArticle` requires an article _with a `body` present_,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Article a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Article, the `feed`
function produces `List (Article ())` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual article view.

-}
type alias Article a =
    { description : String
    , slug : Slug
    , title : String
    , tags : List String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    , author : Author
    , body : a
    }



-- SERIALIZATION


decoder : Decoder (Article ())
decoder =
    baseArticleDecoder
        |> hardcoded ()


decoderWithBody : Decoder (Article Body)
decoderWithBody =
    baseArticleDecoder
        |> required "body" bodyDecoder


baseArticleDecoder : Decoder (a -> Article a)
baseArticleDecoder =
    Decode.succeed Article
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "slug" Slug.decoder
        |> required "title" Decode.string
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Util.dateStringDecoder
        |> required "updatedAt" Util.dateStringDecoder
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
        |> required "author" Author.decoder



-- BODY --


type Body
    = Body Markdown


type alias Markdown =
    String


bodyToHtml : Body -> List (Attribute msg) -> Html msg
bodyToHtml (Body markdown) attributes =
    Markdown.toHtml attributes markdown


bodyToMarkdownString : Body -> String
bodyToMarkdownString (Body markdown) =
    markdown


bodyDecoder : Decoder Body
bodyDecoder =
    Decode.map Body Decode.string


addBody : Body -> Article () -> Article Body
addBody body article =
    { description = article.description
    , slug = article.slug
    , title = article.title
    , tags = article.tags
    , createdAt = article.createdAt
    , updatedAt = article.updatedAt
    , favorited = article.favorited
    , favoritesCount = article.favoritesCount
    , author = article.author
    , body = body
    }
