module Data.Article
    exposing
        ( Article
        , Body
        , Slug
        , Tag
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , decoderWithBody
        , slugParser
        , slugToString
        , tagDecoder
        , tagToString
        )

import Data.Article.Author as Author exposing (Author)
import Date exposing (Date)
import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, required)
import Markdown
import UrlParser


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
    , createdAt : Date
    , updatedAt : Date
    , favorited : Bool
    , favoritesCount : Int
    , author : Author
    , body : a
    }



-- SERIALIZATION --


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
    decode Article
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "slug" (Decode.map Slug Decode.string)
        |> required "title" Decode.string
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Json.Decode.Extra.date
        |> required "updatedAt" Json.Decode.Extra.date
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
        |> required "author" Author.decoder



-- IDENTIFIERS --


type Slug
    = Slug String


slugParser : UrlParser.Parser (Slug -> a) a
slugParser =
    UrlParser.custom "SLUG" (Ok << Slug)


slugToString : Slug -> String
slugToString (Slug slug) =
    slug



-- TAGS --


type Tag
    = Tag String


tagToString : Tag -> String
tagToString (Tag slug) =
    slug


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map Tag Decode.string



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
