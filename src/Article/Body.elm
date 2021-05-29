module Article.Body exposing (Body, MarkdownString, decoder, toHtml, toMarkdownString)

import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder)
import Markdown



-- TYPES


type Body
    = Body MarkdownString


{-| Internal use only. I want to remind myself that the string inside Body contains markdown.
-}
type alias MarkdownString =
    String



-- CONVERSIONS


toHtml (Body markdown) attributes =
    Markdown.toHtml attributes markdown


toMarkdownString (Body markdown) =
    markdown


decoder =
    Decode.map Body Decode.string
