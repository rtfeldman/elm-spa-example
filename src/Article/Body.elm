module Article.Body exposing (Body, MarkdownString, decoder, toHtml, toMarkdownString)

-- TYPES


type Body
    = Body MarkdownString


{-| Internal use only. I want to remind myself that the string inside Body contains markdown.
-}
type alias MarkdownString =
    String



-- CONVERSIONS


toHtml : Body -> List (Attribute msg) -> Html msg
toHtml (Body markdown) attributes =
    Markdown.toHtml attributes markdown


toMarkdownString : Body -> MarkdownString
toMarkdownString (Body markdown) =
    markdown


decoder : Decoder Body
decoder =
    Decode.map Body Decode.string
