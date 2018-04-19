module Data.Article.Slug exposing (Slug, eq, parser, toString)

import Url.Parser


type Slug
    = Slug String


parser : Url.Parser.Parser (Slug -> a) a
parser =
    Url.Parser.custom "SLUG" (Just << Slug)


toString : Slug -> String
toString (Slug slug) =
    slug
