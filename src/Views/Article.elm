module Views.Article exposing (view, viewTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, href, src, id, placeholder, attribute, classList)
import Data.Article exposing (Article)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Date.Format
import Views.Author
import Views.Article.Favorite as Favorite


-- VIEWS --


{-| Some pages want to view just the timestamp, not the whole article.
-}
viewTimestamp : Article a -> Html msg
viewTimestamp article =
    span [ class "date" ] [ text (formattedTimestamp article) ]


view : (Article a -> msg) -> Article a -> Html msg
view toggleFavorite article =
    let
        author =
            article.author
    in
        div [ class "article-preview" ]
            [ div [ class "article-meta" ]
                [ a [ Route.href (Route.Profile author.username) ]
                    [ img [ UserPhoto.src author.image ] [] ]
                , div [ class "info" ]
                    [ Views.Author.view author.username
                    , span [ class "date" ] [ text (formattedTimestamp article) ]
                    ]
                , Favorite.button
                    toggleFavorite
                    article
                    [ class "pull-xs-right" ]
                    [ text (" " ++ toString article.favoritesCount) ]
                ]
            , a [ class "preview-link", Route.href (Route.Article article.slug) ]
                [ h1 [] [ text article.title ]
                , p [] [ text article.description ]
                , span [] [ text "Read more..." ]
                ]
            ]



-- INTERNAL --


formattedTimestamp : Article a -> String
formattedTimestamp article =
    Date.Format.format "%B %e, %Y" article.createdAt
