module Views.Article exposing (view, viewTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

import Data.Article exposing (Article)
import Data.User.Photo as UserPhoto exposing (UserPhoto)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Route exposing (Route)
import Time
import Util
import Views.Article.Favorite as Favorite
import Views.Author



-- TIMESTAMP ONLY


{-| Some pages want to view only the timestamp, not the entire article preview.
-}
viewTimestamp : Time.Zone -> Article a -> Html msg
viewTimestamp timeZone article =
    span [ class "date" ] [ text (Util.formatTimestamp timeZone article.createdAt) ]



-- ARTICLE PREVIEW


view : (Article a -> msg) -> Time.Zone -> Article a -> Html msg
view toggleFavorite timeZone article =
    let
        { username, image } =
            article.author
    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile username) ]
                [ img [ UserPhoto.src image ] [] ]
            , div [ class "info" ]
                [ Views.Author.view username
                , viewTimestamp timeZone article
                ]
            , Favorite.button
                toggleFavorite
                article
                [ class "pull-xs-right" ]
                [ text (" " ++ String.fromInt article.favoritesCount) ]
            ]
        , a [ class "preview-link", Route.href (Route.Article article.slug) ]
            [ h1 [] [ text article.title ]
            , p [] [ text article.description ]
            , span [] [ text "Read more..." ]
            ]
        ]
