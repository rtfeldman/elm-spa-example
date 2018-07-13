module Views.Article exposing (view, viewTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

import Article exposing (Article)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Profile
import Route exposing (Route)
import Time
import UserPhoto exposing (UserPhoto)
import Util
import Views.Article.Favorite as Favorite
import Views.Author



-- TIMESTAMP ONLY


{-| Some pages want to view only the timestamp, not the entire article preview.
-}
viewTimestamp : Time.Zone -> Article a -> Html msg
viewTimestamp timeZone article =
    span [ class "date" ] [ text (Util.formatTimestamp timeZone (Article.createdAt article)) ]



-- ARTICLE PREVIEW


view : (Article a -> msg) -> Time.Zone -> Article a -> Html msg
view toggleFavorite timeZone article =
    let
        profile =
            Article.author article

        username =
            Profile.username profile
    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile username) ]
                [ img [ UserPhoto.src (Profile.image profile) ] [] ]
            , div [ class "info" ]
                [ Views.Author.view username
                , viewTimestamp timeZone article
                ]
            , Favorite.button
                toggleFavorite
                article
                [ class "pull-xs-right" ]
                [ text (" " ++ String.fromInt (Article.favoritesCount article)) ]
            ]
        , a [ class "preview-link", Route.href (Route.Article (Article.slug article)) ]
            [ h1 [] [ text (Article.title article) ]
            , p [] [ text (Article.description article) ]
            , span [] [ text "Read more..." ]
            ]
        ]
