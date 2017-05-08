module Views.Article.Favorite exposing (button)

{-| The Favorite button.
-}

import Html exposing (Html, Attribute, i, text)
import Html.Attributes exposing (class)
import Data.Article as Article exposing (Article)
import Util exposing ((=>), onClickStopPropagation)


{-| This is a "build your own element" API.

You pass it some configuration, followed by a `List (Attribute msg)` and a
`List (Html msg)`, just like any standard Html element.

-}
button :
    (Article a -> msg)
    -> Article a
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
button toggleFavorite article extraAttributes extraChildren =
    let
        favoriteButtonClass =
            if article.favorited then
                "btn-primary"
            else
                "btn-outline-primary"

        attributes =
            [ class ("btn btn-sm " ++ favoriteButtonClass)
            , onClickStopPropagation (toggleFavorite article)
            ]
                ++ extraAttributes

        children =
            [ i [ class "ion-heart" ] [] ]
                ++ extraChildren
    in
        Html.button attributes children
