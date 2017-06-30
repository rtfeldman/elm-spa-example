module Page.NotFound exposing (view)

import Data.Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)
import Views.Assets as Assets


-- VIEW --


view : Session -> Html msg
view session =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Not Found" ]
        , div [ class "row" ]
            [ img [ Assets.src Assets.error, alt "giant laser walrus wreaking havoc" ] [] ]
        ]
