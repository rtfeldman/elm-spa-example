module Views.Errors exposing (view)

{-| Render dismissable errors. We use this all over the place!
-}

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


view : msg -> List String -> Html msg
view dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.append
                (List.map (\error -> p [] [ text error ]) errors)
                [ button [ onClick dismissErrors ] [ text "Ok" ] ]
