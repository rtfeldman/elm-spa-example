module Views.Errors exposing (view)

{-| Render dismissable errors. We use this all over the place!
-}

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Util exposing ((=>))


view : msg -> List String -> Html msg
view dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""
    else
        div [ class "error-messages", styles ] <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]


styles : Attribute msg
styles =
    style
        [ "position" => "fixed"
        , "top" => "0"
        , "background" => "rgb(250, 250, 250)"
        , "padding" => "20px"
        , "border" => "1px solid"
        ]
