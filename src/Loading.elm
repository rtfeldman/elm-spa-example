module Loading exposing (error, icon, slowThreshold)

{-| A loading spinner icon.
-}

import Asset
import Html exposing (Attribute, Html)
import Html.Attributes exposing (alt, height, src, width)
import Process
import Task exposing (Task)


icon =
    Html.img
        [ Asset.src Asset.loading
        , width 64
        , height 64
        , alt "Loading..."
        ]
        []


error str = Html.text ("Error loading " ++ str ++ ".")


slowThreshold = Process.sleep 500
