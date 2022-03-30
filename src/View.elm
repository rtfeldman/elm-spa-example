module View exposing (..)

import Html exposing (Html)
import Page exposing (Page)


type alias View msg =
    { title : String
    , page : Page
    , content : Html msg
    }


map : (msg -> msg1) -> View msg -> View msg1
map fn view =
    { title = view.title
    , page = view.page
    , content = Html.map fn view.content
    }


default : View msg
default =
    { title = ""
    , page = Page.Other
    , content = Html.text ""
    }
