module Page.Logout exposing (Model, Msg, page)

import Api
import Effect exposing (Effect)
import Html
import Page
import Session exposing (Session)
import Spa.Page
import View exposing (View)


page session =
    Spa.Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    ()


type alias Msg =
    ()


init : () -> ( Model, Effect Session.Msg Msg )
init _ =
    ( (), Effect.fromCmd Api.logout )


update : Msg -> Model -> ( Model, Effect Session.Msg Msg )
update _ model =
    ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> View Msg
view _ =
    { title = "Logout"
    , page = Page.Other
    , content = Html.text "Logout in progress"
    }
