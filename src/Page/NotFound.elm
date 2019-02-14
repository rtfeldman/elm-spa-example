module Page.NotFound exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Asset
import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)
import Session exposing (Session)


type alias Model =
    { session : Session
    }


type Msg
    = GotSession Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            , div [ class "row" ]
                [ img [ Asset.src Asset.error ] [] ]
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
