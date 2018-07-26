module Main exposing (main)

import Data.Article exposing (Slug)
import Data.Session exposing (Session)
import Data.User as User exposing (User, Username)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Ports
import Route exposing (Route)
import Task
import Util exposing ((=>))
import Views.Page as Page exposing (ActivePage)


-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!


type Page
    = Blank
    | NotFound
    | Home Home.Model
    | Login Login.Model



-- MODEL --


type alias Model =
    { session : Session
    , page : Page
    , error : Maybe String
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { page = initialPage
        , session = { user = decodeUserFromJson val }
        , error = Nothing
        }


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    viewPage model.session model.page


viewPage : Session -> Page -> Html Msg
viewPage session page =
    let
        frame =
            Page.frame session.user
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> frame Page.Other

            Blank ->
                -- This is for the very initial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame Page.Other

            Home subModel ->
                Home.view session subModel
                    |> frame Page.Home
                    |> Html.map HomeMsg

            Login subModel ->
                Login.view session subModel
                    |> frame Page.Other
                    |> Html.map LoginMsg



-- SUBSCRIPTIONS --
-- Note: we aren't currently doing any page subscriptions, but I thought it would
-- be a good idea to put this in here as an example. If I were actually
-- maintaining this in production, I wouldn't bother until I needed this!


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.page
        , Sub.map SetUser sessionChange
        ]


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    Sub.none


type Msg
    = SetRoute (Maybe Route)
    | HomeMsg Home.Msg
      -- | SettingsMsg Settings.Msg
    | SetUser (Maybe User)
    | LoginMsg Login.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | page = NotFound } => Cmd.none

        Just route ->
            case route of
                Route.Home ->
                    let
                        ( homeModel, homeCmd ) =
                            Home.initialize model.session
                    in
                        ( { model | page = Home homeModel }, Cmd.map HomeMsg homeCmd )

                -- Just Route.Root ->
                --     model => Route.modifyUrl Route.Home
                Route.Login ->
                    { model | page = (Login Login.initialModel) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        --
        -- errored =
        --     pageErrored model
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( SetUser user, _ ) ->
                let
                    cmd =
                        -- If we just signed out, then redirect to Home.
                        if session.user /= Nothing && user == Nothing then
                            Route.modifyUrl Route.Home
                        else
                            Cmd.none
                in
                    { model | session = { session | user = user } }
                        => cmd

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update subMsg subModel

                    newModel =
                        case msgFromPage of
                            Login.NoOp ->
                                model

                            Login.SetUser user ->
                                { model | session = { user = Just user } }
                in
                    { newModel | page = (Login pageModel) }
                        => Cmd.map LoginMsg cmd

            ( HomeMsg subMsg, Home subModel ) ->
                let
                    result =
                        Home.update session subMsg subModel
                in
                    case result of
                        Err error ->
                            ( { model | error = Just error }, Cmd.none )

                        Ok ( homeModel, homeCmd ) ->
                            ( { model | page = Home homeModel }, Cmd.map HomeMsg homeCmd )

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                model => Cmd.none

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                model => Cmd.none



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
