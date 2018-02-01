module Main exposing (main)

import Data.Article exposing (Slug)
import Data.Session exposing (Session)
import Data.User as User exposing (User, Username)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
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
    | Errored PageLoadError
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | Register Register.Model
    | Profile Username Profile.Model
    | Article Article.Model
    | Editor (Maybe Slug) Editor.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = { user = decodeUserFromJson val }
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
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
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

        Errored subModel ->
            Errored.view session subModel
                |> frame Page.Other

        Settings subModel ->
            Settings.view session subModel
                |> frame Page.Other
                |> Html.map SettingsMsg

        Home subModel ->
            Home.view session subModel
                |> frame Page.Home
                |> Html.map HomeMsg

        Login subModel ->
            Login.view session subModel
                |> frame Page.Other
                |> Html.map LoginMsg

        Register subModel ->
            Register.view session subModel
                |> frame Page.Other
                |> Html.map RegisterMsg

        Profile username subModel ->
            Profile.view session subModel
                |> frame (Page.Profile username)
                |> Html.map ProfileMsg

        Article subModel ->
            Article.view session subModel
                |> frame Page.Other
                |> Html.map ArticleMsg

        Editor maybeSlug subModel ->
            let
                framePage =
                    if maybeSlug == Nothing then
                        Page.NewArticle
                    else
                        Page.Other
            in
            Editor.view subModel
                |> frame framePage
                |> Html.map EditorMsg



-- SUBSCRIPTIONS --
-- Note: we aren't currently doing any page subscriptions, but I thought it would
-- be a good idea to put this in here as an example. If I were actually
-- maintaining this in production, I wouldn't bother until I needed this!


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
        , Sub.map SetUser sessionChange
        ]


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        Settings _ ->
            Sub.none

        Home _ ->
            Sub.none

        Login _ ->
            Sub.none

        Register _ ->
            Sub.none

        Profile _ _ ->
            Sub.none

        Article _ ->
            Sub.none

        Editor _ _ ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Home.Model)
    | ArticleLoaded (Result PageLoadError Article.Model)
    | ProfileLoaded Username (Result PageLoadError Profile.Model)
    | EditArticleLoaded Slug (Result PageLoadError Editor.Model)
    | HomeMsg Home.Msg
    | SettingsMsg Settings.Msg
    | SetUser (Maybe User)
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | ProfileMsg Profile.Msg
    | ArticleMsg Article.Msg
    | EditorMsg Editor.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task

        errored =
            pageErrored model
    in
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } => Cmd.none

        Just Route.NewArticle ->
            case model.session.user of
                Just user ->
                    { model | pageState = Loaded (Editor Nothing Editor.initNew) } => Cmd.none

                Nothing ->
                    errored Page.NewArticle "You must be signed in to post an article."

        Just (Route.EditArticle slug) ->
            case model.session.user of
                Just user ->
                    transition (EditArticleLoaded slug) (Editor.initEdit model.session slug)

                Nothing ->
                    errored Page.Other "You must be signed in to edit an article."

        Just Route.Settings ->
            case model.session.user of
                Just user ->
                    { model | pageState = Loaded (Settings (Settings.init user)) } => Cmd.none

                Nothing ->
                    errored Page.Settings "You must be signed in to access your settings."

        Just Route.Home ->
            transition HomeLoaded (Home.init model.session)

        Just Route.Root ->
            model => Route.modifyUrl Route.Home

        Just Route.Login ->
            { model | pageState = Loaded (Login Login.initialModel) } => Cmd.none

        Just Route.Logout ->
            let
                session =
                    model.session
            in
            { model | session = { session | user = Nothing } }
                => Cmd.batch
                    [ Ports.storeSession Nothing
                    , Route.modifyUrl Route.Home
                    ]

        Just Route.Register ->
            { model | pageState = Loaded (Register Register.initialModel) } => Cmd.none

        Just (Route.Profile username) ->
            transition (ProfileLoaded username) (Profile.init model.session username)

        Just (Route.Article slug) ->
            transition ArticleLoaded (Article.init model.session slug)


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    { model | pageState = Loaded (Errored error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            setRoute route model

        ( HomeLoaded (Ok subModel), _ ) ->
            { model | pageState = Loaded (Home subModel) } => Cmd.none

        ( HomeLoaded (Err error), _ ) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        ( ProfileLoaded username (Ok subModel), _ ) ->
            { model | pageState = Loaded (Profile username subModel) } => Cmd.none

        ( ProfileLoaded username (Err error), _ ) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        ( ArticleLoaded (Ok subModel), _ ) ->
            { model | pageState = Loaded (Article subModel) } => Cmd.none

        ( ArticleLoaded (Err error), _ ) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        ( EditArticleLoaded slug (Ok subModel), _ ) ->
            { model | pageState = Loaded (Editor (Just slug) subModel) } => Cmd.none

        ( EditArticleLoaded slug (Err error), _ ) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

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

        ( SettingsMsg subMsg, Settings subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Settings.update model.session subMsg subModel

                newModel =
                    case msgFromPage of
                        Settings.NoOp ->
                            model

                        Settings.SetUser user ->
                            { model | session = { user = Just user } }
            in
            { newModel | pageState = Loaded (Settings pageModel) }
                => Cmd.map SettingsMsg cmd

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
            { newModel | pageState = Loaded (Login pageModel) }
                => Cmd.map LoginMsg cmd

        ( RegisterMsg subMsg, Register subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Register.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Register.NoOp ->
                            model

                        Register.SetUser user ->
                            { model | session = { user = Just user } }
            in
            { newModel | pageState = Loaded (Register pageModel) }
                => Cmd.map RegisterMsg cmd

        ( HomeMsg subMsg, Home subModel ) ->
            toPage Home HomeMsg (Home.update session) subMsg subModel

        ( ProfileMsg subMsg, Profile username subModel ) ->
            toPage (Profile username) ProfileMsg (Profile.update model.session) subMsg subModel

        ( ArticleMsg subMsg, Article subModel ) ->
            toPage Article ArticleMsg (Article.update model.session) subMsg subModel

        ( EditorMsg subMsg, Editor slug subModel ) ->
            case model.session.user of
                Nothing ->
                    if slug == Nothing then
                        errored Page.NewArticle
                            "You must be signed in to post articles."
                    else
                        errored Page.Other
                            "You must be signed in to edit articles."

                Just user ->
                    toPage (Editor slug) EditorMsg (Editor.update user) subMsg subModel

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
