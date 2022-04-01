module Main exposing (main)

import Api exposing (Cred)
import Article.Slug exposing (Slug)
import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Effect
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import Spa.PageStack
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)
import View exposing (View)
import Viewer exposing (Viewer)



-- NOTE: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, it's possible
-- that most of this file may become unnecessary in a future release of Elm.
-- Avoid putting things in this module unless there is no alternative!
-- See https://discourse.elm-lang.org/t/elm-spa-in-0-19/1800/2 for more.


type Model
    = Redirect Session
    | NotFound Session
    | Stack Session StackModel



-- Elm SPA pages


type alias StackModel =
    Spa.PageStack.Model Never StackCurrentModel StackPreviousModel


type alias StackCurrentModel =
    Editor.Model


type alias StackPreviousModel =
    Spa.PageStack.Model
        Never
        Article.Model
        (Spa.PageStack.Model
            Never
            Register.Model
            (Spa.PageStack.Model
                Never
                Login.Model
                (Spa.PageStack.Model
                    Never
                    Profile.Model
                    (Spa.PageStack.Model
                        Never
                        Settings.Model
                        (Spa.PageStack.Model
                            Never
                            Home.Model
                            (Spa.PageStack.Model Never () ())
                        )
                    )
                )
            )
        )


type alias StackMsg =
    Spa.PageStack.Msg Route StackCurrentMsg StackPreviousMsg


type alias StackCurrentMsg =
    Editor.Msg


type alias StackPreviousMsg =
    Spa.PageStack.Msg
        Route
        Article.Msg
        (Spa.PageStack.Msg
            Route
            Register.Msg
            (Spa.PageStack.Msg
                Route
                Login.Msg
                (Spa.PageStack.Msg
                    Route
                    Profile.Msg
                    (Spa.PageStack.Msg
                        Route
                        Settings.Msg
                        (Spa.PageStack.Msg Route Home.Msg (Spa.PageStack.Msg Route () ()))
                    )
                )
            )
        )


type alias Stack =
    Spa.PageStack.Stack Never Session Session.Msg Route (View StackMsg) StackCurrentModel StackPreviousModel StackCurrentMsg StackPreviousMsg


stack : Stack
stack =
    Spa.PageStack.setup { defaultView = View.default }
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchHome (Home.page >> Ok)
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchSettings (Settings.page >> Ok)
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchProfile (Profile.page >> Ok)
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchLogin (Login.page >> Ok)
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchRegister (Register.page >> Ok)
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchArticle (Article.page >> Ok)
        |> Spa.PageStack.add ( View.map, View.map ) Route.matchEditor (Editor.page >> Ok)



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view viewer page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view viewer Page.Other Blank.view

        NotFound _ ->
            Page.view viewer Page.Other NotFound.view

        Stack session stackmodel ->
            let
                page =
                    stack.view session stackmodel
            in
            viewPage page.page StackMsg { title = page.title, content = page.content }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotSession Session
    | StackMsg StackMsg
    | SessionMsg Session.Msg
    | Noop


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Stack session _ ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just route ->
            let
                ( newStack, effect ) =
                    case model of
                        Stack _ stackModel ->
                            stack.update session (Spa.PageStack.routeChange route) stackModel

                        _ ->
                            stack.init session route
            in
            ( Stack session newStack, Effect.toCmd ( always Noop, StackMsg ) effect )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( StackMsg stackMsg, Stack session stackModel ) ->
            stack.update session stackMsg stackModel
                |> Tuple.mapFirst (Stack session)
                |> Tuple.mapSecond (Effect.toCmd ( always Noop, StackMsg ))

        ( SessionMsg sessionMsg, Stack session stackModel ) ->
            let
                ( newSession, cmd ) =
                    Session.update sessionMsg session
            in
            ( Stack newSession stackModel, Cmd.map SessionMsg cmd )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Stack session stackmodel ->
            Sub.batch
                [ Sub.map StackMsg (stack.subscriptions session stackmodel)
                , Sub.map SessionMsg (Session.subscriptions session)
                ]



-- MAIN


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
