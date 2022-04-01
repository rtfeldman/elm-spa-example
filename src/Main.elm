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
import Page.Logout as Logout
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import Spa
import Spa.PageStack
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)
import View exposing (View)
import Viewer exposing (Viewer)



-- Elm SPA pages


type alias StackModel =
    Spa.PageStack.Model Never StackCurrentModel StackPreviousModel


type alias StackCurrentModel =
    Logout.Model


type alias StackPreviousModel =
    Spa.PageStack.Model
        Never
        Editor.Model
        (Spa.PageStack.Model
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
        )


type alias StackMsg =
    Spa.PageStack.Msg Route StackCurrentMsg StackPreviousMsg


type alias StackCurrentMsg =
    Logout.Msg


type alias StackPreviousMsg =
    Spa.PageStack.Msg
        Route
        Editor.Msg
        (Spa.PageStack.Msg
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
        )


type alias Stack =
    Spa.PageStack.Stack Never Session Session.Msg Route (View StackMsg) StackCurrentModel StackPreviousModel StackCurrentMsg StackPreviousMsg


type alias Msg =
    Spa.Msg Session.Msg StackMsg



-- VIEW


toDocument : Session -> View Msg -> Browser.Document Msg
toDocument session view =
    View.view (Session.viewer session) view



-- MAIN


main =
    Spa.init
        { defaultView = View.default
        , extractIdentity = Session.viewer
        }
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchHome Home.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchSettings Settings.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchProfile Profile.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchLogin Login.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchRegister Register.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchArticle Article.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchEditor Editor.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchLogout Logout.page
        |> Spa.application View.map
            { init = Session.init
            , subscriptions = Session.subscriptions
            , update = Session.update
            , toRoute = Route.fromUrl >> Maybe.withDefault Route.Home
            , toDocument = toDocument
            , protectPage = always (Route.toString Route.Home)
            }
        |> Spa.onUrlRequest Session.ClickedLink
        |> Browser.application
