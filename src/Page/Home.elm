module Page.Home exposing (Model, Msg, page)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Article.Tag as Tag exposing (Tag)
import Browser.Dom as Dom
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Spa.Page
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)
import View exposing (View)


page session =
    Spa.Page.element
        { init = init session
        , update = update session
        , subscriptions = subscriptions
        , view = view session
        }



-- MODEL


type alias Model =
    { timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int

    -- Loaded independently from server
    , tags : Status (List Tag)
    , feed : Status Feed.Model
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type FeedTab
    = YourFeed Cred
    | GlobalFeed
    | TagFeed Tag


init : Session -> () -> ( Model, Effect Session.Msg Msg )
init session _ =
    let
        feedTab =
            case Session.cred session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed

        loadTags =
            Http.toTask Tag.list
    in
    ( { timeZone = Time.utc
      , feedTab = feedTab
      , feedPage = 1
      , tags = Loading
      , feed = Loading
      }
    , Effect.batch
        [ fetchFeed session feedTab 1
            |> Effect.attempt CompletedFeedLoad
        , Tag.list
            |> Http.send CompletedTagsLoad
            |> Effect.fromCmd
        , Effect.perform GotTimeZone Time.here
        , Effect.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Session -> Model -> View Msg
view session model =
    { title = "Conduit"
    , page = Page.Home
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        case model.feed of
                            Loaded feed ->
                                [ div [ class "feed-toggle" ] <|
                                    List.concat
                                        [ [ viewTabs
                                                (Session.cred session)
                                                model.feedTab
                                          ]
                                        , Feed.viewArticles model.timeZone feed
                                            |> List.map (Html.map GotFeedMsg)
                                        , [ Feed.viewPagination ClickedFeedPage model.feedPage feed ]
                                        ]
                                ]

                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "feed" ]
                    , div [ class "col-md-3" ] <|
                        case model.tags of
                            Loaded tags ->
                                [ div [ class "sidebar" ] <|
                                    [ p [] [ text "Popular Tags" ]
                                    , viewTags tags
                                    ]
                                ]

                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "tags" ]
                    ]
                ]
            ]
    }


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]



-- TABS


viewTabs : Maybe Cred -> FeedTab -> Html Msg
viewTabs maybeCred tab =
    case tab of
        YourFeed cred ->
            Feed.viewTabs [] (yourFeed cred) [ globalFeed ]

        GlobalFeed ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred ]

                        Nothing ->
                            []
            in
            Feed.viewTabs otherTabs globalFeed []

        TagFeed tag ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred, globalFeed ]

                        Nothing ->
                            [ globalFeed ]
            in
            Feed.viewTabs otherTabs (tagFeed tag) []


yourFeed : Cred -> ( String, Msg )
yourFeed cred =
    ( "Your Feed", ClickedTab (YourFeed cred) )


globalFeed : ( String, Msg )
globalFeed =
    ( "Global Feed", ClickedTab GlobalFeed )


tagFeed : Tag -> ( String, Msg )
tagFeed tag =
    ( "#" ++ Tag.toString tag, ClickedTab (TagFeed tag) )



-- TAGS


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , onClick (ClickedTag tagName)

        -- The RealWorld CSS requires an href to work properly.
        , href ""
        ]
        [ text (Tag.toString tagName) ]



-- UPDATE


type Msg
    = ClickedTag Tag
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | CompletedFeedLoad (Result Http.Error Feed.Model)
    | CompletedTagsLoad (Result Http.Error (List Tag))
    | GotTimeZone Time.Zone
    | GotFeedMsg Feed.Msg
    | PassedSlowLoadThreshold


update : Session -> Msg -> Model -> ( Model, Effect Session.Msg Msg )
update session msg model =
    case msg of
        ClickedTag tag ->
            let
                feedTab =
                    TagFeed tag
            in
            ( { model | feedTab = feedTab }
            , fetchFeed session feedTab 1
                |> Effect.attempt CompletedFeedLoad
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed session tab 1
                |> Effect.attempt CompletedFeedLoad
            )

        ClickedFeedPage feedPage ->
            ( { model | feedPage = feedPage }
            , fetchFeed session model.feedTab feedPage
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Effect.attempt CompletedFeedLoad
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Loaded feed }, Effect.none )

        CompletedFeedLoad (Err error) ->
            ( { model | feed = Failed }, Effect.none )

        CompletedTagsLoad (Ok tags) ->
            ( { model | tags = Loaded tags }, Effect.none )

        CompletedTagsLoad (Err error) ->
            ( { model | tags = Failed }
            , Log.error |> Effect.fromCmd
            )

        GotFeedMsg subMsg ->
            case model.feed of
                Loaded feed ->
                    let
                        ( newFeed, subCmd ) =
                            Feed.update (Session.cred session) subMsg feed
                    in
                    ( { model | feed = Loaded newFeed }
                    , Cmd.map GotFeedMsg subCmd |> Effect.fromCmd
                    )

                Loading ->
                    ( model, Log.error |> Effect.fromCmd )

                LoadingSlowly ->
                    ( model, Log.error |> Effect.fromCmd )

                Failed ->
                    ( model, Log.error |> Effect.fromCmd )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Effect.none )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                feed =
                    case model.feed of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                tags =
                    case model.tags of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | feed = feed, tags = tags }, Effect.none )



-- HTTP


fetchFeed : Session -> FeedTab -> Int -> Task Http.Error Feed.Model
fetchFeed session feedTabs feedPage =
    let
        maybeCred =
            Session.cred session

        decoder =
            Feed.decoder maybeCred articlesPerPage

        params =
            PaginatedList.params { page = feedPage, resultsPerPage = articlesPerPage }

        request =
            case feedTabs of
                YourFeed cred ->
                    Api.get (Endpoint.feed params) maybeCred decoder

                GlobalFeed ->
                    Api.get (Endpoint.articles params) maybeCred decoder

                TagFeed tag ->
                    let
                        firstParam =
                            Url.Builder.string "tag" (Tag.toString tag)
                    in
                    Api.get (Endpoint.articles (firstParam :: params)) maybeCred decoder
    in
    Http.toTask request
        |> Task.map (Feed.init session)


articlesPerPage : Int
articlesPerPage =
    10


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
