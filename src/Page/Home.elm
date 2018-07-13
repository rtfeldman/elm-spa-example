module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Article
import Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import Article.Tag as Tag exposing (Tag)
import AuthToken exposing (AuthToken)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Views.Article.Feed as Feed
import Views.Page as Page



-- MODEL --


type alias Model =
    { tags : List Tag
    , feed : Feed.Model
    }


init : Maybe AuthToken -> Task PageLoadError Model
init maybeToken =
    let
        feedSources =
            case maybeToken of
                Just _ ->
                    FeedSources.fromLists GlobalFeed []

                Nothing ->
                    FeedSources.fromLists YourFeed [ GlobalFeed ]

        loadTags =
            Tag.list
                |> Http.toTask

        loadSources =
            Feed.init maybeToken feedSources

        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
    Task.map2 Model loadTags loadSources
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Conduit"
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] (viewFeed (Session.timeZone session) model.feed)
                    , div [ class "col-md-3" ]
                        [ div [ class "sidebar" ]
                            [ p [] [ text "Popular Tags" ]
                            , viewTags model.tags
                            ]
                        ]
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


viewFeed : Time.Zone -> Feed.Model -> List (Html Msg)
viewFeed timeZone feed =
    div [ class "feed-toggle" ]
        [ Feed.viewFeedSources feed |> Html.map FeedMsg ]
        :: (Feed.viewArticles timeZone feed |> List.map (Html.map FeedMsg))


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        , onClick (SelectTag tagName)
        ]
        [ text (Tag.toString tagName) ]



-- UPDATE --


type Msg
    = FeedMsg Feed.Msg
    | SelectTag Tag


update : Maybe AuthToken -> Msg -> Model -> ( Model, Cmd Msg )
update maybeToken msg model =
    case msg of
        FeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update maybeToken subMsg model.feed
            in
            ( { model | feed = newFeed }, Cmd.map FeedMsg subCmd )

        SelectTag tagName ->
            let
                subCmd =
                    Feed.selectTag maybeToken tagName
            in
            ( model, Cmd.map FeedMsg subCmd )
