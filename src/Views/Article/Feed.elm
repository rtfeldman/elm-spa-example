module Views.Article.Feed exposing (Model, Msg, init, selectTag, update, viewArticles, viewFeedSources)

{-| NOTE: This module has its own Model, view, and update. This is not normal!
If you find yourself doing this often, please watch <https://www.youtube.com/watch?v=DoA4Txr4GUs>

This is the reusable Article Feed that appears on both the Home page as well as
on the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}

import Browser
import Data.Article as Article exposing (Article)
import Data.Article.Feed exposing (Feed)
import Data.Article.Slug as Slug
import Data.Article.Tag as Tag exposing (Tag)
import Data.AuthToken exposing (AuthToken)
import Data.Session exposing (Session)
import Data.User
import Data.User.Username as Username exposing (Username)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Request.Article
import SelectList exposing (Position(..), SelectList)
import Task exposing (Task)
import Util exposing (onClickStopPropagation, pair, viewIf)
import Views.Article
import Views.Article.Feed.Source as FeedSource exposing (FeedSource(..), tagFeed)
import Views.Errors as Errors
import Views.Page exposing (bodyId)
import Views.Spinner exposing (spinner)



-- MODEL --


type Model
    = Model InternalModel


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this file.
-}
type alias InternalModel =
    { errors : List String
    , feed : Feed
    , feedSources : SelectList FeedSource
    , activePage : Int
    , isLoading : Bool
    }


init : Session -> SelectList FeedSource -> Task Http.Error Model
init session feedSources =
    let
        source =
            SelectList.selected feedSources

        toModel ( activePage, feed ) =
            Model
                { errors = []
                , activePage = activePage
                , feed = feed
                , feedSources = feedSources
                , isLoading = False
                }
    in
    source
        |> fetch (Maybe.map .token session.user) 1
        |> Task.map toModel



-- VIEW --


viewArticles : Model -> List (Html Msg)
viewArticles (Model { activePage, feed, feedSources }) =
    List.append
        (List.map (Views.Article.view ToggleFavorite) feed.articles)
        [ pagination activePage feed (SelectList.selected feedSources) ]


viewFeedSources : Model -> Html Msg
viewFeedSources (Model { feedSources, isLoading, errors }) =
    ul [ class "nav nav-pills outline-active" ] <|
        List.append
            (SelectList.toList (SelectList.mapBy viewFeedSource feedSources))
            [ Errors.view DismissErrors errors, viewIf isLoading spinner ]


viewFeedSource : Position -> FeedSource -> Html Msg
viewFeedSource position source =
    let
        classname =
            case position of
                Selected ->
                    "nav-link active"

                _ ->
                    "nav-link"
    in
    li [ class "nav-item" ]
        [ a
            [ class classname
            , href "javascript:void(0);"
            , onClick (SelectFeedSource source)
            ]
            [ text (sourceName source) ]
        ]


selectTag : Maybe AuthToken -> Tag -> Cmd Msg
selectTag maybeAuthToken tagName =
    let
        source =
            tagFeed tagName
    in
    source
        |> fetch maybeAuthToken 1
        |> Task.attempt (FeedLoadCompleted source)


sourceName : FeedSource -> String
sourceName source =
    case source of
        YourFeed ->
            "Your Feed"

        GlobalFeed ->
            "Global Feed"

        TagFeed tagName ->
            "#" ++ Tag.toString tagName

        FavoritedFeed username ->
            "Favorited Articles"

        AuthorFeed username ->
            "My Articles"


limit : FeedSource -> Int
limit feedSource =
    case feedSource of
        YourFeed ->
            10

        GlobalFeed ->
            10

        TagFeed tagName ->
            10

        FavoritedFeed username ->
            5

        AuthorFeed username ->
            5


pagination : Int -> Feed -> FeedSource -> Html Msg
pagination activePage feed feedSource =
    let
        articlesPerPage =
            limit feedSource

        totalPages =
            ceiling (toFloat feed.articlesCount / toFloat articlesPerPage)
    in
    if totalPages > 1 then
        List.range 1 totalPages
            |> List.map (\page -> pageLink page (page == activePage))
            |> ul [ class "pagination" ]

    else
        Html.text ""


pageLink : Int -> Bool -> Html Msg
pageLink page isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , href "javascript:void(0);"
            , onClick (SelectPage page)
            ]
            [ text (String.fromInt page) ]
        ]



-- UPDATE --


type Msg
    = DismissErrors
    | SelectFeedSource FeedSource
    | FeedLoadCompleted FeedSource (Result Http.Error ( Int, Feed ))
    | ToggleFavorite (Article ())
    | FavoriteCompleted (Result Http.Error (Article ()))
    | SelectPage Int


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg (Model internalModel) =
    updateInternal session msg internalModel
        |> Tuple.mapFirst Model


updateInternal : Session -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal session msg model =
    case msg of
        DismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        SelectFeedSource source ->
            source
                |> fetch (Maybe.map .token session.user) 1
                |> Task.attempt (FeedLoadCompleted source)
                |> pair { model | isLoading = True }

        FeedLoadCompleted source (Ok ( activePage, feed )) ->
            ( { model
                | feed = feed
                , feedSources = selectFeedSource source model.feedSources
                , activePage = activePage
                , isLoading = False
              }
            , Cmd.none
            )

        FeedLoadCompleted _ (Err error) ->
            ( { model
                | errors = List.append model.errors [ "Server error while trying to load feed" ]
                , isLoading = False
              }
            , Cmd.none
            )

        ToggleFavorite article ->
            case session.user of
                Nothing ->
                    ( { model | errors = List.append model.errors [ "You are currently signed out. You must sign in to favorite articles." ] }
                    , Cmd.none
                    )

                Just user ->
                    Request.Article.toggleFavorite article user.token
                        |> Http.send FavoriteCompleted
                        |> pair model

        FavoriteCompleted (Ok article) ->
            let
                feed =
                    model.feed

                newFeed =
                    { feed | articles = List.map (replaceArticle article) feed.articles }
            in
            ( { model | feed = newFeed }, Cmd.none )

        FavoriteCompleted (Err error) ->
            ( { model | errors = List.append model.errors [ "Server error while trying to favorite article." ] }
            , Cmd.none
            )

        SelectPage page ->
            let
                source =
                    SelectList.selected model.feedSources
            in
            source
                |> fetch (Maybe.map .token session.user) page
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt (FeedLoadCompleted source)
                |> pair model


scrollToTop : Task x ()
scrollToTop =
    Browser.setScrollTop bodyId 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())


fetch : Maybe AuthToken -> Int -> FeedSource -> Task Http.Error ( Int, Feed )
fetch token page feedSource =
    let
        defaultListConfig =
            Request.Article.defaultListConfig

        articlesPerPage =
            limit feedSource

        offset =
            (page - 1) * articlesPerPage

        listConfig =
            { defaultListConfig | offset = offset, limit = articlesPerPage }

        task =
            case feedSource of
                YourFeed ->
                    let
                        defaultFeedConfig =
                            Request.Article.defaultFeedConfig

                        feedConfig =
                            { defaultFeedConfig | offset = offset, limit = articlesPerPage }
                    in
                    token
                        |> Maybe.map (Request.Article.feed feedConfig >> Http.toTask)
                        |> Maybe.withDefault (Task.fail (Http.BadUrl "You need to be signed in to view your feed."))

                GlobalFeed ->
                    Request.Article.list listConfig token
                        |> Http.toTask

                TagFeed tagName ->
                    Request.Article.list { listConfig | tag = Just tagName } token
                        |> Http.toTask

                FavoritedFeed username ->
                    Request.Article.list { listConfig | favorited = Just username } token
                        |> Http.toTask

                AuthorFeed username ->
                    Request.Article.list { listConfig | author = Just username } token
                        |> Http.toTask
    in
    task
        |> Task.map (\feed -> ( page, feed ))


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if Slug.eq newArticle.slug oldArticle.slug then
        newArticle

    else
        oldArticle


selectFeedSource : FeedSource -> SelectList FeedSource -> SelectList FeedSource
selectFeedSource source sources =
    let
        withoutTags =
            sources
                |> SelectList.toList
                |> List.filter (not << isTagFeed)

        newSources =
            case source of
                YourFeed ->
                    withoutTags

                GlobalFeed ->
                    withoutTags

                FavoritedFeed _ ->
                    withoutTags

                AuthorFeed _ ->
                    withoutTags

                TagFeed _ ->
                    List.append withoutTags [ source ]
    in
    case newSources of
        [] ->
            -- This should never happen. If we had a logging service set up,
            -- we would definitely want to report if it somehow did happen!
            sources

        first :: rest ->
            SelectList.fromLists [] first rest
                |> SelectList.select (FeedSource.eq source)


isTagFeed : FeedSource -> Bool
isTagFeed source =
    case source of
        TagFeed _ ->
            True

        _ ->
            False
