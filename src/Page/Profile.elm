module Page.Profile exposing (Model, Msg, page)

{-| An Author's profile.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar exposing (Avatar)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Spa.Page
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)
import View exposing (View)
import Viewer exposing (Viewer)


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
    , errors : List String
    , feedTab : FeedTab
    , feedPage : Int
    , username : Username

    -- Loaded independently from server
    , author : Status Author
    , feed : Status Feed.Model
    }


type FeedTab
    = MyArticles
    | FavoritedArticles


type Status a
    = Loading Username
    | LoadingSlowly Username
    | Loaded a
    | Failed Username


init : Session -> Username -> ( Model, Effect Session.Msg Msg )
init session username =
    let
        maybeCred =
            Session.cred session
    in
    ( { timeZone = Time.utc
      , errors = []
      , feedTab = defaultFeedTab
      , feedPage = 1
      , username = username
      , author = Loading username
      , feed = Loading username
      }
    , Effect.batch
        [ Author.fetch username maybeCred
            |> Http.toTask
            |> Task.mapError (Tuple.pair username)
            |> Effect.attempt CompletedAuthorLoad
        , fetchFeed session defaultFeedTab username 1
        , Effect.perform GotTimeZone Time.here
        , Effect.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


currentUsername : Model -> Username
currentUsername model =
    case model.author of
        Loading username ->
            username

        LoadingSlowly username ->
            username

        Loaded author ->
            Author.username author

        Failed username ->
            username


defaultFeedTab : FeedTab
defaultFeedTab =
    MyArticles



-- HTTP


fetchFeed : Session -> FeedTab -> Username -> Int -> Effect Session.Msg Msg
fetchFeed session feedTabs username feedPage =
    let
        maybeCred =
            Session.cred session

        firstParam =
            case feedTabs of
                MyArticles ->
                    Url.Builder.string "author" (Username.toString username)

                FavoritedArticles ->
                    Url.Builder.string "favorited" (Username.toString username)

        params =
            firstParam :: PaginatedList.params { page = feedPage, resultsPerPage = articlesPerPage }

        expect =
            Feed.decoder maybeCred articlesPerPage
    in
    Api.get (Endpoint.articles params) maybeCred expect
        |> Http.toTask
        |> Task.map (Feed.init session)
        |> Task.mapError (Tuple.pair username)
        |> Effect.attempt CompletedFeedLoad


articlesPerPage : Int
articlesPerPage =
    5



-- VIEW


view : Session -> Model -> View Msg
view session model =
    let
        title =
            case model.author of
                Loaded (IsViewer _ _) ->
                    myProfileTitle

                Loaded ((IsFollowing followedAuthor) as author) ->
                    titleForOther (Author.username author)

                Loaded ((IsNotFollowing unfollowedAuthor) as author) ->
                    titleForOther (Author.username author)

                Loading username ->
                    titleForMe (Session.cred session) username

                LoadingSlowly username ->
                    titleForMe (Session.cred session) username

                Failed username ->
                    titleForMe (Session.cred session) username
    in
    { title = title
    , page = Page.Profile model.username
    , content =
        case model.author of
            Loaded author ->
                let
                    profile =
                        Author.profile author

                    username =
                        Author.username author

                    followButton =
                        case Session.cred session of
                            Just cred ->
                                case author of
                                    IsViewer _ _ ->
                                        -- We can't follow ourselves!
                                        text ""

                                    IsFollowing followedAuthor ->
                                        Author.unfollowButton ClickedUnfollow cred followedAuthor

                                    IsNotFollowing unfollowedAuthor ->
                                        Author.followButton ClickedFollow cred unfollowedAuthor

                            Nothing ->
                                -- We can't follow if we're logged out
                                text ""
                in
                div [ class "profile-page" ]
                    [ View.viewErrors ClickedDismissErrors model.errors
                    , div [ class "user-info" ]
                        [ div [ class "container" ]
                            [ div [ class "row" ]
                                [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                                    [ img [ class "user-img", Avatar.src (Profile.avatar profile) ] []
                                    , h4 [] [ Username.toHtml username ]
                                    , p [] [ text (Maybe.withDefault "" (Profile.bio profile)) ]
                                    , followButton
                                    ]
                                ]
                            ]
                        ]
                    , case model.feed of
                        Loaded feed ->
                            div [ class "container" ]
                                [ div [ class "row" ]
                                    [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                                        [ div [ class "articles-toggle" ] <|
                                            List.concat
                                                [ [ viewTabs model.feedTab ]
                                                , Feed.viewArticles model.timeZone feed
                                                    |> List.map (Html.map GotFeedMsg)
                                                , [ Feed.viewPagination ClickedFeedPage model.feedPage feed ]
                                                ]
                                        ]
                                    ]
                                ]

                        Loading _ ->
                            text ""

                        LoadingSlowly _ ->
                            Loading.icon

                        Failed _ ->
                            Loading.error "feed"
                    ]

            Loading _ ->
                text ""

            LoadingSlowly _ ->
                Loading.icon

            Failed _ ->
                Loading.error "profile"
    }



-- PAGE TITLE


titleForOther : Username -> String
titleForOther otherUsername =
    "Profile — " ++ Username.toString otherUsername


titleForMe : Maybe Cred -> Username -> String
titleForMe maybeCred username =
    case maybeCred of
        Just cred ->
            if username == Api.username cred then
                myProfileTitle

            else
                defaultTitle

        Nothing ->
            defaultTitle


myProfileTitle : String
myProfileTitle =
    "My Profile"


defaultTitle : String
defaultTitle =
    "Profile"



-- TABS


viewTabs : FeedTab -> Html Msg
viewTabs tab =
    case tab of
        MyArticles ->
            Feed.viewTabs [] myArticles [ favoritedArticles ]

        FavoritedArticles ->
            Feed.viewTabs [ myArticles ] favoritedArticles []


myArticles : ( String, Msg )
myArticles =
    ( "My Articles", ClickedTab MyArticles )


favoritedArticles : ( String, Msg )
favoritedArticles =
    ( "Favorited Articles", ClickedTab FavoritedArticles )



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | CompletedFollowChange (Result Http.Error Author)
    | CompletedAuthorLoad (Result ( Username, Http.Error ) Author)
    | CompletedFeedLoad (Result ( Username, Http.Error ) Feed.Model)
    | GotTimeZone Time.Zone
    | GotFeedMsg Feed.Msg
    | PassedSlowLoadThreshold


update : Session -> Msg -> Model -> ( Model, Effect Session.Msg Msg )
update session msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Effect.none )

        ClickedUnfollow cred followedAuthor ->
            ( model
            , Author.requestUnfollow followedAuthor cred
                |> Http.send CompletedFollowChange
                |> Effect.fromCmd
            )

        ClickedFollow cred unfollowedAuthor ->
            ( model
            , Author.requestFollow unfollowedAuthor cred
                |> Http.send CompletedFollowChange
                |> Effect.fromCmd
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed session tab (currentUsername model) 1
            )

        ClickedFeedPage feedPage ->
            ( { model | feedPage = feedPage }
            , fetchFeed session model.feedTab (currentUsername model) feedPage
            )

        CompletedFollowChange (Ok newAuthor) ->
            ( { model | author = Loaded newAuthor }
            , Effect.none
            )

        CompletedFollowChange (Err error) ->
            ( model
            , Log.error |> Effect.fromCmd
            )

        CompletedAuthorLoad (Ok author) ->
            ( { model | author = Loaded author }, Effect.none )

        CompletedAuthorLoad (Err ( username, err )) ->
            ( { model | author = Failed username }
            , Log.error |> Effect.fromCmd
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Loaded feed }
            , Effect.none
            )

        CompletedFeedLoad (Err ( username, err )) ->
            ( { model | feed = Failed username }
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
                    , Cmd.map GotFeedMsg subCmd
                        |> Effect.fromCmd
                    )

                Loading _ ->
                    ( model, Log.error |> Effect.fromCmd )

                LoadingSlowly _ ->
                    ( model, Log.error |> Effect.fromCmd )

                Failed _ ->
                    ( model, Log.error |> Effect.fromCmd )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Effect.none )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                feed =
                    case model.feed of
                        Loading username ->
                            LoadingSlowly username

                        other ->
                            other
            in
            ( { model | feed = feed }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
