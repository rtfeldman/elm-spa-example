module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| An Author's profile.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar exposing (Avatar)
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
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String
    , feedTab : FeedTab
    , feedPage : Int

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


init : Session -> Username -> ( Model, Cmd Msg )
init session username =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , timeZone = Time.utc
      , errors = []
      , feedTab = defaultFeedTab
      , feedPage = 1
      , author = Loading username
      , feed = Loading username
      }
    , Cmd.batch
        [ Author.fetch username maybeCred
            |> Http.toTask
            |> Task.mapError (Tuple.pair username)
            |> Task.attempt CompletedAuthorLoad
        , fetchFeed session defaultFeedTab username 1
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
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


fetchFeed : Session -> FeedTab -> Username -> Int -> Cmd Msg
fetchFeed session feedTabs username page =
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
            firstParam :: PaginatedList.params { page = page, resultsPerPage = articlesPerPage }

        expect =
            Feed.decoder maybeCred articlesPerPage
    in
    Api.get (Endpoint.articles params) maybeCred expect
        |> Http.toTask
        |> Task.map (Feed.init session)
        |> Task.mapError (Tuple.pair username)
        |> Task.attempt CompletedFeedLoad


articlesPerPage : Int
articlesPerPage =
    5



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
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
                    titleForMe (Session.cred model.session) username

                LoadingSlowly username ->
                    titleForMe (Session.cred model.session) username

                Failed username ->
                    titleForMe (Session.cred model.session) username
    in
    { title = title
    , content =
        case model.author of
            Loaded author ->
                let
                    profile =
                        Author.profile author

                    username =
                        Author.username author

                    followButton =
                        case Session.cred model.session of
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
                    [ Page.viewErrors ClickedDismissErrors model.errors
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
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedUnfollow cred followedAuthor ->
            ( model
            , Author.requestUnfollow followedAuthor cred
                |> Http.send CompletedFollowChange
            )

        ClickedFollow cred unfollowedAuthor ->
            ( model
            , Author.requestFollow unfollowedAuthor cred
                |> Http.send CompletedFollowChange
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed model.session tab (currentUsername model) 1
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , fetchFeed model.session model.feedTab (currentUsername model) page
            )

        CompletedFollowChange (Ok newAuthor) ->
            ( { model | author = Loaded newAuthor }
            , Cmd.none
            )

        CompletedFollowChange (Err error) ->
            ( model
            , Log.error
            )

        CompletedAuthorLoad (Ok author) ->
            ( { model | author = Loaded author }, Cmd.none )

        CompletedAuthorLoad (Err ( username, err )) ->
            ( { model | author = Failed username }
            , Log.error
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Loaded feed }
            , Cmd.none
            )

        CompletedFeedLoad (Err ( username, err )) ->
            ( { model | feed = Failed username }
            , Log.error
            )

        GotFeedMsg subMsg ->
            case model.feed of
                Loaded feed ->
                    let
                        ( newFeed, subCmd ) =
                            Feed.update (Session.cred model.session) subMsg feed
                    in
                    ( { model | feed = Loaded newFeed }
                    , Cmd.map GotFeedMsg subCmd
                    )

                Loading _ ->
                    ( model, Log.error )

                LoadingSlowly _ ->
                    ( model, Log.error )

                Failed _ ->
                    ( model, Log.error )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

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
            ( { model | feed = feed }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
