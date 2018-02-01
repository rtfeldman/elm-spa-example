module Article.Feed exposing (Model, Msg, decoder, init, update, viewArticles, viewPagination, viewTabs)

import Api exposing (Cred)
import Article exposing (Article, Preview)
import Article.Slug as ArticleSlug exposing (Slug)
import Article.Tag as Tag exposing (Tag)
import Author
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import PaginatedList exposing (PaginatedList)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Url exposing (Url)
import Username exposing (Username)


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



-- MODEL


type Model
    = Model Internals


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this module.
-}
type alias Internals =
    { session : Session
    , errors : List String
    , articles : PaginatedList (Article Preview)
    , isLoading : Bool
    }


init : Session -> PaginatedList (Article Preview) -> Model
init session articles =
    Model
        { session = session
        , errors = []
        , articles = articles
        , isLoading = False
        }



-- VIEW


viewArticles : Time.Zone -> Model -> List (Html Msg)
viewArticles timeZone (Model { articles, session, errors }) =
    let
        maybeCred =
            Session.cred session

        articlesHtml =
            PaginatedList.values articles
                |> List.map (viewPreview maybeCred timeZone)
    in
    Page.viewErrors ClickedDismissErrors errors :: articlesHtml


viewPreview : Maybe Cred -> Time.Zone -> Article Preview -> Html Msg
viewPreview maybeCred timeZone article =
    let
        slug =
            Article.slug article

        { title, description, createdAt } =
            Article.metadata article

        author =
            Article.author article

        profile =
            Author.profile author

        username =
            Author.username author

        faveButton =
            case maybeCred of
                Just cred ->
                    let
                        { favoritesCount, favorited } =
                            Article.metadata article

                        viewButton =
                            if favorited then
                                Article.unfavoriteButton cred (ClickedUnfavorite cred slug)

                            else
                                Article.favoriteButton cred (ClickedFavorite cred slug)
                    in
                    viewButton [ class "pull-xs-right" ]
                        [ text (" " ++ String.fromInt favoritesCount) ]

                Nothing ->
                    text ""
    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile username) ]
                [ img [ Avatar.src (Profile.avatar profile) ] [] ]
            , div [ class "info" ]
                [ Author.view username
                , Timestamp.view timeZone createdAt
                ]
            , faveButton
            ]
        , a [ class "preview-link", Route.href (Route.Article (Article.slug article)) ]
            [ h1 [] [ text title ]
            , p [] [ text description ]
            , span [] [ text "Read more..." ]
            , ul [ class "tag-list" ]
                (List.map viewTag (Article.metadata article).tags)
            ]
        ]


viewTabs :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
viewTabs before selected after =
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewTab []) before
            , [ viewTab [ class "active" ] selected ]
            , List.map (viewTab []) after
            ]


viewTab : List (Attribute msg) -> ( String, msg ) -> Html msg
viewTab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a (class "nav-link" :: onClick msg :: href "" :: attrs)
            [ text name ]
        ]


viewPagination : (Int -> msg) -> Int -> Model -> Html msg
viewPagination toMsg page (Model feed) =
    let
        viewPageLink currentPage =
            pageLink toMsg currentPage (currentPage == page)

        totalPages =
            PaginatedList.total feed.articles
    in
    if totalPages > 1 then
        List.range 1 totalPages
            |> List.map viewPageLink
            |> ul [ class "pagination" ]

    else
        Html.text ""


pageLink : (Int -> msg) -> Int -> Bool -> Html msg
pageLink toMsg targetPage isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , onClick (toMsg targetPage)

            -- The RealWorld CSS requires an href to work properly.
            , href ""
            ]
            [ text (String.fromInt targetPage) ]
        ]


viewTag : String -> Html msg
viewTag tagName =
    li [ class "tag-default tag-pill tag-outline" ] [ text tagName ]



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedFavorite Cred Slug
    | ClickedUnfavorite Cred Slug
    | CompletedFavorite (Result Http.Error (Article Preview))


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )

        ClickedFavorite cred slug ->
            fave Article.favorite cred slug model

        ClickedUnfavorite cred slug ->
            fave Article.unfavorite cred slug model

        CompletedFavorite (Ok article) ->
            ( Model { model | articles = PaginatedList.map (replaceArticle article) model.articles }
            , Cmd.none
            )

        CompletedFavorite (Err error) ->
            ( Model { model | errors = Api.addServerError model.errors }
            , Cmd.none
            )


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if Article.slug newArticle == Article.slug oldArticle then
        newArticle

    else
        oldArticle



-- SERIALIZATION


decoder : Maybe Cred -> Int -> Decoder (PaginatedList (Article Preview))
decoder maybeCred resultsPerPage =
    Decode.succeed PaginatedList.fromList
        |> required "articlesCount" (pageCountDecoder resultsPerPage)
        |> required "articles" (Decode.list (Article.previewDecoder maybeCred))


pageCountDecoder : Int -> Decoder Int
pageCountDecoder resultsPerPage =
    Decode.int
        |> Decode.map (\total -> ceiling (toFloat total / toFloat resultsPerPage))



-- INTERNAL


fave : (Slug -> Cred -> Http.Request (Article Preview)) -> Cred -> Slug -> Internals -> ( Model, Cmd Msg )
fave toRequest cred slug model =
    ( Model model
    , toRequest slug cred
        |> Http.toTask
        |> Task.attempt CompletedFavorite
    )
