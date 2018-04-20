module Views.Article.Feed.Source exposing (FeedSource(..), authorFeed, eq, favoritedFeed, globalFeed, tagFeed, yourFeed)

import Data.Article as Article
import Data.Article.Tag as Tag exposing (Tag)
import Data.User.Username as Username exposing (Username)



-- FEEDSOURCE --


type FeedSource
    = YourFeed
    | GlobalFeed
    | TagFeed Tag
    | FavoritedFeed Username
    | AuthorFeed Username


eq : FeedSource -> FeedSource -> Bool
eq a b =
    case ( a, b ) of
        ( YourFeed, YourFeed ) ->
            True

        ( YourFeed, _ ) ->
            False

        ( GlobalFeed, GlobalFeed ) ->
            True

        ( GlobalFeed, _ ) ->
            False

        ( TagFeed tagA, TagFeed tagB ) ->
            Tag.eq tagA tagB

        ( TagFeed _, _ ) ->
            False

        ( FavoritedFeed usernameA, FavoritedFeed usernameB ) ->
            Username.eq usernameA usernameB

        ( FavoritedFeed _, _ ) ->
            False

        ( AuthorFeed usernameA, AuthorFeed usernameB ) ->
            Username.eq usernameA usernameB

        ( AuthorFeed _, _ ) ->
            False


yourFeed : FeedSource
yourFeed =
    YourFeed


globalFeed : FeedSource
globalFeed =
    GlobalFeed


tagFeed : Tag -> FeedSource
tagFeed =
    TagFeed


favoritedFeed : Username -> FeedSource
favoritedFeed =
    FavoritedFeed


authorFeed : Username -> FeedSource
authorFeed =
    AuthorFeed
