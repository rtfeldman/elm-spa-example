module Views.Article.Feed.Source exposing (Source, authorFeed, favoritedFeed, globalFeed, tagFeed, yourFeed)

import Data.Article as Article exposing (Tag)
import Data.Article.Tag as Tag exposing (Tag)
import Data.User.Username as Username exposing (Username)


-- FEEDSOURCE --


type Source
    = YourFeed
    | GlobalFeed
    | TagFeed Tag
    | FavoritedFeed Username
    | AuthorFeed Username


<<<<<<< HEAD
eq : FeedSource -> FeedSource -> Bool
eq a b =
    case ( a, b ) of
        ( YourFeed, YourFeed ) ->
            True

        ( YourFeed, _ ) ->
            True

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

        ( FavoritedFeed usernameA, FavoritedFeed usernameB ) ->
            Username.eq usernameA usernameB

        ( FavoritedFeed _, _ ) ->
            False


=======
>>>>>>> 0971909... fixup! Move Feed.Source into its own module
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
