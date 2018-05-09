module Views.Article.Feed.Source exposing (FeedSource(..), authorFeed, favoritedFeed, globalFeed, tagFeed, yourFeed)

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
