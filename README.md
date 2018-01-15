# ![RealWorld Example App](https://cloud.githubusercontent.com/assets/556934/25448178/3e7dc5c0-2a7d-11e7-8069-06da5169dae6.png)

👉 I gave a talk, [**Scaling Elm Apps**](https://www.youtube.com/watch?v=DoA4Txr4GUs),
to explain the principles I used to build this. I highly recommend [watching it](https://www.youtube.com/watch?v=DoA4Txr4GUs)!

> [Elm](http://elm-lang.org) codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld-example-apps) spec and API.


### [Demo](https://rtfeldman.github.io/elm-spa-example)&nbsp;&nbsp;&nbsp;&nbsp;[Demo with Time-traveling Debugger](https://rtfeldman.github.io/elm-spa-example-with-debug)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application built with [Elm](http://elm-lang.org) including CRUD operations, authentication, routing, pagination, and more.

For more information on how this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# How it works

Check out [the full writeup](https://dev.to/rtfeldman/tour-of-an-open-source-elm-spa)!

# Getting started

If you don't already have `elm` and `elm-live`:

> npm install -g elm elm-live

Then, to build everything:

> elm-live --output=elm.js src/Main.elm --pushstate --open --debug

(Leave off the `--debug` if you don't want the time-traveling debugger.)
