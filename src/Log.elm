module Log exposing (error)

{-| This is a placeholder API for how we might do logging through
some service like <http://rollbar.com> (which is what we use at work).

Whenever you see Log.error used in this code base, it means
"Something unexpected happened. This is where we would log an
error to our server with some diagnostic info so we could investigate
what happened later."

(Since this is outside the scope of the RealWorld spec, and is only
a placeholder anyway, I didn't bother making this function accept actual
diagnostic info, authentication tokens, etc.)

-}


error : Cmd msg
error =
    Cmd.none
