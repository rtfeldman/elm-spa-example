module Request.Helpers exposing (apiUrl)


apiUrl : String -> String
apiUrl str =
    "https://conduit.productionready.io/api" ++ str
