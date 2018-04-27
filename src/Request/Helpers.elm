module Request.Helpers exposing (apiUrl)


apiUrl : String -> String
apiUrl str =
    "http://0.0.0.0:3000/api" ++ str
