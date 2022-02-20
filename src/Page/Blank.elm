module Page.Blank exposing (view)


view : { title : String, content : Html msg }
view =
    { title = ""
    , content = Html.text ""
    }
