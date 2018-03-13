module Date.Local exposing (Local, Months, TimeZones, WeekDays, brazilian, french, international)

{-| A record type to store localized time formatting information.

@docs international, french, brazilian

@docs Local, Months, WeekDays, TimeZones

-}

import Dict exposing (Dict)


{-| A collection of strings and formats for localizing formats.

Time zones and default formats are not implemented,
but included to avoid possible version conflicts in the future.

-}
type alias Local =
    { date :
        { months : Months
        , monthsAbbrev : Months
        , wdays : WeekDays
        , wdaysAbbrev : WeekDays
        , defaultFormat :
            Maybe String

        -- for %x
        }
    , time :
        { am : String
        , pm : String
        , defaultFormat :
            Maybe String

        -- for %X
        }
    , timeZones :
        Maybe TimeZones

    -- for %Z
    , defaultFormat :
        Maybe String

    -- for %c
    }


{-| A record of names for the months of the year.
-}
type alias Months =
    { jan : String
    , feb : String
    , mar : String
    , apr : String
    , may : String
    , jun : String
    , jul : String
    , aug : String
    , sep : String
    , oct : String
    , nov : String
    , dec : String
    }


{-| A record of names for the days of the week.
-}
type alias WeekDays =
    { mon : String
    , tue : String
    , wed : String
    , thu : String
    , fri : String
    , sat : String
    , sun : String
    }


{-| Maps from %z type string (+hhmm or -hhmm) to timezone name or abbreviation.

Not currently implemented.

-}
type alias TimeZones =
    Dict String String


{-| A default set of localizations.
-}
international : Local
international =
    { date =
        { months =
            { jan = "January"
            , feb = "February"
            , mar = "March"
            , apr = "April"
            , may = "May"
            , jun = "June"
            , jul = "July"
            , aug = "August"
            , sep = "September"
            , oct = "October"
            , nov = "November"
            , dec = "December"
            }
        , monthsAbbrev =
            { jan = "Jan"
            , feb = "Feb"
            , mar = "Mar"
            , apr = "Apr"
            , may = "May"
            , jun = "Jun"
            , jul = "Jul"
            , aug = "Aug"
            , sep = "Sep"
            , oct = "Oct"
            , nov = "Nov"
            , dec = "Dec"
            }
        , wdays =
            { mon = "Monday"
            , tue = "Tuesday"
            , wed = "Wednesday"
            , thu = "Thursday"
            , fri = "Friday"
            , sat = "Saturday"
            , sun = "Sunday"
            }
        , wdaysAbbrev =
            { mon = "Mon"
            , tue = "Tue"
            , wed = "Wed"
            , thu = "Thu"
            , fri = "Fri"
            , sat = "Sat"
            , sun = "Sun"
            }
        , defaultFormat = Nothing
        }
    , time =
        { am = "am"
        , pm = "pm"
        , defaultFormat = Nothing
        }
    , timeZones = Nothing
    , defaultFormat = Nothing
    }


{-| French set of localizations.
-}
french : Local
french =
    { date =
        { months =
            { jan = "Janvier"
            , feb = "Février"
            , mar = "Mars"
            , apr = "Avril"
            , may = "Mai"
            , jun = "Juin"
            , jul = "Juillet"
            , aug = "Août"
            , sep = "Septembre"
            , oct = "Octobre"
            , nov = "Novembre"
            , dec = "Décembre"
            }
        , monthsAbbrev =
            { jan = "Jan"
            , feb = "Fév"
            , mar = "Mar"
            , apr = "Avr"
            , may = "Mai"
            , jun = "Jui"
            , jul = "Jul"
            , aug = "Aoû"
            , sep = "Sep"
            , oct = "Oct"
            , nov = "Nov"
            , dec = "Déc"
            }
        , wdays =
            { mon = "Lundi"
            , tue = "Mardi"
            , wed = "Mercredi"
            , thu = "Jeudi"
            , fri = "Vendredi"
            , sat = "Samedi"
            , sun = "Dimanche"
            }
        , wdaysAbbrev =
            { mon = "Lun"
            , tue = "Mar"
            , wed = "Mer"
            , thu = "Jeu"
            , fri = "Ven"
            , sat = "Sam"
            , sun = "Dim"
            }
        , defaultFormat = Nothing
        }
    , time =
        { am = "am"
        , pm = "pm"
        , defaultFormat = Nothing
        }
    , timeZones = Nothing
    , defaultFormat = Nothing
    }


{-| Brazilian set of localizations.
-}
brazilian : Local
brazilian =
    { date =
        { months =
            { jan = "Janeiro"
            , feb = "Fevereiro"
            , mar = "Março"
            , apr = "Abril"
            , may = "Maio"
            , jun = "Junho"
            , jul = "Julho"
            , aug = "Agosto"
            , sep = "Setembro"
            , oct = "Outubro"
            , nov = "Novembro"
            , dec = "Dezembro"
            }
        , monthsAbbrev =
            { jan = "Jan"
            , feb = "Fev"
            , mar = "Mar"
            , apr = "Abr"
            , may = "Mai"
            , jun = "Jun"
            , jul = "Jul"
            , aug = "Ago"
            , sep = "Set"
            , oct = "Out"
            , nov = "Nov"
            , dec = "Dez"
            }
        , wdays =
            { mon = "Segunda-feira"
            , tue = "Terça-feira"
            , wed = "Quarta-feira"
            , thu = "Quinta-feira"
            , fri = "Sexta-feira"
            , sat = "Sábado"
            , sun = "Domingo"
            }
        , wdaysAbbrev =
            { mon = "Seg"
            , tue = "Ter"
            , wed = "Qua"
            , thu = "Qui"
            , fri = "Sex"
            , sat = "Sáb"
            , sun = "Dom"
            }
        , defaultFormat = Just "%e de %B de %Y"
        }
    , time =
        { am = "am"
        , pm = "pm"
        , defaultFormat = Just "%k:%M"
        }
    , timeZones = Nothing
    , defaultFormat = Nothing
    }
