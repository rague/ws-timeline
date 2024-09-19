module Moment exposing (Duration, Moment(..), add, addDuration, addDurationToPosix, between, changeHour, durationBetween, durationIsZero, durationNotZero, format, fromDuration, greaterThan, intersect, lessOrEqualThan, lessThan, mapDuration, maxPosix, minPosix, startOf, subtractDuration, toDuration, toHoursString)

--
-- import Date exposing (Date, Day(..))
-- import Date.Extra.Config.Config_fr_fr exposing (config)
-- import Date.Extra.Core as Coredire
-- import Date.Extra.Create as Create
-- import Date.Extra.Field as F
-- import Date.Extra.Format as Format
-- import Date.Extra.Period as Period
-- import Date.Extra.TimeUnit as TU
-- import Date.Extra.Utils as Utils

import Date
import Html.Attributes exposing (start)
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as TE


type Duration
    = Duration Int


toDuration : Int -> Duration
toDuration =
    Duration


fromDuration : Duration -> Int
fromDuration (Duration int) =
    int


greaterThan : Posix -> Posix -> Bool
greaterThan a b =
    Time.posixToMillis a > Time.posixToMillis b


lessThan : Posix -> Posix -> Bool
lessThan a b =
    Time.posixToMillis a < Time.posixToMillis b


lessOrEqualThan : Posix -> Posix -> Bool
lessOrEqualThan a b =
    Time.posixToMillis a <= Time.posixToMillis b


durationIsZero : Duration -> Bool
durationIsZero (Duration dur) =
    dur == 0


durationNotZero : Duration -> Bool
durationNotZero (Duration dur) =
    dur /= 0


mapDuration : (Int -> Int) -> Duration -> Duration
mapDuration func (Duration d) =
    Duration (func d)


addDuration : Duration -> Duration -> Duration
addDuration (Duration a) (Duration b) =
    Duration (a + b)


between : Posix -> Posix -> Posix -> Bool
between pa pb pc =
    let
        ( a, b, c ) =
            ( Time.posixToMillis pa, Time.posixToMillis pb, Time.posixToMillis pc )
    in
    a > b && a < c


intersect : Posix -> Posix -> Posix -> Posix -> Bool
intersect pa1 pa2 pb1 pb2 =
    let
        ( a1, b1 ) =
            ( Time.posixToMillis pa1, Time.posixToMillis pb1 )
    in
    if a1 >= b1 then
        a1 <= Time.posixToMillis pb2

    else
        b1 <= Time.posixToMillis pa2


minPosix : Posix -> Posix -> Posix
minPosix a b =
    min (Time.posixToMillis a) (Time.posixToMillis b) |> Time.millisToPosix


maxPosix : Posix -> Posix -> Posix
maxPosix a b =
    max (Time.posixToMillis a) (Time.posixToMillis b) |> Time.millisToPosix


durationBetween : Posix -> Posix -> Duration
durationBetween start end =
    Time.posixToMillis end
        - Time.posixToMillis start
        |> Duration


addDurationToPosix : Posix -> Duration -> Posix
addDurationToPosix posix (Duration dur) =
    Time.posixToMillis posix + dur |> Time.millisToPosix


subtractDuration : Posix -> Duration -> Posix
subtractDuration posix (Duration dur) =
    Time.posixToMillis posix - dur |> Time.millisToPosix


toHoursString : Duration -> String
toHoursString (Duration dur) =
    let
        hours =
            dur // 3600000

        minutes =
            (dur - (hours * 3600000)) // 60000

        seconds =
            (hours - (minutes * 60000)) // 1000
    in
    String.fromInt hours
        ++ "h "
        ++ (if minutes > 0 || seconds > 0 then
                String.fromInt minutes ++ "mn "

            else
                ""
           )
        ++ (if seconds > 0 then
                String.fromInt seconds ++ "s"

            else
                ""
           )


fr : Date.Language
fr =
    { monthName =
        \month ->
            case month of
                Jan ->
                    "janvier"

                Feb ->
                    "février"

                Mar ->
                    "mars"

                Apr ->
                    "avril"

                May ->
                    "mai"

                Jun ->
                    "juin"

                Jul ->
                    "juillet"

                Aug ->
                    "août"

                Sep ->
                    "septembre"

                Oct ->
                    "octobre"

                Nov ->
                    "novembre"

                Dec ->
                    "décembre"
    , monthNameShort =
        \month ->
            case month of
                Jan ->
                    "jan"

                Feb ->
                    "fév"

                Mar ->
                    "mar"

                Apr ->
                    "avr"

                May ->
                    "mai"

                Jun ->
                    "juin"

                Jul ->
                    "juil"

                Aug ->
                    "aoû"

                Sep ->
                    "sep"

                Oct ->
                    "oct"

                Nov ->
                    "nov"

                Dec ->
                    "déc"
    , weekdayName =
        \weekday ->
            case weekday of
                Mon ->
                    "lundi"

                Tue ->
                    "mardi"

                Wed ->
                    "mercredi"

                Thu ->
                    "jeudi"

                Fri ->
                    "vendredi"

                Sat ->
                    "samdi"

                Sun ->
                    "dimanche"
    , weekdayNameShort =
        \weekday ->
            case weekday of
                Mon ->
                    "lun"

                Tue ->
                    "mar"

                Wed ->
                    "mer"

                Thu ->
                    "jeu"

                Fri ->
                    "ven"

                Sat ->
                    "sam"

                Sun ->
                    "dim"
    , dayWithSuffix =
        \day ->
            if day == 1 then
                "er"

            else
                ""
    }


type Moment
    = Year
    | Month
    | Week
    | Day
    | Hour
    | Minute


startOf : Moment -> Zone -> Int -> Posix -> Posix
startOf unit zone delta date =
    case unit of
        Year ->
            TE.floor TE.Year zone date

        Month ->
            TE.floor TE.Month zone date

        Week ->
            TE.floor TE.Week zone date

        Day ->
            TE.floor TE.Day zone date

        Hour ->
            let
                parts =
                    TE.posixToParts zone date
            in
            TE.partsToPosix zone
                { parts
                    | hour = parts.hour - modBy delta parts.hour
                    , minute = 0
                    , second = 0
                    , millisecond = 0
                }

        -- TE.floor TE.Hour zone date
        Minute ->
            let
                parts =
                    TE.posixToParts zone date
            in
            TE.partsToPosix zone
                { parts
                    | minute = parts.minute - modBy delta parts.minute
                    , second = 0
                    , millisecond = 0
                }



-- TE.floor TE.Minute zone date


changeHour : Int -> Zone -> Posix -> Posix
changeHour hour zone date =
    let
        parts =
            TE.posixToParts zone date
    in
    TE.partsToPosix zone { parts | hour = hour }


add : Moment -> Int -> Zone -> Posix -> Posix
add unit plus zone date =
    case unit of
        Year ->
            TE.add TE.Year plus zone date

        Month ->
            TE.add TE.Month plus zone date

        Week ->
            TE.add TE.Week plus zone date

        Day ->
            TE.add TE.Day plus zone date

        Hour ->
            TE.add TE.Hour plus zone date

        Minute ->
            TE.add TE.Minute plus zone date


format : Zone -> Moment -> Maybe String -> Posix -> String
format zone unit mbformat posix =
    if mbformat == Just "" then
        ""

    else
        case unit of
            Year ->
                Date.formatWithLanguage fr (Maybe.withDefault "y" mbformat) (Date.fromPosix zone posix)

            Month ->
                Date.formatWithLanguage fr (Maybe.withDefault "MMM" mbformat) (Date.fromPosix zone posix)

            Week ->
                (if mbformat == Nothing then
                    "s"

                 else
                    ""
                )
                    ++ Date.formatWithLanguage fr (Maybe.withDefault "ww" mbformat) (Date.fromPosix zone posix)

            Day ->
                Date.formatWithLanguage fr (Maybe.withDefault "EEE dd" mbformat) (Date.fromPosix zone posix)

            Hour ->
                let
                    minutes =
                        Time.toMinute zone posix
                in
                (Time.toHour zone posix |> String.fromInt)
                    ++ "h"
                    ++ (if minutes > 0 then
                            minutes |> String.fromInt |> String.padLeft 2 '0'

                        else
                            ""
                       )

            Minute ->
                Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0'
