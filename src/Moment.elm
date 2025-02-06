module Moment exposing (Duration, Moment(..), add, addDuration, addDurationToPosix, between, durationBetween, durationIsZero, durationNotZero, format, formatI18n, fromDuration, greaterThan, intersect, lessOrEqualThan, lessThan, mapDuration, maxPosix, minPosix, startOf, subtractDuration, toDuration)

import Cldr.Format.Date as FDate
import Cldr.Format.DateTime as FDateTime
import Cldr.Format.Length as FLength
import Cldr.Format.Options as O
import Cldr.Format.OptionsBuilder as B
import Cldr.Locale exposing (Locale)
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


format : Locale -> Zone -> Moment -> Maybe String -> Posix -> String
format locale zone unit mbformat posix =
    if mbformat == Just "" then
        ""

    else
        case unit of
            Year ->
                formatI18n locale zone (Maybe.withDefault "yyyy" mbformat) posix

            -- Date.formatWithLanguage fr (Maybe.withDefault "y" mbformat) (Date.fromPosix zone posix)
            Month ->
                formatI18n locale zone (Maybe.withDefault "MMMM" mbformat) posix

            -- Date.formatWithLanguage fr (Maybe.withDefault "MMM" mbformat) (Date.fromPosix zone posix)
            Week ->
                (if mbformat == Nothing then
                    "w"

                 else
                    ""
                )
                    ++ Date.formatWithLanguage fr (Maybe.withDefault "ww" mbformat) (Date.fromPosix zone posix)

            Day ->
                formatI18n locale zone (Maybe.withDefault "ddd dd" mbformat) posix

            -- Date.formatWithLanguage fr (Maybe.withDefault "EEE dd" mbformat) (Date.fromPosix zone posix)
            Hour ->
                FDateTime.format
                    (FDateTime.TimeOnly FLength.Short)
                    -- (FDateTime.WithOptions (B.initDateTime |> B.setHour O.TwoDigit |> B.toOptions))
                    locale
                    zone
                    posix
                    |> String.replace ":00" ""

            -- let
            --     minutes =
            --         Time.toMinute zone posix
            -- in
            -- (Time.toHour zone posix |> String.fromInt)
            --     ++ "h"
            --     ++ (if minutes > 0 then
            --             minutes |> String.fromInt |> String.padLeft 2 '0'
            --         else
            --             ""
            --        )
            Minute ->
                Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0'


formatI18n : Locale -> Zone -> String -> Posix -> String
formatI18n lang zone fmt time =
    fmt
        |> String.split ""
        |> group
        |> List.map String.concat
        |> List.map (formatPart lang zone time)
        |> String.concat


stringPadLeft20 i =
    i
        |> String.fromInt
        |> String.padLeft 2 '0'


stringTake n i =
    i
        |> String.fromInt
        |> String.split ""
        |> List.take n
        |> String.concat


to12hour n =
    if n == 12 then
        12

    else
        modBy 12 n


formatPart : Locale -> Zone -> Posix -> String -> String
formatPart locale zone time part =
    case part of
        "d" ->
            Time.toDay zone time
                |> String.fromInt

        "dd" ->
            Time.toDay zone time
                |> stringPadLeft20

        "ddd" ->
            FDate.format
                (FDate.WithOptions (B.initDate |> B.setWeekday O.Short |> B.toOptions))
                locale
                (Date.fromPosix zone time)

        "dddd" ->
            FDate.format
                (FDate.WithOptions (B.initDate |> B.setWeekday O.Long |> B.toOptions))
                locale
                (Date.fromPosix zone time)

        "h" ->
            Time.toHour zone time
                |> to12hour
                |> String.fromInt

        "hh" ->
            Time.toHour zone time
                |> to12hour
                |> stringPadLeft20

        "H" ->
            Time.toHour zone time
                |> String.fromInt

        "HH" ->
            Time.toHour zone time
                |> stringPadLeft20

        "m" ->
            Time.toMinute zone time
                |> String.fromInt

        "mm" ->
            Time.toMinute zone time
                |> stringPadLeft20

        "M" ->
            FDate.format
                (FDate.WithOptions (B.initDate |> B.setMonthNumber O.Numeric |> B.toOptions))
                locale
                (Date.fromPosix zone time)

        "MM" ->
            FDate.format
                (FDate.WithOptions (B.initDate |> B.setMonthNumber O.TwoDigit |> B.toOptions))
                locale
                (Date.fromPosix zone time)

        "MMM" ->
            FDate.format
                (FDate.WithOptions (B.initDate |> B.setMonthText O.Short |> B.toOptions))
                locale
                (Date.fromPosix zone time)

        "MMMM" ->
            FDate.format
                (FDate.WithOptions (B.initDate |> B.setMonthText O.Long |> B.toOptions))
                locale
                (Date.fromPosix zone time)

        "s" ->
            Time.toSecond zone time
                |> String.fromInt

        "ss" ->
            Time.toSecond zone time
                |> stringPadLeft20

        "t" ->
            if Time.toHour zone time < 12 then
                "A"

            else
                "P"

        "tt" ->
            if Time.toHour zone time < 12 then
                "AM"

            else
                "PM"

        "yy" ->
            Time.toYear zone time
                |> modBy 100
                |> stringPadLeft20

        "yyyy" ->
            Time.toYear zone time
                |> String.fromInt

        "f" ->
            Time.toMillis zone time
                |> stringTake 1

        "ff" ->
            Time.toMillis zone time
                |> stringTake 2

        "fff" ->
            Time.toMillis zone time
                |> stringTake 3

        "x" ->
            FDate.format
                (FDate.WithOptions
                    (B.initDate
                        |> B.setWeekday O.Short
                        |> B.setDay O.Numeric
                        |> B.setMonthNumber O.Numeric
                        |> B.setYear O.TwoDigit
                        |> B.toOptions
                    )
                )
                locale
                (Date.fromPosix zone time)

        "xx" ->
            FDate.format
                (FDate.WithOptions
                    (B.initDate
                        |> B.setWeekday O.Short
                        |> B.setDay O.Numeric
                        |> B.setMonthText O.Short
                        |> B.setYear O.Numeric
                        |> B.toOptions
                    )
                )
                locale
                (Date.fromPosix zone time)

        a ->
            a



-- LIST


group : List a -> List (List a)
group =
    group_ []


group_ : List (List a) -> List a -> List (List a)
group_ acc fmt =
    case fmt of
        [] ->
            List.reverse acc

        a :: bs ->
            case acc of
                [] ->
                    group_ [ [ a ] ] bs

                (y :: ys) :: xs ->
                    if a == y then
                        group_ ((a :: y :: ys) :: xs) bs

                    else
                        group_ ([ a ] :: (y :: ys) :: xs) bs

                [] :: _ ->
                    acc
