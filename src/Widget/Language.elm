module Widget.Language exposing (..)

import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "color", I18Next.string "Color" )
                , ( "comment", I18Next.string "Comment" )
                , ( "content", I18Next.string "Content" )
                , ( "cumulativeDuration"
                  , I18Next.string "Cumulative duration: "
                  )
                , ( "daysShort", I18Next.string "d" )
                , ( "duration", I18Next.string "Duration" )
                , ( "editableColumns", I18Next.string "Editable columns" )
                , ( "endDate", I18Next.string "End date" )
                , ( "groupBy", I18Next.string "Group by" )
                , ( "horizontal", I18Next.string "Horizontal" )
                , ( "hoursShort", I18Next.string "h" )
                , ( "moment", I18Next.string "moment" )
                , ( "moment_plural", I18Next.string "moments" )
                , ( "settings", I18Next.string "Settings" )
                , ( "startDate", I18Next.string "Start date" )
                , ( "subgroupBy", I18Next.string "Then group by" )
                , ( "timeRange", I18Next.string "Time range: " )
                , ( "timelineDirection", I18Next.string "Timeline direction" )
                , ( "vertical", I18Next.string "Vertical" )
                ]
          )
        ]
