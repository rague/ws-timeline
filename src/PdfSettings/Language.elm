module PdfSettings.Language exposing (..)

import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "align", I18Next.string "Align" )
                , ( "alignLeft", I18Next.string "Left" )
                , ( "alignMiddle", I18Next.string "Middle" )
                , ( "alignRight", I18Next.string "Right" )
                , ( "chooseColumn", I18Next.string "Choose a column" )
                , ( "chooseTable", I18Next.string "Choose a table" )
                , ( "colorField", I18Next.string "Color" )
                , ( "content", I18Next.string "Content" )
                , ( "data", I18Next.string "Data" )
                , ( "dataAndAppearance", I18Next.string "Data and appearance" )
                , ( "direction", I18Next.string "Direction" )
                , ( "filters", I18Next.string "Filters" )
                , ( "fromDate", I18Next.string "Start date" )
                , ( "fromField", I18Next.string "From field" )
                , ( "groupField", I18Next.string "Group by" )
                , ( "groupFontSize", I18Next.string "Groups" )
                , ( "horizontal", I18Next.string "Horizontal" )
                , ( "horizontalPages", I18Next.string "Pages in width" )
                , ( "hourFontSize", I18Next.string "Hours" )
                , ( "landscape", I18Next.string "Landscape" )
                , ( "orientation", I18Next.string "Orientation" )
                , ( "pageLayout", I18Next.string "Layout" )
                , ( "paperSize", I18Next.string "Paper size" )
                , ( "pdfDownload", I18Next.string "PDF Download" )
                , ( "period", I18Next.string "Period" )
                , ( "portrait", I18Next.string "Portrait" )
                , ( "settings", I18Next.string "Settings" )
                , ( "sort", I18Next.string "Sort" )
                , ( "sortAndFilter", I18Next.string "Sort and filter" )
                , ( "subGroupField", I18Next.string "Subgroup by" )
                , ( "table", I18Next.string "Table" )
                , ( "taskFontSize", I18Next.string "Tasks" )
                , ( "tasksLayout", I18Next.string "Tasks layout" )
                , ( "textSize", I18Next.string "Text size" )
                , ( "title", I18Next.string "title" )
                , ( "toDate", I18Next.string "End date" )
                , ( "toField", I18Next.string "To field" )
                , ( "vertical", I18Next.string "Vertical" )
                , ( "verticalPages", I18Next.string "Pages en height" )
                , ( "wrapText", I18Next.string "Wrap text" )
                ]
          )
        ]
