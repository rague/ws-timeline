module PdfSettings.Translations exposing (..)

import I18Next


align : List I18Next.Translations -> String
align translations =
    I18Next.tf translations "align"


alignLeft : List I18Next.Translations -> String
alignLeft translations =
    I18Next.tf translations "alignLeft"


alignMiddle : List I18Next.Translations -> String
alignMiddle translations =
    I18Next.tf translations "alignMiddle"


alignRight : List I18Next.Translations -> String
alignRight translations =
    I18Next.tf translations "alignRight"


colorField : List I18Next.Translations -> String
colorField translations =
    I18Next.tf translations "colorField"


content : List I18Next.Translations -> String
content translations =
    I18Next.tf translations "content"


fromDate : List I18Next.Translations -> String
fromDate translations =
    I18Next.tf translations "fromDate"


fromField : List I18Next.Translations -> String
fromField translations =
    I18Next.tf translations "fromField"


groupField : List I18Next.Translations -> String
groupField translations =
    I18Next.tf translations "groupField"


groupFontSize : List I18Next.Translations -> String
groupFontSize translations =
    I18Next.tf translations "groupFontSize"


horizontal : List I18Next.Translations -> String
horizontal translations =
    I18Next.tf translations "horizontal"


horizontalPages : List I18Next.Translations -> String
horizontalPages translations =
    I18Next.tf translations "horizontalPages"


hourFontSize : List I18Next.Translations -> String
hourFontSize translations =
    I18Next.tf translations "hourFontSize"


landscape : List I18Next.Translations -> String
landscape translations =
    I18Next.tf translations "landscape"


multiline : List I18Next.Translations -> String
multiline translations =
    I18Next.tf translations "multiline"


orientation : List I18Next.Translations -> String
orientation translations =
    I18Next.tf translations "orientation"


pageLayout : List I18Next.Translations -> String
pageLayout translations =
    I18Next.tf translations "pageLayout"


paperSize : List I18Next.Translations -> String
paperSize translations =
    I18Next.tf translations "paperSize"


portrait : List I18Next.Translations -> String
portrait translations =
    I18Next.tf translations "portrait"


subGroupField : List I18Next.Translations -> String
subGroupField translations =
    I18Next.tf translations "subGroupField"


table : List I18Next.Translations -> String
table translations =
    I18Next.tf translations "table"


taskFontSize : List I18Next.Translations -> String
taskFontSize translations =
    I18Next.tf translations "taskFontSize"


textSize : List I18Next.Translations -> String
textSize translations =
    I18Next.tf translations "textSize"


title : List I18Next.Translations -> String
title translations =
    I18Next.tf translations "title"


toDate : List I18Next.Translations -> String
toDate translations =
    I18Next.tf translations "toDate"


toField : List I18Next.Translations -> String
toField translations =
    I18Next.tf translations "toField"


vertical : List I18Next.Translations -> String
vertical translations =
    I18Next.tf translations "vertical"


verticalPages : List I18Next.Translations -> String
verticalPages translations =
    I18Next.tf translations "verticalPages"
