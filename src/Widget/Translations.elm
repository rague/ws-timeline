module Widget.Translations exposing (..)

import I18Next


color : List I18Next.Translations -> String
color translations =
    I18Next.tf translations "color"


comment : List I18Next.Translations -> String
comment translations =
    I18Next.tf translations "comment"


content : List I18Next.Translations -> String
content translations =
    I18Next.tf translations "content"


cumulativeDuration : List I18Next.Translations -> String
cumulativeDuration translations =
    I18Next.tf translations "cumulativeDuration"


daysShort : List I18Next.Translations -> String
daysShort translations =
    I18Next.tf translations "daysShort"


duration : List I18Next.Translations -> String
duration translations =
    I18Next.tf translations "duration"


editableColumns : List I18Next.Translations -> String
editableColumns translations =
    I18Next.tf translations "editableColumns"


endDate : List I18Next.Translations -> String
endDate translations =
    I18Next.tf translations "endDate"


groupBy : List I18Next.Translations -> String
groupBy translations =
    I18Next.tf translations "groupBy"


horizontal : List I18Next.Translations -> String
horizontal translations =
    I18Next.tf translations "horizontal"


hoursShort : List I18Next.Translations -> String
hoursShort translations =
    I18Next.tf translations "hoursShort"


moment : List I18Next.Translations -> String
moment translations =
    I18Next.tf translations "moment"


momentPlural : List I18Next.Translations -> String
momentPlural translations =
    I18Next.tf translations "moment_plural"


settings : List I18Next.Translations -> String
settings translations =
    I18Next.tf translations "settings"


startDate : List I18Next.Translations -> String
startDate translations =
    I18Next.tf translations "startDate"


subgroupBy : List I18Next.Translations -> String
subgroupBy translations =
    I18Next.tf translations "subgroupBy"


timeRange : List I18Next.Translations -> String
timeRange translations =
    I18Next.tf translations "timeRange"


timelineDirection : List I18Next.Translations -> String
timelineDirection translations =
    I18Next.tf translations "timelineDirection"


vertical : List I18Next.Translations -> String
vertical translations =
    I18Next.tf translations "vertical"
