module PdfSettings.Language exposing (..)

import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "align", I18Next.string "Alignement" )
                , ( "alignLeft", I18Next.string "Gauche" )
                , ( "alignMiddle", I18Next.string "Milieu" )
                , ( "alignRight", I18Next.string "Droite" )
                , ( "colorField", I18Next.string "Couleur" )
                , ( "content", I18Next.string "contenu" )
                , ( "fromDate", I18Next.string "Début période" )
                , ( "fromField", I18Next.string "Champ début" )
                , ( "groupField", I18Next.string "Grouper par" )
                , ( "groupFontSize", I18Next.string "Groupes" )
                , ( "horizontal", I18Next.string "Horizontale" )
                , ( "horizontalPages", I18Next.string "Pages en largeur" )
                , ( "hourFontSize", I18Next.string "Heures" )
                , ( "landscape", I18Next.string "Paysage" )
                , ( "multiline", I18Next.string "Retour à la ligne" )
                , ( "orientation", I18Next.string "Orientation" )
                , ( "pageLayout", I18Next.string "Mise en page" )
                , ( "paperSize", I18Next.string "Taille du papier" )
                , ( "portrait", I18Next.string "Portrait" )
                , ( "subGroupField", I18Next.string "Sous-grouper par" )
                , ( "table", I18Next.string "Table" )
                , ( "taskFontSize", I18Next.string "Tâches" )
                , ( "textSize", I18Next.string "Taille du texte" )
                , ( "title", I18Next.string "Titre" )
                , ( "toDate", I18Next.string "Fin période" )
                , ( "toField", I18Next.string "Champ fin" )
                , ( "vertical", I18Next.string "Verticale" )
                , ( "verticalPages", I18Next.string "Pages en hauteur" )
                ]
          )
        ]
