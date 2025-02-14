module Timeline.Utils exposing (between, findSection, findSectionById, intersect, setLasso)

import Dict
import List.Extra as Extra
import Moment
import Time exposing (Posix)
import Timeline.Models exposing (..)


between : comparable -> comparable -> comparable -> Bool
between a b c =
    a > b && a < c


findSectionById : List SectionBox -> SectionId -> Maybe SectionBox
findSectionById sections id =
    Extra.find (\s -> s.section.id == id) sections


findSection : Posix -> ( Float, Float ) -> List SectionBox -> Maybe SectionBox
findSection posix ( line, size ) sections =
    Extra.find
        (\({ section } as sbox) ->
            Moment.between posix section.start section.end
                && (section.isGlobal || between line (toFloat sbox.line) (toFloat sbox.line + size))
        )
        sections



{-
   b1 a1 b2 a2
   b1 a1 a2 b2
   b1 b2 a1 a2 *
   a1 b1 b2 a2
   a1 b1 a2 b2
   a1 a2 b1 b2 *
-}


intersect : comparable -> comparable -> comparable -> comparable -> Bool
intersect a1 a2 b1 b2 =
    if a1 >= b1 then
        a1 <= b2

    else
        b1 <= a2



-- findSectionsIds : Position -> Position -> List SectionBox -> Set SectionId
-- findSectionsIds ( fromH, fromV ) ( toH, toV ) sections =
--     List.filter
--         (\({ section } as sbox) ->
--             Moment.intersect section.start section.end fromH toH
--                 && intersect (toFloat sbox.line) (toFloat sbox.line + 1) fromV toV
--         )
--         sections
--         |> List.map (.section >> .id)
--         |> Set.fromList


findSectionsIds : Position -> Position -> List GroupBox -> Selection
findSectionsIds ( fromH, fromV ) ( toH, toV ) groups =
    groups
        |> List.foldl
            (\gbox selset ->
                List.foldl
                    (\({ section } as sbox) set ->
                        if
                            Moment.intersect section.start section.end fromH toH
                                && (section.isGlobal || intersect (toFloat (sbox.line + gbox.position)) (toFloat (sbox.line + gbox.position) + 1) fromV toV)
                                && not gbox.isSubtotal
                        then
                            addToSelection gbox.id [ section.id ] set

                        else
                            set
                    )
                    selset
                    gbox.sections
            )
            emptySelection


setLasso : SelectAction -> TimelineBox -> Position -> Position -> TimelineBox
setLasso interaction box ( fromP, fromL ) ( toP, toL ) =
    let
        ( start, end ) =
            if Moment.greaterThan fromP toP then
                ( toP, fromP )

            else
                ( fromP, toP )

        ( startLine, endLine ) =
            if fromL > toL then
                ( toL, fromL )

            else
                ( fromL, toL )
    in
    { box
        | interaction =
            Select interaction
                ( fromP, fromL )
                (findSectionsIds ( start, startLine ) ( end, endLine ) (Dict.values box.groups))
                ( ( start, startLine ), ( Moment.durationBetween start end, endLine - startLine ) )
    }
