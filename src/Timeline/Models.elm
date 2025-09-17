module Timeline.Models exposing
    ( Direction(..)
    , Group
    , GroupBox
    , GroupId
    , Interaction(..)
    , MoveType(..)
    , Period
    , Position
    , Section
    , SectionBox
    , SectionId
    , Sectionic
    , SelectAction(..)
    , Selection
    , TimelineBox
    , Vertex
    , addSelection
    , addToSelection
    , alpha
    , alphaInfo
    , anis
    , azur
    , bleu
    , cyan
    , directionGetter
    , duration
    , emptySelection
    , errorName
    , findColor
    , findColorName
    , foldSelection
    , groupDiffSelection
    , isGroupSelected
    , isSelected
    , jaune
    , magenta
    , mapSelection
    , mauve
    , maxDuration
    , meshesForGroups
    , minDurationOf
    , modifySections
    , mouseOverInteraction
    , moveSections
    , normalize
    , okName
    , orange
    , orderGroups
    , problemName
    , rose
    , rouge
    , selectedGroups
    , selectedSections
    , selection
    , selectionIsEmpty
    , selectionOverManyGroups
    , selectionSize
    , selectionToSet
    , selectionWith
    , substractSelection
    , turquoise
    , unscale
    , vert
    , warningName
    )

import Cldr.Locale exposing (Locale)
import Color exposing (Color)
import Dict exposing (Dict)
import DnDList
import Json.Decode exposing (dict)
import List.Extra
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector4 exposing (Vec4, vec4)
import Moment exposing (Duration)
import Set exposing (Set)
import Time exposing (Posix)
import WebGL exposing (Mesh)


type alias GroupId =
    String


type alias SectionId =
    String


type alias Period a =
    { a
        | start : Posix
        , end : Posix
    }



--type Color = Blue | Cyan | Green | Orange | Pink
--  | Purple | Error | Problem | Valid | Warning | NoColor


type alias Sectionic =
    { id : SectionId
    , color : String
    , isLocked : Bool
    , labels : List String
    , hasComment : Bool
    , isGlobal : Bool
    }


type alias Section =
    Period Sectionic


type alias Group =
    { id : GroupId
    , sections : List Section
    , label : List String
    , isSubtotal : Bool
    }


type alias SectionBox =
    { groupId : GroupId
    , line : Int
    , section : Section
    }


type alias GroupBox =
    { id : GroupId
    , position : Int
    , size : Int
    , label : List String
    , isSubtotal : Bool
    , sections : List SectionBox
    }


type Direction
    = Horizontal
    | Vertical


type alias Vertex =
    { position : Vec2 -- x y isBottom
    , color : Vec4
    , selected : Float
    , border : Float
    , middle : Vec2
    , comment : Float
    }


type Selection
    = Selection (Dict GroupId (Set SectionId))


selection : TimelineBox -> Selection
selection tm =
    tm.selection


mapSelection : (GroupId -> Set SectionId -> Set SectionId) -> Selection -> Selection
mapSelection func (Selection sel) =
    Dict.map func sel
        |> Selection


selectedSections : TimelineBox -> List Section
selectedSections tm =
    let
        (Selection sel) =
            tm.selection
    in
    Dict.foldl
        (\gid set res ->
            let
                list =
                    Dict.get gid tm.groups
                        |> Maybe.map (\gb -> List.filter (\sb -> Set.member (.id sb.section) set) gb.sections)
                        |> Maybe.withDefault []
            in
            list ++ res
        )
        []
        sel
        |> List.map .section
        |> List.sortBy (.start >> Time.posixToMillis)


selectionSize : Selection -> Int
selectionSize (Selection dict) =
    Dict.foldl (\_ set out -> Set.size set + out) 0 dict


selectionIsEmpty : Selection -> Bool
selectionIsEmpty (Selection dict) =
    Dict.foldl (\_ set out -> Set.isEmpty set && out) True dict


selectionToSet : Selection -> Set SectionId
selectionToSet (Selection dict) =
    Dict.foldl (\_ set out -> Set.union set out) Set.empty dict


emptySelection : Selection
emptySelection =
    Selection Dict.empty


selectionWith : GroupId -> SectionId -> Selection
selectionWith gid sid =
    Selection (Dict.singleton gid (Set.singleton sid))


addToSelection : GroupId -> List SectionId -> Selection -> Selection
addToSelection gid ids (Selection dict) =
    let
        addSet =
            Set.fromList ids
    in
    Dict.update gid
        (Maybe.map (Set.union addSet)
            >> Maybe.withDefault addSet
            >> Just
        )
        dict
        |> Selection


selectionOverManyGroups : Selection -> Bool
selectionOverManyGroups (Selection dict) =
    Dict.size dict > 1


addSelection : Selection -> Selection -> Selection
addSelection (Selection add) (Selection sel) =
    Dict.merge Dict.insert (\gid left right result -> Dict.insert gid (Set.union left right) result) Dict.insert add sel Dict.empty
        |> Selection


substractSelection : Selection -> Selection -> Selection
substractSelection (Selection rem) (Selection sel) =
    Dict.merge (\_ _ result -> result) (\gid remS selS result -> Dict.insert gid (Set.diff selS remS) result) Dict.insert rem sel Dict.empty
        |> Selection


selectedGroups : Selection -> Set GroupId
selectedGroups (Selection sel) =
    Dict.keys sel |> Set.fromList


groupDiffSelection : Selection -> Selection -> Set GroupId
groupDiffSelection (Selection left) (Selection right) =
    Dict.merge (\gid _ result -> Set.insert gid result)
        (\gid lset rset result ->
            if lset == rset then
                result

            else
                Set.insert gid result
        )
        (\gid _ result -> Set.insert gid result)
        left
        right
        Set.empty


isSelected : GroupId -> SectionId -> Selection -> Bool
isSelected gid sid (Selection sel) =
    Dict.get gid sel
        |> Maybe.map (\set -> Set.member sid set)
        |> Maybe.withDefault False


isGroupSelected : GroupId -> Selection -> Bool
isGroupSelected gid (Selection sel) =
    Dict.get gid sel
        |> Maybe.map (\set -> not (Set.isEmpty set))
        |> Maybe.withDefault False


foldSelection : (GroupId -> SectionId -> result -> result) -> result -> Selection -> result
foldSelection fun acc (Selection dict) =
    Dict.foldl (\gid set result -> Set.foldl (fun gid) result set) acc dict


type alias TimelineBox =
    { groups : Dict GroupId GroupBox
    , srcgroups : List Group
    , groupsLen : Int
    , lines : Int
    , sections : List SectionBox
    , meshesSelection : Selection
    , meshes : Dict GroupId { position : Float, meshes : Mesh Vertex }
    , selectedMeshes : Dict GroupId { position : Float, meshes : Mesh Vertex }
    , sectionOffsetY : Float
    , start : Float
    , zoom : Float
    , zoomChange : Int
    , lineSize : Float
    , first : Posix
    , direction : Direction
    , selection : Selection
    , interaction : Interaction
    , standby : Bool
    , dnd : DnDList.Model
    , locale : Locale
    , zone : Time.Zone
    , canSortGroups : Bool
    , canEditGroups : Bool
    , canEditSections : Bool
    , displayAxis : Bool
    , wrapText : Bool
    , currentPosix : Posix
    , groupsSize : Int
    }



---- interactions


type alias Position =
    ( Posix, Float )


type SelectAction
    = AddSelection
    | SubstractSelection


type Interaction
    = MouseOver Position
    | Select SelectAction Position Selection ( Position, ( Duration, Float ) )
    | Move MoveType SectionBox Position ( Duration, Int )
    | ResizeRight Position Duration
    | ResizeLeft Position Duration
    | Draw Posix Posix Int
    | EditGroupLabel GroupId String
    | ResizeGroups Int


type MoveType
    = Clone
    | SimpleMove


mouseOverInteraction : TimelineBox -> Float -> Float -> Interaction
mouseOverInteraction box x y =
    MouseOver (normalize box x y)


normalize : TimelineBox -> Float -> Float -> Position
normalize box ax ay =
    let
        dir =
            directionGetter box.direction
    in
    Tuple.mapBoth
        (\x ->
            ((x - box.start) * duration.day / box.zoom + (Time.posixToMillis box.first |> toFloat))
                |> round
                |> Time.millisToPosix
        )
        (\y -> (y - 2 - box.sectionOffsetY) / box.lineSize)
        ( dir.date ( ax, ay ), dir.line ( ax, ay ) )


maxDuration : number
maxDuration =
    365 * 100 * 24 * 60 * 60 * 1000



--- functions


duration : { day : Float }
duration =
    { day = 24 * 60 * 60 * 1000 }


minDurationOf : Dict GroupId GroupBox -> Selection -> Int
minDurationOf groups sel =
    foldSelection
        (\gid sid res ->
            case Dict.get gid groups of
                Nothing ->
                    res

                Just gbox ->
                    case List.Extra.find (\a -> a.section.id == sid) gbox.sections of
                        Nothing ->
                            res

                        Just s ->
                            min (Time.posixToMillis s.section.end - Time.posixToMillis s.section.start) res
        )
        maxDuration
        sel


unscale : Float -> Float -> Float
unscale zoom delta =
    (delta * duration.day) / zoom


directionGetter :
    Direction
    ->
        { h : ( a, a ) -> a
        , v : ( a, a ) -> a
        , date : ( a, a ) -> a
        , line : ( a, a ) -> a
        , xy : ( a, a ) -> ( a, a )
        , mapBoth : (a -> a) -> (a -> a) -> ( a, a ) -> ( a, a )
        }
directionGetter dir =
    case dir of
        Horizontal ->
            { h = Tuple.first
            , v = Tuple.second
            , date = Tuple.first
            , line = Tuple.second
            , xy = identity
            , mapBoth = \hfunc vfunc ( a, b ) -> ( hfunc a, vfunc b )
            }

        Vertical ->
            { h = Tuple.second
            , v = Tuple.first
            , date = Tuple.second
            , line = Tuple.first
            , xy = \( x, y ) -> ( y, x )
            , mapBoth = \hfunc vfunc ( a, b ) -> ( vfunc a, hfunc b )
            }


mapSections : List Group -> Selection -> (Section -> Section) -> List Group
mapSections groups sel mapfunc =
    List.map
        (\group ->
            let
                sections =
                    List.Extra.updateIf (\s -> isSelected group.id s.id sel)
                        mapfunc
                        group.sections
            in
            { group
                | sections =
                    List.sortBy (.start >> Time.posixToMillis) sections
            }
        )
        groups


modifySections : List Group -> Selection -> ( Duration, Duration ) -> List Group
modifySections groups ids ( move, resize ) =
    mapSections groups ids <|
        \section ->
            let
                newStart =
                    if Moment.durationIsZero resize then
                        Moment.addDurationToPosix section.start move
                        -- move + section.start

                    else
                        Moment.minPosix (Moment.addDurationToPosix section.start move) section.end

                -- min (move + section.start) section.end
            in
            { section
                | start =
                    newStart
                , end =
                    Moment.durationBetween section.start section.end
                        |> Moment.addDuration resize
                        |> Moment.mapDuration (max 0)
                        |> Moment.addDurationToPosix newStart

                -- max (resize + section.end - section.start) 0 + newStart
            }


twinfilter : (a -> Bool) -> List a -> ( List a, List a )
twinfilter pred list =
    List.foldl
        (\item ( la, lb ) ->
            if pred item then
                ( la ++ [ item ], lb )

            else
                ( la, lb ++ [ item ] )
        )
        ( [], [] )
        list


moveSections : List Group -> Selection -> GroupId -> List Group
moveSections groups ids destg =
    let
        ( minusGroups, selSections ) =
            List.foldl
                (\group ( grps, sects ) ->
                    let
                        ( adds, keep ) =
                            twinfilter (\s -> isSelected group.id s.id ids) group.sections

                        ng =
                            { group | sections = keep }
                    in
                    ( grps ++ [ ng ], sects ++ adds )
                )
                ( [], [] )
                groups
    in
    List.Extra.findIndex (\g -> g.id == destg) minusGroups
        |> Maybe.map
            (\i ->
                List.Extra.updateAt i (\g -> { g | sections = List.sortBy (.start >> Time.posixToMillis) <| g.sections ++ selSections }) minusGroups
            )
        |> Maybe.withDefault groups


orderGroups : List GroupId -> List Group -> List Group
orderGroups gids groups =
    let
        dict =
            List.map (\g -> ( g.id, g )) groups |> Dict.fromList
    in
    List.filterMap (\gid -> Dict.get gid dict) gids


rose : String
rose =
    "rose"


magenta : String
magenta =
    "magenta"


mauve : String
mauve =
    "mauve"


bleu : String
bleu =
    "bleu"


azur : String
azur =
    "azur"


cyan : String
cyan =
    "cyan"


turquoise : String
turquoise =
    "turquoise"


vert : String
vert =
    "vert"


anis : String
anis =
    "anis"


jaune : String
jaune =
    "jaune"


orange : String
orange =
    "orange"


rouge : String
rouge =
    "rouge"


errorName : String
errorName =
    "error"


warningName : String
warningName =
    "warning"


problemName : String
problemName =
    "problem"


okName : String
okName =
    "ok"


meshesForGroups :
    Posix
    -> Dict GroupId GroupBox
    -> List GroupId
    -> Selection
    -> ( Dict GroupId { position : Float, meshes : Mesh Vertex }, Dict GroupId { position : Float, meshes : Mesh Vertex } )
    -> ( Dict GroupId { position : Float, meshes : Mesh Vertex }, Dict GroupId { position : Float, meshes : Mesh Vertex } )
meshesForGroups firstDate groups groupsList sel oldMeshes =
    List.foldl
        (\gid ( mres, sres ) ->
            case Dict.get gid groups of
                Nothing ->
                    ( Dict.remove gid mres, Dict.remove gid sres )

                Just group ->
                    if List.isEmpty group.sections then
                        ( Dict.remove gid mres, Dict.remove gid sres )

                    else
                        List.map
                            (\sbox ->
                                { section = sbox.section
                                , line = sbox.line
                                , selected = isSelected gid sbox.section.id sel
                                }
                            )
                            group.sections
                            |> List.filter (.section >> .isGlobal >> not)
                            |> List.partition (.selected >> not)
                            |> Tuple.mapBoth (toMeshes group.isSubtotal firstDate >> (\m -> Dict.insert gid { position = toFloat group.position, meshes = m } mres))
                                (toMeshes group.isSubtotal firstDate >> (\m -> Dict.insert gid { position = toFloat group.position, meshes = m } sres))
        )
        oldMeshes
        groupsList


toMeshes : Bool -> Posix -> List { a | section : { b | start : Posix, end : Posix, color : String, isLocked : Bool, hasComment : Bool }, line : Int, selected : Bool } -> Mesh Vertex
toMeshes isSubtotal first taches =
    let
        firstms =
            Time.posixToMillis first |> toFloat
    in
    List.concatMap
        (\({ section } as tache) ->
            let
                start =
                    ((Time.posixToMillis section.start |> toFloat) - firstms) / 300000

                end =
                    ((Time.posixToMillis section.end |> toFloat) - firstms) / 300000

                line =
                    toFloat tache.line

                middle =
                    vec2 ((end + start) / 2) (line + 0.5)

                color =
                    findColor section.color
                        |> Color.toRgba

                background =
                    vec4 color.red
                        color.green
                        color.blue
                        (color.alpha
                            * (if section.isLocked then
                                0.3

                               else
                                1
                              )
                        )

                -- vec4 (color.red * color.alpha) (color.green * color.alpha) (color.blue * color.alpha) color.alpha
                border =
                    if isSubtotal then
                        128

                    else if section.isLocked then
                        3

                    else
                        1

                selected =
                    if tache.selected then
                        1

                    else
                        0

                -- vec4 (1 * d) (1 * d) (1 * d) a
                -- Math.Vector4.setW 0.5 (color tache.color)
                -- color tache.color
                hasComment =
                    if section.hasComment then
                        1

                    else
                        0

                topleft =
                    Vertex (vec2 start line) background selected border middle hasComment

                topright =
                    Vertex (vec2 end line) background selected border middle hasComment

                botleft =
                    Vertex (vec2 start (line + 1)) background selected border middle hasComment

                botright =
                    Vertex (vec2 end (line + 1)) background selected border middle hasComment
            in
            [ ( topleft
              , topright
              , botleft
              )
            , ( botleft
              , topright
              , botright
              )
            ]
        )
        taches
        |> WebGL.triangles


alpha =
    200


alphaInfo =
    210


pale =
    0.55


findColor : String -> Color
findColor name =
    if String.isEmpty name then
        Color.rgba255 170 170 170 alpha

    else if String.startsWith "#" name then
        Color.fromHex name
            |> Maybe.map (Color.setAlpha (alpha / 255))
            |> Maybe.withDefault (Color.rgba255 170 170 170 alpha)

    else
        Dict.get name colors
            |> Maybe.withDefault (Color.rgba255 170 170 170 alpha)


findColorName : String -> String
findColorName hex =
    case Color.fromHex hex |> Maybe.map Color.toHsla of
        Nothing ->
            hex

        Just { hue, saturation, lightness } ->
            Dict.remove "new" colors
                |> Dict.toList
                |> List.map
                    (\( name, color ) ->
                        let
                            rgba =
                                Color.toRgba color

                            r2 =
                                (1 - rgba.alpha) + (rgba.alpha * rgba.red)

                            g2 =
                                (1 - rgba.alpha) + (rgba.alpha * rgba.green)

                            b2 =
                                (1 - rgba.alpha) + (rgba.alpha * rgba.blue)

                            hsl =
                                Color.fromRgb { red = r2, green = g2, blue = b2 } |> Color.toHsla
                        in
                        ( name, (hsl.hue - hue) * 5 + (hsl.saturation - saturation) * 3 + (hsl.lightness - lightness) |> abs )
                    )
                |> List.Extra.minimumBy Tuple.second
                |> Maybe.map Tuple.first
                |> Maybe.withDefault ""


colors =
    Dict.fromList
        [ ( "new", Color.rgba255 255 255 255 230 )
        , ( "rose", Color.rgba255 255 0 170 alpha )
        , ( "rose-pale", Color.rgba255 255 0 170 alpha |> Color.mapAlpha ((*) pale) )
        , ( "magenta", Color.rgba255 240 0 255 alpha )
        , ( "magenta-pale", Color.rgba255 240 0 255 alpha |> Color.mapAlpha ((*) pale) )
        , ( "violet", Color.rgba255 140 50 255 alpha )
        , ( "violet-pale", Color.rgba255 120 0 255 alpha |> Color.mapAlpha ((*) pale) )
        , ( "bleu", Color.rgba255 50 110 255 alpha )
        , ( "bleu-pale", Color.rgba255 0 110 255 alpha |> Color.mapAlpha ((*) pale) )
        , ( "azur", Color.rgba255 0 170 255 alpha )
        , ( "azur-pale", Color.rgba255 0 170 255 alpha |> Color.mapAlpha ((*) pale) )
        , ( "cyan", Color.rgba255 0 240 255 alpha )
        , ( "cyan-pale", Color.rgba255 0 240 255 alpha |> Color.mapAlpha ((*) pale) )
        , ( "turquoise", Color.rgba255 0 255 170 alpha )
        , ( "turquoise-pale", Color.rgba255 0 255 170 alpha |> Color.mapAlpha ((*) pale) )
        , ( "vert", Color.rgba255 0 255 0 (alpha + 25) )
        , ( "vert-pale", Color.rgba255 0 255 0 (alpha + 25) |> Color.mapAlpha ((*) pale) )
        , ( "anis", Color.rgba255 170 245 0 (alpha + 25) )
        , ( "anis-pale", Color.rgba255 170 245 0 (alpha + 25) |> Color.mapAlpha ((*) pale) )
        , ( "jaune", Color.rgba255 255 220 0 (alpha + 25) )
        , ( "jaune-pale", Color.rgba255 255 220 0 (alpha + 25) |> Color.mapAlpha ((*) pale) )
        , ( "orange", Color.rgba255 255 160 0 (alpha + 25) )
        , ( "orange-pale", Color.rgba255 255 160 0 (alpha + 25) |> Color.mapAlpha ((*) pale) )
        , ( "rouge", Color.rgba255 255 0 0 (alpha + 0) )
        , ( "rouge-pale", Color.rgba255 255 0 0 (alpha + 0) |> Color.mapAlpha ((*) pale) )
        , ( "error", Color.rgba255 255 0 0 alphaInfo )
        , ( "problem", Color.rgba255 255 127 0 alphaInfo )
        , ( "warning", Color.rgba255 255 255 0 alphaInfo )
        , ( "ok", Color.rgba255 0 255 0 alphaInfo )
        , ( "gris", Color.rgba255 170 170 170 alpha )
        , ( "gris-pale", Color.rgba255 170 170 170 alpha )
        ]
