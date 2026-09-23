port module Main exposing
    ( ChartKind(..)
    , ClipboardData
    , ClipboardFormat(..)
    , Col
    , Doc(..)
    , DocMsg(..)
    , Filter(..)
    , Freshness
    , Index
    , Moving(..)
    , Msg
    , NumberFormat(..)
    , Rect
    , Shade(..)
    , Sheet
    , SheetView
    , SortOrder(..)
    , Stat(..)
    , Table
    , TableBounds
    , Type(..)
    , When(..)
    , blankRows
    , canonicalTypeNames
    , cellDecoder
    , cellRewrites
    , chartAt
    , chartBoxes
    , chartFold
    , chartKinds
    , chartPoints
    , chartPointsMax
    , chartRuns
    , chartSpan
    , civilDays
    , clampIndex
    , columnExtent
    , columnSplit
    , computeBoolishStats
    , computeTemporalStats
    , cycleSort
    , dependents
    , detectFormat
    , displayYToDocY
    , docDecoder
    , dropOf
    , duplicateRows
    , emptySheet
    , emptyView
    , expandSelection
    , fillSeries
    , filterAndSortIndexed
    , formatNumber
    , freshnessCell
    , freshnessDecoder
    , kindSpec
    , knownTypeName
    , legendLayout
    , main
    , maxFuzzyPairs
    , maxFuzzyRows
    , movePatch
    , moveSelection
    , nameClash
    , nearDuplicates
    , nextSortOrder
    , normalizeRect
    , paletteCommands
    , parseAnnotation
    , parseCsv
    , parseDay
    , parseJson
    , parseTsv
    , pinLeft
    , pruneView
    , queryHome
    , rect
    , rectToIndices
    , rowDeletions
    , rowSplices
    , selectAll
    , serializeToTsv
    , seriesEncoder
    , shortcutGroups
    , similarity
    , skipHidden
    , sortRankOf
    , soundex
    , sparkMax
    , sparkValues
    , tableHome
    , typeName
    , unviewable
    , usd
    , viewDecoder
    , viewPatches
    , xy
    )

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Browser.Navigation as Nav
import Date
import Dict exposing (Dict)
import File exposing (File)
import Html as H exposing (Html, text)
import Html.Attributes as A
import Html.Events as A
import Html.Style as S
import Http
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Task
import Time
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), (<?>))
import Url.Parser.Query as UrlQ



---- NAVIGATION ---------------------------------------------------------------


type alias Index =
    { x : Int, y : Int }


type alias Rect =
    { a : Index, b : Index }


type alias TableBounds =
    { maxX : Int
    , maxY : Int
    }


type SortOrder
    = Ascending
    | Descending


xy : Int -> Int -> Index
xy x y =
    Index x y


rect : Int -> Int -> Int -> Int -> Rect
rect ax ay bx by =
    Rect (xy ax ay) (xy bx by)


clampIndex : TableBounds -> Index -> Index
clampIndex bounds idx =
    -- Elm's `clamp lo hi` answers `hi` when `lo > hi`. With no drawn rows
    -- `maxY` is 0, and row 0 is the header, which a keystroke renames.
    { x = clamp 0 bounds.maxX idx.x
    , y = clamp 1 (max 1 bounds.maxY) idx.y
    }


moveSelection : TableBounds -> Int -> Int -> Index -> Rect
moveSelection bounds dx dy current =
    let
        newIdx =
            clampIndex bounds { x = current.x + dx, y = current.y + dy }
    in
    Rect newIdx newIdx


{-| A plain click drops every other key; shift keeps them. A key that is already
sorted holds its rank rather than jumping to last.
-}
cycleSort : Bool -> String -> List ( String, SortOrder ) -> List ( String, SortOrder )
cycleSort shift key keys =
    let
        existing =
            iif shift keys []
    in
    case ( nextSortOrder (sortOrderOf key keys), sortRankOf key existing ) of
        ( Nothing, _ ) ->
            List.filter (\( k, _ ) -> k /= key) existing

        ( Just order, Just _ ) ->
            List.map (\( k, o ) -> ( k, iif (k == key) order o )) existing

        ( Just order, Nothing ) ->
            existing ++ [ ( key, order ) ]


sortOrderOf : String -> List ( String, SortOrder ) -> Maybe SortOrder
sortOrderOf key keys =
    List.filter (\( k, _ ) -> k == key) keys |> List.head |> Maybe.map Tuple.second


sortRankOf : String -> List ( String, SortOrder ) -> Maybe Int
sortRankOf key keys =
    List.indexedMap (\i ( k, _ ) -> ( i, k )) keys
        |> List.filter (\( _, k ) -> k == key)
        |> List.head
        |> Maybe.map (Tuple.first >> (+) 1)


cellText : String -> Row -> String
cellText key row =
    Dict.get key row |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""


compareBySort : List ( String, SortOrder ) -> ( Int, Row ) -> ( Int, Row ) -> Order
compareBySort keys ( _, r1 ) ( _, r2 ) =
    case keys of
        [] ->
            EQ

        ( key, order ) :: rest ->
            let
                ( v1, v2 ) =
                    ( cellText key r1, cellText key r2 )

                base =
                    Maybe.map2 compare (String.toFloat v1) (String.toFloat v2) |> Maybe.withDefault (compare v1 v2)

                first =
                    case ( order, base ) of
                        ( Descending, LT ) ->
                            GT

                        ( Descending, GT ) ->
                            LT

                        _ ->
                            base
            in
            iif (first == EQ) (compareBySort rest ( 0, r1 ) ( 0, r2 )) first


nextSortOrder : Maybe SortOrder -> Maybe SortOrder
nextSortOrder current =
    case current of
        Nothing ->
            Just Ascending

        Just Ascending ->
            Just Descending

        Just Descending ->
            Nothing


normalizeRect : Rect -> Rect
normalizeRect r =
    { a =
        { x = min r.a.x r.b.x
        , y = min r.a.y r.b.y
        }
    , b =
        { x = max r.a.x r.b.x
        , y = max r.a.y r.b.y
        }
    }


expandSelection : TableBounds -> Int -> Int -> Rect -> Rect
expandSelection bounds dx dy sel =
    let
        newB =
            clampIndex bounds { x = sel.b.x + dx, y = sel.b.y + dy }
    in
    { sel | b = newB }


selectAll : TableBounds -> Rect
selectAll bounds =
    Rect (xy 0 1) (clampIndex bounds (xy bounds.maxX bounds.maxY))


rectToIndices : Rect -> List Index
rectToIndices r =
    let
        normalized =
            normalizeRect r

        xs =
            List.range normalized.a.x normalized.b.x

        ys =
            List.range normalized.a.y normalized.b.y
    in
    List.concatMap (\y -> List.map (\x -> xy x y) xs) ys



---- CLIPBOARD ----------------------------------------------------------------


type alias ClipboardData =
    List (List String)


type ClipboardFormat
    = Tsv
    | Csv
    | JsonArray
    | PlainText


detectFormat : String -> ClipboardFormat
detectFormat clipText =
    let
        trimmed =
            String.trim clipText

        hasTab =
            String.contains "\t" clipText

        hasComma =
            String.contains "," clipText

        hasNewline =
            String.contains "\n" clipText
    in
    if String.startsWith "[" trimmed then
        JsonArray

    else if hasTab then
        Tsv

    else if hasComma && hasNewline then
        Csv

    else
        PlainText


parseTsv : String -> ClipboardData
parseTsv clipText =
    clipText
        |> String.lines
        |> List.filter (not << String.isEmpty)
        |> List.map (String.split "\t")


parseCsv : String -> ClipboardData
parseCsv clipText =
    clipText
        |> String.lines
        |> List.filter (not << String.isEmpty)
        |> List.map parseCsvRow


parseCsvRow : String -> List String
parseCsvRow row =
    parseCsvRowHelper row "" False []


parseCsvRowHelper : String -> String -> Bool -> List String -> List String
parseCsvRowHelper remaining current inQuotes acc =
    case String.uncons remaining of
        Nothing ->
            List.reverse (String.trim current :: acc)

        Just ( char, rest ) ->
            if char == '"' then
                if inQuotes then
                    case String.uncons rest of
                        Just ( '"', restAfterQuote ) ->
                            parseCsvRowHelper restAfterQuote (current ++ "\"") True acc

                        _ ->
                            parseCsvRowHelper rest current False acc

                else
                    parseCsvRowHelper rest current True acc

            else if char == ',' && not inQuotes then
                parseCsvRowHelper rest "" False (String.trim current :: acc)

            else
                parseCsvRowHelper rest (current ++ String.fromChar char) inQuotes acc


parseJson : String -> Result String ClipboardData
parseJson jsonText =
    let
        arrayOfArrays =
            D.list (D.list stringOrValue)

        arrayOfObjects =
            D.list (D.dict stringOrValue)
                |> D.map
                    (\objs ->
                        case objs of
                            [] ->
                                []

                            first :: _ ->
                                let
                                    keys =
                                        first
                                            |> Dict.keys
                                            |> List.sort
                                in
                                List.map
                                    (\obj ->
                                        List.map
                                            (\k -> Dict.get k obj |> Maybe.withDefault "")
                                            keys
                                    )
                                    objs
                    )

        stringOrValue =
            D.oneOf
                [ D.string
                , D.int |> D.map String.fromInt
                , D.float |> D.map String.fromFloat
                , D.bool
                    |> D.map
                        (\b ->
                            if b then
                                "true"

                            else
                                "false"
                        )
                , D.null ""
                , D.value |> D.map (E.encode 0)
                ]
    in
    jsonText
        |> D.decodeString
            (D.oneOf
                [ arrayOfArrays
                , arrayOfObjects
                ]
            )
        |> Result.mapError D.errorToString


serializeToTsv : List (List String) -> String
serializeToTsv rows =
    rows
        |> List.map (String.join "\t")
        |> String.join "\n"



---- HELPERS ------------------------------------------------------------------


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a


iif : Bool -> a -> a -> a
iif c a b =
    if c then
        a

    else
        b


{-| Two decimal places, rounded rather than truncated. It floored, so `usd`
rendered $1.999 as $1.99 and every total read a cent light. Still a float, so
$1.005 is whatever the double nearest it is -- exact decimal money is its own
job and is blocked on the engine compiling `sum` inline.
-}
round2 : Float -> Float
round2 =
    (*) 100 >> round >> toFloat >> flip (/) 100


{-| Whether a number is written as digits at all. JavaScript writes a magnitude
of 1e21 or more as "1e+21" and one that is not finite as a word, and a string
cut by position then splices that into the middle of the number: a usd total
that overflowed read "$In,fin,ity.00", and 1e11 at ten places read
"0.000001e+21" because the scaled integer had stopped being digits. Neither is a
number a column can dress up, so both are handed back the way Elm writes them.
-}
positional : Float -> Bool
positional value =
    abs value < 1.0e21


{-| A number written at exactly this many decimal places. Built off the whole
part and the scaled fraction rather than off `String.fromFloat`, which drops a
trailing zero and writes a small number in exponent form: 1.5 at three places
has to come back "1.500", and rounding the scaled fraction is also what keeps
binary float error out of the answer -- 0.1 + 0.2 prints as 0.3.

Only the fraction is scaled. `value * 10 ^ places` leaves the digits behind at
magnitudes a column really holds -- 1e11 at the ten places `maxDecimals`
allows -- and the answer was then cut out of "1e+21". The magnitude is what is
scaled, so a half rounds away from zero on both sides of it: `round` sends
-0.125 to -0.12 and 0.125 to 0.13, which is one number reading two ways in one
column, and money already rounded the other way through `usd`.

-}
fixed : Int -> Float -> String
fixed places value =
    let
        magnitude =
            abs value

        whole =
            toFloat (floor magnitude)

        scaled =
            round ((magnitude - whole) * toFloat (10 ^ places))

        -- 0.999 at two places rounds up to 100 hundredths, which is the next
        -- whole number and no fraction left at all.
        carried =
            scaled == 10 ^ places

        units =
            iif carried (whole + 1) whole

        fraction =
            iif carried 0 scaled
    in
    if not (positional value) then
        String.fromFloat value

    else
        -- The sign is taken off the digits that get written and not off the
        -- value: -0.4 at zero places is written "0", and prefixing it gave a num
        -- column of small values a "-0" beside a "0", which reads as two
        -- different numbers.
        iif (value < 0 && (units > 0 || fraction > 0)) "-" ""
            ++ String.fromFloat units
            ++ iif (places == 0) "" ("." ++ (String.fromInt fraction |> String.padLeft places '0'))


commas : String -> String
commas =
    String.reverse
        >> String.toList
        >> List.indexedMap
            (\i c ->
                if i > 0 && modBy 3 i == 0 then
                    [ ',', c ]

                else
                    [ c ]
            )
        >> List.concat
        >> String.fromList
        >> String.reverse


{-| A separator every three digits of the whole part of a number already
written. `commas` counts from the right over everything it is handed, which is
why the sign and the fraction are held back from it: it made "1234.5" into
"1,23,4.5" and "-123" into "-,123".
-}
groupWhole : String -> String
groupWhole written =
    let
        sign =
            iif (String.startsWith "-" written) "-" ""

        unsigned =
            String.dropLeft (String.length sign) written

        whole =
            String.indexes "." unsigned |> List.head |> Maybe.withDefault (String.length unsigned)
    in
    sign ++ commas (String.left whole unsigned) ++ String.dropLeft whole unsigned


{-| Money at a decimal count: the symbol and the digit grouping are what make it
money, and neither depends on how many places are asked for. The grouping is
`groupWhole`, the one a `grouped` column asks for by name: money is the column
that always asked for it. The one value `fixed` answers without digits at all is
not money either, and `formatNumber` hands that one back before it ever reaches
here.
-}
usd : Int -> Float -> String
usd places amount =
    let
        digits =
            fixed places (abs amount)
    in
    -- The sign off the digits that get written and not off the amount, the way
    -- `fixed` takes it: -0.4 at no places is written "0", and a "-$0" beside a
    -- "$0" reads as two different amounts in one column.
    iif (amount < 0 && String.any (\c -> Char.isDigit c && c /= '0') digits) "-" ""
        ++ "$"
        ++ groupWhole digits


{-| How a number is written, beyond how many places it carries. `Grouped` is
the separator every three digits money already puts in; `Scientific` is a
mantissa and a signed exponent.

Neither is a column type. A currency symbol and a percent sign are `usd` and
`percentage`, which a column already declares, so a format says how the digits
read and never what they mean.

-}
type NumberFormat
    = Grouped
    | Scientific


{-| The one table of formats: the word a document stores, and the reading it
asks for, which is the label the column's panel offers. `label` is an example
rather than a description, because the choice is about how a number looks.
-}
formatSpec : NumberFormat -> { name : String, label : String }
formatSpec format =
    case format of
        Grouped ->
            { name = "grouped", label = "1,234.5" }

        Scientific ->
            { name = "scientific", label = "1.2345e3" }


numberFormats : List NumberFormat
numberFormats =
    [ Grouped, Scientific ]


{-| The format a word names, or none at all. A spelling outside the list is a
column nobody formatted rather than an error, the way a decimal count out of
range is a column with no count: the format is how you were reading the numbers,
and losing it must never cost you the numbers.
-}
numberFormat : String -> Maybe NumberFormat
numberFormat name =
    numberFormats |> List.filter (\f -> (formatSpec f).name == name) |> List.head


{-| How a numeric column's values are drawn as a picture of themselves: a
colour scale across the column's own range, or a bar per cell. Not a column
type and not a format -- `formatNumber` is untouched, because a shade is a
background and never text.

The absent case is no constructor, the way it is for a format: a column nobody
shaded is a column with no entry.

-}
type Shade
    = Scale
    | Bars


{-| The one table of shades: the word a document stores, and the label the
column's panel offers. `Bars` is spelled out because `Bar` is already a way a
chart is drawn.
-}
shadeSpec : Shade -> { name : String, label : String }
shadeSpec shading =
    case shading of
        Scale ->
            { name = "scale", label = "colour scale" }

        Bars ->
            { name = "bar", label = "data bars" }


shades : List Shade
shades =
    [ Scale, Bars ]


{-| The shade a word names, or none at all. A spelling outside the list is a
column nobody shaded rather than an error, the way a format nobody wrote is:
the shade is how you were reading the numbers, and losing it must never cost
you the numbers.
-}
shade : String -> Maybe Shade
shade name =
    shades |> List.filter (\s -> (shadeSpec s).name == name) |> List.head


{-| A value with its exponent divided out, `10 ^ exponent` computed as two
smaller powers rather than one. `10 ^ exponent` alone overflows to Infinity
past 308 and underflows to 0 past -323 -- both real exponents a float can
carry -- and a divide by either turns a plain value into `0` or `Infinity`:
the smallest positive float (5e-324, exponent -324) came back "Infinitye-324".
Halving the exponent first keeps each power within a double's normal range for
every exponent a float has, at the cost of the same rounding a divide already
carries.
-}
descaled : Float -> Int -> Float
descaled value exponent =
    let
        half =
            exponent // 2
    in
    value / 10 ^ toFloat half / 10 ^ toFloat (exponent - half)


{-| A number as a mantissa and a signed exponent. Elm has no `toExponential`, so
the exponent is the floor of the base-10 log and the mantissa is what is left
when it is divided out. Zero is answered by name: `logBase 10 0` is -Infinity.

Two digits before the point mean the exponent was one too low, and the mantissa
is written again one exponent higher. That happens two ways and one check
answers both, because the mantissa is in [1, 10) until one of them does: `fixed`
rounds, so 9.99 at one place is "10.0"; and `logBase` divides two logs, so
1000 comes back at an exponent of 2 and a mantissa of exactly 10.

-}
scientific : Maybe Int -> Float -> String
scientific decimals v =
    let
        mantissa exponent =
            case decimals of
                Just places ->
                    fixed places (descaled (abs v) exponent)

                Nothing ->
                    -- No count is "as many digits as the mantissa needs," not
                    -- two places -- a fixed count the way money reads would
                    -- have cut 1.23456789e3 down to 1.23e3. But the divide
                    -- that makes the mantissa is itself a binary float rarely
                    -- exact, and `String.fromFloat` wrote its own noise
                    -- straight into the cell: 1234.56789 read
                    -- "1.2345678900000001e3". A double holds about 15 decimal
                    -- digits before it runs out of precision at all, so
                    -- rounding the mantissa to 12 drops the noise and stops
                    -- short of any digit a typed value could have meant.
                    let
                        clean =
                            1.0e12
                    in
                    String.fromFloat (toFloat (round (descaled (abs v) exponent * clean)) / clean)
    in
    if v == 0 then
        mantissa 0 ++ "e0"

    else
        let
            found =
                floor (logBase 10 (abs v))

            exponent =
                iif (String.startsWith "10" (mantissa found)) (found + 1) found
        in
        -- The sign off the value rather than off the digits, the way `fixed`
        -- takes it off them: a mantissa is never all zeroes, so there is no
        -- "-0" here for it to have to answer for.
        iif (v < 0) "-" "" ++ mantissa exponent ++ "e" ++ String.fromInt exponent


{-| The digits a number is written with, before whatever its type puts around
them: the count the column asked for, and the way it asked them to read. No
count is two places of rounding, which is what a numeric column did before a
count could be asked for at all.
-}
digitsOf : Maybe Int -> Maybe NumberFormat -> Float -> String
digitsOf decimals format v =
    let
        plain =
            case decimals of
                Just places ->
                    fixed places v

                Nothing ->
                    String.fromFloat (round2 v)
    in
    case format of
        Just Grouped ->
            groupWhole plain

        Just Scientific ->
            scientific decimals v

        Nothing ->
            plain


{-| How a number reads in the column it is in. Three places used to answer this
independently -- the cell, the stats row and the totals row -- and they
disagreed: a usd column's cells carried a $ and its total did not, which is a
number that looks like a different number.

A wildcard rather than a table, because the question is only ever asked of a
column that holds a number, and a type that does not hold one has no reading of
its own to state.

The count and the format are the column's own, and their absence is the reading
each type had before anybody could ask: two places for money, as many as the
value needs for the other two, and a separator every three digits where money
already put one. A format lands on top of the type rather than instead of it --
money asked for an exponent keeps its symbol and a percentage keeps its sign --
so `grouped` on a usd column asks for exactly what that column already writes.

-}
formatNumber : Type -> Maybe Int -> Maybe NumberFormat -> Float -> String
formatNumber typ decimals format v =
    if not (positional v) then
        -- Neither a currency symbol nor digit grouping belongs on a value that
        -- is not written as digits: `commas` runs over the string form, so a usd
        -- total that overflowed to Infinity read "$In,fin,ity.00". Two cells of
        -- 1e308 in a usd column is all it takes, and the totals row sums them.
        String.fromFloat v

    else
        case ( typ, format ) of
            ( Usd, Just Scientific ) ->
                iif (v < 0) "-" "" ++ "$" ++ scientific (Just (Maybe.withDefault 2 decimals)) (abs v)

            ( Usd, _ ) ->
                usd (Maybe.withDefault 2 decimals) v

            ( Percentage, _ ) ->
                digitsOf decimals format (v * 100) ++ "%"

            _ ->
                digitsOf decimals format v



---- PORTS --------------------------------------------------------------------


port librarySynced : (D.Value -> msg) -> Sub msg


{-| This browser's own facts about a sheet in its library. A `Nothing` field is
left as it was — src/index.html drops a null out of the patch rather than out of
the entry — so restoring writes `Just False` and never `Nothing`.
-}
port updateLibrary : Idd { name : Maybe String, tags : Maybe (List String), trashed : Maybe Bool, starred : Maybe Bool } -> Cmd msg


port changeId : Id -> Cmd msg


port newDoc : E.Value -> Cmd msg


{-| Copy the sheet with this id into one of your own, keeping the lineage link.
-}
port forkDoc : String -> Cmd msg


{-| Save the chart on screen as "svg" or "png".
-}
port downloadChart : String -> Cmd msg


port deleteDoc : String -> Cmd msg


port changeDoc : Idd (List Patch) -> Cmd msg


{-| The arrangement, on its own wire. Every patch that leaves here is a view
field and nothing else, which is what lets the page keep a viewer's arrangement
in this browser without a second copy of the list of what a view field is.
-}
port arrangeDoc : Idd (List Patch) -> Cmd msg


port queryDoc : Idd { lang : String, code : String, cols : D.Value } -> Cmd msg


port docSelected : (Idd { doc : D.Value } -> msg) -> Sub msg


port docChanged : (Idd DocDelta -> msg) -> Sub msg


port docNotified : (Idd D.Value -> msg) -> Sub msg


port docQueried : (Idd D.Value -> msg) -> Sub msg


port docErrored : (String -> msg) -> Sub msg


{-| The poller's request, once, now: what the sheet would fetch with what it
would send. The answer comes back on `preflightLoaded`, named by sheet.
-}
port preflight : Idd { url : String, headers : String, method : String, body : String } -> Cmd msg


port preflightLoaded : (Idd D.Value -> msg) -> Sub msg


{-| The open sheet's versions, newest first, as automerge records them. The
answer comes back on `historyLoaded`, named by sheet.
-}
port historyLoad : String -> Cmd msg


port historyLoaded : (Idd D.Value -> msg) -> Sub msg


{-| One version's rows, read off the document as it stood at that change. The
answer comes back on `historyShown`, named by sheet and by version.
-}
port historyView : { id : String, hash : String } -> Cmd msg


port historyShown : (Idd D.Value -> msg) -> Sub msg


{-| The sheet's own poll, now rather than on its timer: the row it writes comes
back on `runLoaded`, named by sheet the way a pre-flight is.
-}
port runNow : String -> Cmd msg


port runLoaded : (Idd D.Value -> msg) -> Sub msg


port signup : String -> Cmd msg


port login : { email : String, password : String } -> Cmd msg


port logout : () -> Cmd msg


port importCsv : { filename : String, content : String } -> Cmd msg


{-| How the server reads the chosen file, with the types this browser settled
on for this header last time laid over its guesses.
-}
port importPreviewed : (D.Value -> msg) -> Sub msg


{-| The types the user settled on for the file the preview was of.
-}
port importConfirm : { types : List ( String, String ) } -> Cmd msg


port authResult : (D.Value -> msg) -> Sub msg


port copyToClipboard : String -> Cmd msg


port pasteFromClipboard : (String -> msg) -> Sub msg


port requestCopy : (() -> msg) -> Sub msg


port queryEditorState : ({ cursorPos : Int, textBeforeCursor : String } -> msg) -> Sub msg


{-| What columns one referenced sheet has. Asked for when the editor sees a dot
after a ref, answered by `describe @<ref>` through the page's own engine -- the
same read the statement answers with, so a suggestion cannot disagree with the
schema the query will then be checked against.

The columns cannot be read in Elm: the sheets live in the `sheets()` closure in
src/page.mjs behind the automerge repo, which is the whole reason this is a port.

-}
port columnsFor : String -> Cmd msg


port columnsLoaded : (Idd (List String) -> msg) -> Sub msg


port insertAtCursor : String -> Cmd msg


port saveTutorial : Int -> Cmd msg


{-| One row of `library:freshness` per sheet whose runs are written down: a
polled feed, a webhook, an alert. It arrives through a port rather than Elm's
own Http for the reason sharing does: the JWT lives in index.html. An anonymous
visitor is sent nothing at all, which is why the library's column is absent
rather than a row of blanks that would read as "nothing is wrong".
-}
port freshnessLoaded : (D.Value -> msg) -> Sub msg


{-| Which sheets read this one, asked for when a rename or a delete would break
them. The answer is `library:lineage` whole, or why it could not be read, and it
names the sheet it was asked about the way a pre-flight does.
-}
port lineageFor : String -> Cmd msg


port lineageLoaded : (D.Value -> msg) -> Sub msg


{-| Sharing runs through JS because the JWT lives there, same as changeDoc.
`action` is one of list, add, remove, public, link, hook. The answer comes back
carrying the same `id` and `action`, so ShareLoad can tell an answer about this
sheet from one about the sheet you just left.
-}
port shareAction : ShareAsk -> Cmd msg


{-| What `shareAction` carries. One shape for six actions, so most of it is
unused at any one call site: `shareAsk` is the whole of it doing nothing, and
each caller is written as the difference from that.
-}
type alias ShareAsk =
    { id : String
    , action : String
    , email : String
    , role : String
    , public : Bool
    , personal : Bool
    , days : Int
    , password : String
    }


shareAsk : ShareAsk
shareAsk =
    { id = "", action = "", email = "", role = "", public = False, personal = False, days = 0, password = "" }


port shareLoaded : (D.Value -> msg) -> Sub msg


type alias DocDelta =
    { doc : D.Value
    , handle : D.Value
    , patchInfo : D.Value
    , patches : List D.Value
    }


type alias Idd a =
    { id : Id
    , data : a
    }


type alias Patch =
    { action : String
    , path : List D.Value
    , value : D.Value
    }



---- MAIN ---------------------------------------------------------------------


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClick
        }



---- MODEL --------------------------------------------------------------------


type alias Id =
    String


type alias Library =
    Dict Id SheetInfo


type alias Model =
    { nav : Nav.Key
    , api : String
    , id : String
    , search : String
    , error : String
    , library : Library
    , sheet : Sheet
    , auth : Auth
    , deleteConfirm : Maybe String
    , pending : Maybe Pending

    -- Whether the library is showing what was thrown away instead of what was kept.
    , trash : Bool

    -- The file being imported, once the server has said how it reads it.
    , importing : Maybe Importing
    , showSettings : Bool
    , share : Share
    , showShortcuts : Bool
    , palette : Maybe Palette

    -- A past version of the open sheet. Its own field and never `sheet`: every
    -- view of a `Sheet` is wired to writes, and this one is read and nothing else.
    , history : Maybe History
    , freshness : Dict Id Freshness
    , tutorial : Maybe Int
    , embed : Bool

    -- The page's own clock, in epoch milliseconds: taken at boot and again
    -- whenever a snooze is set. Nothing renders it -- it is only ever compared
    -- with a timestamp the document holds, which is a question no pure view can
    -- answer on its own.
    , now : Int
    }


{-| What `library:freshness` says about one sheet: when it last ran, and how
many runs since its last good one. A sheet with no entry has no freshness at
all, which is a different fact from zero failures -- a table or a query has no
runs to be stale, and the read answers for nothing that has none.
-}
type alias Freshness =
    { lastRun : Maybe String, failures : Int, nextRun : Maybe String }


{-| A rename or a delete held back until `library:lineage` says who reads the
columns it touches. `write` is the header editor's text, which closes on hold.
-}
type alias Pending =
    { id : Id
    , edit : DocMsg
    , write : Maybe String
    , columns : List String
    , stage : Stage
    }


{-| `Warning` carries each dependent's name and why it is at risk, or why
nobody could say. `Confirmed` lives for one `updateDocMsg` call: the replay
that writes the held edit.
-}
type Stage
    = Asking
    | Warning (Result String (List ( String, String )))
    | Confirmed


{-| The command palette: what has been typed, and which match is selected.
Nothing is closed.
-}
type alias Palette =
    { query : String, selected : Int }


{-| The versions of one sheet, and the one of them on screen. `left` is how many
older versions the answer left out. A `Nothing` is an answer still in flight.
-}
type alias History =
    { id : Id
    , versions : Maybe (Result String (List Version))
    , left : Int
    , hash : Maybe String
    , past : Maybe (Result String { columns : List String, rows : List (List String) })
    }


{-| One change, in automerge's own words. `time` is epoch seconds, and 0 when
the writer stamped none.
-}
type alias Version =
    { hash : String, time : Int, actor : String, seq : Int, message : Maybe String }


type alias Auth =
    { state : AuthState
    , email : String
    , password : String
    }


type AuthState
    = Anonymous
    | LoggingIn
    | LoggedIn { usrId : String }


type alias SheetInfo =
    { name : String
    , tags : List String
    , scratch : Bool
    , system : Bool
    , thumb : D.Value
    , seen : String -- ISO 8601, when this browser last opened it; "" if never
    , trashed : Bool -- whether this browser put it in the trash
    , starred : Bool -- whether this browser keeps it at the top of the library
    }


{-| Who a sheet is shared with, as last loaded from the server. `link` holds a
freshly minted view-only URL, and `hook` a net sheet's signing secret. Both are
shown only after the owner asks for them: a secret nobody asked for is a secret
on somebody's screen share.
-}
type alias Share =
    { members : List Member
    , public : Bool

    -- What the publisher says about the personal data in the sheet. The server
    -- refuses a publish over an email address, a phone number, an SSN or a card
    -- number unless this rides with it, so it is a claim and never a guess.
    , personal : Bool
    , link : Maybe String
    , hook : Maybe Hook
    , email : String
    , role : String

    -- What the next minted link is asked for. Both blank by default, and blank
    -- means "leave it out": the server's own default is a thirty-day link
    -- anyone holding the url can open, and an empty body is what asks for it.
    , days : String
    , password : String
    }


type alias Member =
    { email : String, role : String }


type alias Hook =
    { url : String, secret : String, repro : String }


emptyShare : Share
emptyShare =
    { members = []
    , public = False
    , personal = False
    , link = Nothing
    , hook = Nothing
    , email = ""
    , role = "viewer"
    , days = ""
    , password = ""
    }


type alias Sheet =
    { id : Id
    , select : Rect
    , hover : Index
    , drag : Bool
    , write : Maybe String
    , doc : Result String Doc
    , table : Result String Table
    , stats : Result String (Array Stat)
    , hidden : Set String
    , pinned : Set String
    , sort : List ( String, SortOrder )
    , filters : Dict String Filter
    , filterOpen : Maybe String
    , findReplace : Maybe FindReplace
    , queryAutocomplete : Maybe QueryAutocomplete
    , queryColumns : Dict String (List String)
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , netStatus : Maybe String

    -- What the feed's request answered when it was last tested, by sheet.
    , preflight : Maybe (Result String Preview)

    -- What the sheet's last run-now landed, one line of it, by sheet.
    , run : Maybe (Result String String)
    , widths : Dict String Int

    -- How many decimal places this browser writes a numeric column's values at,
    -- where it asked for a count at all.
    , decimals : Dict String Int

    -- How the digits of a numeric column read, where it asked for a reading
    -- other than the one its type has.
    , formats : Dict String NumberFormat

    -- How a numeric column's values are drawn as a picture of themselves,
    -- where it asked to be drawn as one at all.
    , shades : Dict String Shade

    -- What the open column panel's split box holds. One box, because one panel
    -- is open at a time, and this browser's own: it is the argument a split is
    -- about to be run with, never anything the document keeps.
    , splitOn : String
    , near : String

    -- What the library strip's tag box holds, for the same reason the two above
    -- are held: it is the argument `TagSelected` is about to be run with, and
    -- nothing about it belongs to a document.
    , tag : String
    , resizing : Maybe { key : String, startX : Int, startWidth : Int }

    -- The column or row being dragged to a new position, while the drag lasts.
    -- What is under the pointer is `hover`, which every cell already reports.
    , moving : Maybe Moving

    -- What this browser has written to the document, plus what the document
    -- held when it was opened. Every write is diffed against it, so closing a
    -- filter panel that changed nothing writes nothing, and moving one sort key
    -- rewrites one column.
    --
    -- Not re-read from the document as it changes, on purpose. A collaborator's
    -- sort would land in here, the next diff would see it go from theirs to the
    -- nothing on my screen, and I would write a deletion over their sort. What
    -- I did not touch, I do not write.
    , storedView : SheetView
    , lineage : Maybe String
    }


emptySheet : Sheet
emptySheet =
    { id = ""
    , select = Rect (xy -1 -1) (xy -1 -1)
    , hover = xy -1 -1
    , drag = False
    , write = Nothing
    , doc = Err ""
    , table = Err ""
    , stats = Err ""
    , hidden = Set.empty
    , pinned = Set.empty
    , sort = []
    , filters = Dict.empty
    , filterOpen = Nothing
    , findReplace = Nothing
    , queryAutocomplete = Nothing
    , queryColumns = Dict.empty
    , undoStack = []
    , redoStack = []
    , netStatus = Nothing
    , preflight = Nothing
    , run = Nothing
    , lineage = Nothing
    , widths = Dict.empty
    , decimals = Dict.empty
    , formats = Dict.empty
    , shades = Dict.empty
    , splitOn = ""
    , near = ""
    , tag = ""
    , resizing = Nothing
    , moving = Nothing
    , storedView = emptyView
    }


type alias Preview =
    { status : Int
    , ms : Int
    , bytes : Int
    , contentType : String
    , body : String
    }


{-| A response is the preview; a failure off the wire arrives as the poller's
own failure record, and its `error` is the whole of it.
-}
previewDecoder : D.Decoder (Result String Preview)
previewDecoder =
    D.oneOf
        [ D.map Err (D.field "error" D.string)
        , D.map Ok
            (D.map5 Preview
                (D.field "status" D.int)
                (D.field "ms" D.int)
                (D.field "bytes" D.int)
                (D.field "content_type" D.string)
                (D.field "body" D.string)
            )
        ]


{-| One line about the run that just happened, out of the `net` row the server
answers with. A refusal arrives the way a pre-flight's does, as an `error` the
page shows whole.
-}
runDecoder : D.Decoder (Result String String)
runDecoder =
    D.oneOf
        [ D.map Err (D.field "error" D.string)
        , D.map Ok
            (D.field "method" D.string
                |> D.andThen
                    (\method ->
                        D.map2 (\at line -> String.left 19 (String.replace "T" " " at) ++ " · " ++ method ++ " · " ++ line)
                            (D.field "created_at" D.string)
                            (runLine method)
                    )
            )
        ]


{-| What the run amounts to, in the words its own row already uses: an alert's
body is the verdict and what was done about it, and a feed's meta is the status
the poll came back with. Picked by `method` -- "ALERT" or an HTTP verb -- and
never guessed by trying one shape and falling back, because a feed's own body
is arbitrary JSON and may itself hold "status" and "delivery" keys, which read
as the alert verdict those two words mean on an alert's row.

An alert's row says what was delivered under `delivery`, except the one whose
`status` is "error": that run never reached a delivery and carries the message
instead. Dispatched on the status for the same reason the shape above is
dispatched on the method.

-}
runLine : String -> D.Decoder String
runLine method =
    if method == "ALERT" then
        D.field "body" D.string
            |> D.andThen
                (\raw ->
                    case D.decodeString (D.field "status" D.string |> D.andThen (\status -> D.map (\rest -> status ++ " · " ++ rest) (D.field (iif (status == "error") "error" "delivery") D.string))) raw of
                        Ok line ->
                            D.succeed line

                        Err err ->
                            D.fail (D.errorToString err)
                )

    else
        D.map2 (\status ms -> "HTTP " ++ String.fromInt status ++ " · " ++ String.fromInt ms ++ " ms")
            (D.at [ "meta", "status" ] D.int)
            (D.at [ "meta", "ms" ] D.int)


type alias Importing =
    { filename : String
    , name : String
    , cols : List { name : String, typ : String, remembered : Bool }
    , rows : List (List String)
    , count : Int
    }


importingDecoder : D.Decoder Importing
importingDecoder =
    D.map5 Importing
        (D.field "filename" D.string)
        (D.field "name" D.string)
        (D.field "cols"
            (D.list
                (D.map3 (\name typ remembered -> { name = name, typ = typ, remembered = remembered })
                    (D.field "name" D.string)
                    (D.field "type" D.string)
                    (D.field "remembered" D.bool)
                )
            )
        )
        (D.field "rows" (D.list (D.map Dict.values (D.dict previewCell))))
        (D.field "count" D.int)


{-| A preview cell as text, whatever the server typed it as.
-}
previewCell : D.Decoder String
previewCell =
    D.oneOf
        [ D.string
        , D.map String.fromFloat D.float
        , D.map (\b -> iif b "true" "false") D.bool
        , D.null ""
        ]


type alias UndoEntry =
    { forward : List Patch -- Patches to redo
    , backward : List Patch -- Patches to undo (inverse)
    }


{-| What a drag carries: a column by key, or a row by its document index. A row
is only ever picked up while the table is in document order, which is what lets
the index be the document's rather than the display's.
-}
type Moving
    = MovingCol String
    | MovingRow Int


type alias QueryAutocomplete =
    { trigger : String -- The text that triggered autocomplete (e.g., "@tab")
    , suggestions : List String -- Matching sheet IDs
    , selectedIndex : Int -- Currently highlighted suggestion
    }


type alias FindReplace =
    { findText : String
    , replaceText : String
    , showReplace : Bool
    , matches : List Index
    , currentMatch : Int
    }


type Filter
    = TextContains String


{-| How a sheet was last being looked at: which columns are hidden, how it is
sorted, what is filtered, and how wide each column was dragged.

Stored on the columns themselves, in `data[0]`, for two reasons. `applyPatches`
in `src/index.html` is rooted at `data`, so a key beside it cannot be written at
all; and hidden, width, sort and filter are each a fact about one column anyway.
It lives in the document rather than in this browser so that it travels with a
share, which is the whole point of remembering it.

One view per sheet, not a named set of them: a second view is a setting, and a
setting earns its place after both values are wanted in real use.

-}
type alias SheetView =
    { hidden : Set String
    , pinned : Set String
    , sort : List ( String, SortOrder )
    , filters : Dict String Filter
    , widths : Dict String Int
    , decimals : Dict String Int
    , formats : Dict String NumberFormat
    , shades : Dict String Shade
    }


emptyView : SheetView
emptyView =
    { hidden = Set.empty, pinned = Set.empty, sort = [], filters = Dict.empty, widths = Dict.empty, decimals = Dict.empty, formats = Dict.empty, shades = Dict.empty }


{-| One column's share of the view. Every field is optional and every default is
the absence of the thing, so a document written before any of this existed reads
as a sheet nobody has arranged yet.
-}
type alias ColView =
    { key : String
    , hidden : Bool
    , pinned : Bool
    , sort : Maybe SortOrder
    , rank : Int
    , filter : String
    , width : Maybe Int
    , decimals : Maybe Int
    , format : Maybe NumberFormat
    , shade : Maybe Shade
    }


{-| Narrower than this and a column cannot be grabbed to widen it again, which
is why the drag clamps here too. A width the document carries has never been
through that drag -- a collaborator, a share or an API write can put any integer
there -- so an unusable one is read as no width at all, the way an unusable sort
is read as no sort.
-}
minColWidth : Int
minColWidth =
    32


{-| What a column that sizes itself renders at, near enough. A resize drag has
to start somewhere, and a pinned column's left edge is a sum of widths that has
to be exact -- so pinning a self-sizing column fixes its width at this.
-}
autoColWidth : Int
autoColWidth =
    140


{-| The most decimal places a column may ask its numbers to be written at. A
count is a document field anybody may write, and `fixed` scales by `10 ^ places`
before it rounds: past a float's own precision the extra digits are noise, and a
big enough exponent is Infinity. An unusable count is read as no count at all,
the way an unusable width is.
-}
maxDecimals : Int
maxDecimals =
    10


{-| One column's view fields, under a key its home decides rather than one
written beside them. A table's columns carry their own key; a query has no
stored columns at all, so the map key is the only key there is.

`D.map8` is the ceiling, so the ninth field is read by handing the record the
`map8` half built one more argument. A tenth needs the same again.

-}
colViewFields : String -> D.Decoder ColView
colViewFields key =
    D.map8 (ColView key)
        (D.oneOf [ D.field "hidden" D.bool, D.succeed False ])
        (D.oneOf [ D.field "pinned" D.bool, D.succeed False ])
        (D.oneOf
            [ D.field "sort" D.string
                |> D.map
                    (\order ->
                        case order of
                            "asc" ->
                                Just Ascending

                            "desc" ->
                                Just Descending

                            _ ->
                                Nothing
                    )
            , D.succeed Nothing
            ]
        )
        (D.oneOf [ D.field "rank" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "filter" D.string, D.succeed "" ])
        (D.oneOf [ D.field "width" (D.map (\w -> iif (w >= minColWidth) (Just w) Nothing) D.int), D.succeed Nothing ])
        (D.oneOf [ D.field "decimals" (D.map (\d -> iif (d >= 0 && d <= maxDecimals) (Just d) Nothing) D.int), D.succeed Nothing ])
        (D.oneOf [ D.field "format" (D.map numberFormat D.string), D.succeed Nothing ])
        |> D.andThen (\make -> D.map make (D.oneOf [ D.field "shade" (D.map shade D.string), D.succeed Nothing ]))


colViewDecoder : D.Decoder ColView
colViewDecoder =
    -- The key `colDecoder` will give this column, not the one written on it.
    -- They differ: a column whose name is a JSON number falls back to a key
    -- of "" there and read as "1" here, so the arrangement was stored under
    -- a key the rendered table never used and `pruneView` then deleted it.
    D.map .key colDecoder |> D.andThen colViewFields


colViewAt : ( String, D.Value ) -> Maybe ColView
colViewAt ( key, value ) =
    D.decodeValue (colViewFields key) value |> Result.toMaybe


{-| The view a document carries, or none. A sheet with no `data` -- the library,
the shop -- has no columns to carry one, and a shape this cannot read is a sheet
nobody arranged rather than an error: the view is how you were looking at the
rows, and losing it must never cost you the rows.

Two homes, because `data[0]` has two shapes. A table's is the column list, and
each column carries its own share. A query's is one object -- `lang`, `code`,
`cols` -- because its rows and its headers are both computed, so the view lives
in a `view` map beside `cols`, keyed the way `cols` is: by the column's name.
The `view` branch goes first; a table's `data[0]` is a list, so `D.field "view"`
fails there and falls through.

-}
viewDecoder : D.Decoder SheetView
viewDecoder =
    D.oneOf
        [ D.field "data" (D.index 0 (D.field "view" (D.keyValuePairs D.value)))
            |> D.map (List.filterMap colViewAt >> viewOf)
        , D.field "data" (D.index 0 (D.oneOf [ D.list colViewDecoder, D.dict colViewDecoder |> D.map Dict.values ]))
            |> D.map viewOf
        , D.succeed emptyView
        ]


viewOf : List ColView -> SheetView
viewOf cols =
    { hidden = cols |> List.filter .hidden |> List.map .key |> Set.fromList
    , pinned = cols |> List.filter .pinned |> List.map .key |> Set.fromList
    , sort =
        cols
            |> List.filterMap (\c -> Maybe.map (\order -> ( c.rank, ( c.key, order ) )) c.sort)
            |> List.sortBy Tuple.first
            |> List.map Tuple.second
    , filters =
        cols
            |> List.filterMap (\c -> iif (String.isEmpty c.filter) Nothing (Just ( c.key, TextContains c.filter )))
            |> Dict.fromList
    , widths = cols |> List.filterMap (\c -> Maybe.map (Tuple.pair c.key) c.width) |> Dict.fromList
    , decimals = cols |> List.filterMap (\c -> Maybe.map (Tuple.pair c.key) c.decimals) |> Dict.fromList
    , formats = cols |> List.filterMap (\c -> Maybe.map (Tuple.pair c.key) c.format) |> Dict.fromList
    , shades = cols |> List.filterMap (\c -> Maybe.map (Tuple.pair c.key) c.shade) |> Dict.fromList
    }


{-| Drop the arrangement of every column the document no longer has.

An arrangement belongs to a column, so it goes when the column does. Kept, it
came back wrong twice: an undo restored the sort rank the column had when it was
deleted, colliding with a rank written since; and `SheetColumnPush` keys a new
column by the column count, so deleting the last column and adding one inherited
its hidden flag and its sort.

Runs where the document is re-read rather than where a column is deleted, so a
collaborator's delete is pruned too. It only ever removes, so it cannot overwrite
a filter somebody is in the middle of typing -- which is why the arrangement is
otherwise read on open and not on every change.

A table only. A query's columns are whatever its last run returned, and they
come and go on every keystroke while its SQL is being edited -- pruning there
would drop your sort the moment a column momentarily stopped existing. Nothing
is written for a column the query no longer returns anyway, because
`viewPatches` walks the columns it does, and the entry comes back if the column
does.

-}
pruneView : Result String Doc -> Sheet -> Sheet
pruneView doc sheet =
    case doc of
        Ok (Tab tbl) ->
            let
                live =
                    tbl.cols |> Array.toList |> List.map .key |> Set.fromList

                keep arrangement =
                    { hidden = Set.filter (\key -> Set.member key live) arrangement.hidden
                    , pinned = Set.filter (\key -> Set.member key live) arrangement.pinned
                    , sort = List.filter (\( key, _ ) -> Set.member key live) arrangement.sort
                    , filters = Dict.filter (\key _ -> Set.member key live) arrangement.filters
                    , widths = Dict.filter (\key _ -> Set.member key live) arrangement.widths
                    , decimals = Dict.filter (\key _ -> Set.member key live) arrangement.decimals
                    , formats = Dict.filter (\key _ -> Set.member key live) arrangement.formats
                    , shades = Dict.filter (\key _ -> Set.member key live) arrangement.shades
                    }

                onScreen =
                    keep (onScreenView sheet)
            in
            { sheet
                | hidden = onScreen.hidden
                , pinned = onScreen.pinned
                , sort = onScreen.sort
                , filters = onScreen.filters
                , widths = onScreen.widths
                , decimals = onScreen.decimals
                , formats = onScreen.formats
                , shades = onScreen.shades
                , storedView = keep sheet.storedView
            }

        _ ->
            sheet


{-| What changed between the arrangement the document holds and the one on
screen, and nothing else. A diff rather than a rewrite for two reasons: moving
one sort key must not rewrite forty columns, and closing a filter panel that
changed nothing must write nothing at all.

Walks the columns by position, so a filter left over from a column that has
since been deleted is written nowhere, and two columns sharing a key are written
once each rather than once per pair.

A field that goes back to its default is deleted rather than set: a `false`
hidden and a `null` width are noise in a document somebody may read.

-}
viewPatches : (Int -> String -> List D.Value) -> Array Col -> SheetView -> SheetView -> List Patch
viewPatches at cols before after =
    let
        ranked arrangement =
            arrangement.sort |> List.indexedMap (\i ( k, order ) -> ( k, ( i + 1, order ) )) |> Dict.fromList

        ( wasSorted, nowSorted ) =
            ( ranked before, ranked after )

        text_ filters key =
            case Dict.get key filters of
                Just (TextContains t) ->
                    Just t

                Nothing ->
                    Nothing

        set x key field value =
            [ { action = iif (value == Nothing) "del" "set"
              , path = at x key ++ [ E.string field ]
              , value = Maybe.withDefault E.null value
              }
            ]

        only was now patches =
            iif (was == now) [] patches
    in
    cols
        |> Array.toIndexedList
        |> List.concatMap
            (\( x, col ) ->
                let
                    key =
                        col.key
                in
                List.concat
                    [ only (Set.member key before.hidden) (Set.member key after.hidden) <|
                        set x key "hidden" (iif (Set.member key after.hidden) (Just (E.bool True)) Nothing)
                    , only (Set.member key before.pinned) (Set.member key after.pinned) <|
                        set x key "pinned" (iif (Set.member key after.pinned) (Just (E.bool True)) Nothing)
                    , only (Dict.get key before.widths) (Dict.get key after.widths) <|
                        set x key "width" (Maybe.map E.int (Dict.get key after.widths))
                    , only (Dict.get key before.decimals) (Dict.get key after.decimals) <|
                        set x key "decimals" (Maybe.map E.int (Dict.get key after.decimals))
                    , only (Dict.get key before.formats) (Dict.get key after.formats) <|
                        set x key "format" (Maybe.map (formatSpec >> .name >> E.string) (Dict.get key after.formats))
                    , only (Dict.get key before.shades) (Dict.get key after.shades) <|
                        set x key "shade" (Maybe.map (shadeSpec >> .name >> E.string) (Dict.get key after.shades))
                    , only (text_ before.filters key) (text_ after.filters key) <|
                        set x key "filter" (Maybe.map E.string (text_ after.filters key))
                    , only (Dict.get key wasSorted) (Dict.get key nowSorted) <|
                        case Dict.get key nowSorted of
                            Just ( rank, order ) ->
                                set x key "sort" (Just (E.string (iif (order == Ascending) "asc" "desc")))
                                    ++ set x key "rank" (Just (E.int rank))

                            Nothing ->
                                set x key "sort" Nothing ++ set x key "rank" Nothing
                    ]
            )


{-| A value moved within a list: a column within `data[0]` at `[ 0 ]`, a row
within `data` at `[]`.

One patch, not a splice out and a splice back in: the value re-inserted has to
be the object the document already holds. `Col` carries only key, name and
type, so a column rebuilt here would arrive stripped of its own arrangement, and
`Row` carries only the cells. `applyPatches` in src/index.html moves the value
in place.

-}
movePatch : List Int -> Int -> Int -> Patch
movePatch path from to =
    { action = "move", path = List.map E.int path, value = E.list E.int [ from, to ] }


{-| Whether the rows on screen are the document's rows in the document's order:
nothing sorted, and nothing filtered or searched away. Only then is a display
row a document row, which is what a row drag needs at both ends -- the handle
is withheld without it, and a drop after a sort taken mid-drag is refused.
-}
inDocumentOrder : String -> Sheet -> Array Row -> Bool
inDocumentOrder search sheet rows =
    List.isEmpty sheet.sort && Array.length (filterAndSort search sheet rows) == Array.length rows


{-| Where a drag lands when the pointer lets go, or nowhere.

The column under the pointer is `hover.x`: every cell reports it already, so a
drag needs no tracking of its own. A move is a splice on `data[0]` rather than
a display permutation -- rows are keyed by `col.key`, so no cell moves and the
display index stays the document index, which is what keeps every selection
index in this file right without a display-to-document map. It travels alone
in its batch: every other view write is addressed by a position this one
changes. It is a document edit rather than an arrangement -- everyone looking
at the sheet sees the new order -- so it goes out on `changeDoc` with an undo
entry beside it, and a viewer's is refused the way any other edit of theirs is.

A row, likewise, in document indices: the handle is drawn only while the rows
on screen are the document's rows in the document's order, so `hover.y` is the
document row. Let go on a header row or off the table and nothing moves.

Let go anywhere but over a target and the thing goes nowhere. `hover` is -1 off
the table, and a drop that guessed 0 from that moved a column somebody was only
putting down.

A table only. A query's columns are its result's, in the order its select list
put them, and moving one means editing the query; its rows are computed.

-}
dropOf : Moving -> Index -> Table -> Maybe DocMsg
dropOf moving hover tbl =
    let
        onto edit from to =
            iif (from == to) Nothing (Just (edit from to))
    in
    case moving of
        MovingCol key ->
            Maybe.map2 (onto SheetColumnMove)
                (tbl.cols |> Array.toIndexedList |> List.filter (\( _, c ) -> c.key == key) |> List.head |> Maybe.map Tuple.first)
                (iif (hover.x < 0) Nothing (Just (min hover.x (Array.length tbl.cols - 1))))
                |> Maybe.andThen identity

        MovingRow from ->
            -- A row picked up before a collaborator deleted it is no row at all.
            iif (hover.y < 1 || from < 1 || from > Array.length tbl.rows) Nothing (Just (min hover.y (Array.length tbl.rows)))
                |> Maybe.andThen (onto SheetRowMove from)


{-| The arrangement as it stands on screen: the half of a `Sheet` that is a
`SheetView`.
-}
onScreenView : Sheet -> SheetView
onScreenView sheet =
    { hidden = sheet.hidden
    , pinned = sheet.pinned
    , sort = sheet.sort
    , filters = sheet.filters
    , widths = sheet.widths
    , decimals = sheet.decimals
    , formats = sheet.formats
    , shades = sheet.shades
    }


{-| Where one column's view fields live in `data[0]`. A table's columns are the
list itself, so the address is the position. A query has no stored columns to
write on -- its rows and its headers are both computed -- so they live under
`view`, keyed by the column's name the way its `cols` overrides are.
-}
tableHome : Int -> String -> List D.Value
tableHome x _ =
    [ E.int 0, E.string (String.fromInt x) ]


queryHome : Int -> String -> List D.Value
queryHome _ key =
    [ E.int 0, E.string "view", E.string key ]


{-| The columns an arrangement may name, and where each one's fields go. One
`case` rather than two, so a sheet type cannot be arrangeable in one direction
and not the other. A query's columns are whatever its last run returned -- the
same `Col` record through the same decoder, just held beside the document
instead of inside it.
-}
arrangeable : Sheet -> Maybe { cols : Array Col, home : Int -> String -> List D.Value }
arrangeable sheet =
    case sheet.doc of
        Ok (Tab tbl) ->
            Just { cols = tbl.cols, home = tableHome }

        Ok (Query _) ->
            sheet.table |> Result.toMaybe |> Maybe.map (\tbl -> { cols = tbl.cols, home = queryHome })

        _ ->
            Nothing


{-| Whether the column controls -- sort, filter, hide, pin, resize -- are offered
on this sheet at all.

Anything `arrangeable` answers for, because there the arrangement is kept. Plus
the library and the shop, which are listings this app builds rather than sheets:
their order is a way of reading the list rather than a fact about it, there is no
document under them to keep it in, and nobody expects a reload to hold it.

Everything else is a feed. Its rows are the log of what happened to the sheet,
`arrange` has nowhere to put an arrangement, and a control that works and then
forgets on reload reads as a bug in saving rather than as a sheet with no columns
of its own. A control that is absent explains itself.

-}
arrangeControls : Sheet -> Bool
arrangeControls sheet =
    case ( arrangeable sheet, sheet.doc ) of
        ( Just _, _ ) ->
            True

        ( Nothing, Ok Library ) ->
            True

        ( Nothing, Ok Shop ) ->
            True

        _ ->
            False


{-| Move to a sheet arranged this way, and store the arrangement on its columns.

Deliberately not routed through `updateDocMsg`: dragging a column wider is not
an edit to the data, and does not belong on the undo stack beside one. It goes
out through `arrangeDoc` rather than `changeDoc` for the same reason from the
other side -- the page then knows a batch is only ever view fields, without
having to keep a second copy of what those are.

`storedView` moves to `after` here, before the page has done anything with the
patches, and **the page must never drop a batch** -- it is written to hold that
up: the document first and the browser store last, a store that refuses keeps
what it was given in memory, and a missing automerge handle is a named refusal
rather than a no-op. There is no acknowledgement to wait for and no correct
value to roll back to: this is what _I_ wrote, not what the document holds, and
re-reading the document to recover it is what would write a deletion over a
collaborator's sort.

-}
arrange : Model -> Sheet -> ( Model, Cmd Msg )
arrange model next =
    let
        after =
            onScreenView next
    in
    case arrangeable next of
        Just target ->
            ( { model | sheet = { next | storedView = after } }
            , case viewPatches target.home target.cols next.storedView after of
                [] ->
                    Cmd.none

                patches ->
                    arrangeDoc { id = next.id, data = patches }
            )

        Nothing ->
            ( { model | sheet = next }, Cmd.none )


type Stat
    = Numeric
        { histogram : Dict String Int
        , count : Int
        , sum : Float
        , min : Maybe Float
        , max : Maybe Float
        }
    | Enumerative
    | Descriptive
        { lengths : Dict Int Int
        , keywords : Dict String Int
        , count : Int
        , sum : Int
        , min : Maybe Int
        , max : Int
        }
    | Temporal
        { days : Set Int
        , count : Int
        , first : Maybe String
        , last : Maybe String
        }
    | Boolish
        { true : Int
        , false : Int
        , blank : Int
        }


type Doc
    = Library
    | Shop
    | Tab Table
    | Query Query_
    | NetHook
    | NetHttp { url : String, interval : Int, headers : String, method : String, body : String, pageBy : String, pageParam : String, pagePath : String, mode : String, key : String, rowsPath : String, paused : Bool, cron : String, timezone : String }
    | Alert { code : String, to : String, interval : Int, digest : Bool, when : When, paused : Bool, snoozedUntil : String, cron : String, timezone : String }
    | Chart Chart_
    | Dashboard (List String)
    | NetSocket { url : String }
    | Unviewable String


type alias Table =
    { cols : Array Col
    , rows : Array Row
    }


type alias Query_ =
    { lang : String
    , code : String
    , args : Args
    , examples : List String
    , cols : D.Value
    }


{-| A chart's settings, named the way `Query_` is because four readers spell
them -- the doc, the decoder, `viewChart` and `viewChartSettings` -- and seven
fields written out four times is four places a new one can be forgotten.

`y2` is a second column drawn against a scale of its own; blank is the one scale
every chart had before there were two. `annotations` are the moments marked on
the axis, `( at, label )` each, and they are drawn only where the axis is time.

-}
type alias Chart_ =
    { source : String
    , kind : ChartKind
    , x : String
    , y : String
    , y2 : String
    , series : String
    , annotations : List ( String, String )
    }


type alias Col =
    { key : String
    , name : String
    , typ : Type

    -- The document's own spelling of the type, the way `key` is the document's
    -- own spelling of the column. Kept because the app must never rewrite it: a
    -- rename used to re-encode the type beside the name, which turned `int`
    -- into `num`, `percentage` into `pct` and `float` into a type the decoder
    -- did not know. Nothing normalizes a spelling now -- it is displayed as
    -- written, edited as written and written back as written.
    , raw : String
    }


type alias Row =
    Dict String D.Value


type alias Args =
    Dict String Type


type Type
    = Unknown
    | Text
    | Number
    | Usd
    | Boolean
    | Percentage
    | Date
    | Many Type
    | Link
    | SheetId
    | Json
    | Timestamp
    | Image
    | Delete
    | Trash
    | Restore
    | Star
    | Create
    | Form
    | Enum (List String)
    | Thumb


{-| What an alert asks of its query's answer. One table with no wildcard, so a
new constructor fails to compile here; `main.ts` keeps the same list as
`ALERT_WHEN`, and the name is the one both spell on the document.
-}
type When
    = OnRows
    | OnAdded
    | OnRemoved


whenSpec : When -> { name : String, label : String }
whenSpec when =
    case when of
        OnRows ->
            { name = "rows", label = "the query returns a row" }

        OnAdded ->
            { name = "added", label = "a row is new since the run before" }

        OnRemoved ->
            { name = "removed", label = "a row has left since the run before" }


whens : List When
whens =
    [ OnRows, OnAdded, OnRemoved ]


{-| A field a document may leave out, but may not spell as something else.
`D.oneOf [ D.field name inner, D.succeed fallback ]` falls through to the
fallback whenever the field is present and the inner decoder fails, which paints
a document the server refuses as though it held the fallback it does not: a
select reading GET over a `"method": 5` the poller will not poll.
-}
optionalField : String -> D.Decoder a -> a -> D.Decoder a
optionalField name inner fallback =
    D.maybe (D.field name D.value)
        |> D.andThen
            (\present ->
                case present of
                    Nothing ->
                        D.succeed fallback

                    Just _ ->
                        D.field name inner
            )


whenDecoder : String -> D.Decoder When
whenDecoder name =
    case List.filter (\w -> (whenSpec w).name == name) whens of
        [ w ] ->
            D.succeed w

        _ ->
            D.fail ("not a condition an alert knows: " ++ name ++ "; expected one of " ++ String.join ", " (List.map (whenSpec >> .name) whens))


{-| The verbs a feed may be polled with. `NET_METHODS` in `main.ts` is the same
list on the other side of the wire, and `browser_test.ts` fails when the two
disagree. One the server would refuse is refused here too: a select reading GET
over a document that says otherwise is a lie about what the poller sends.
-}
netMethods : List String
netMethods =
    [ "GET", "POST", "PUT" ]


methodDecoder : String -> D.Decoder String
methodDecoder name =
    if List.member name netMethods then
        D.succeed name

    else
        D.fail ("not a method a feed can be polled with: " ++ name ++ "; expected one of " ++ String.join ", " netMethods)


{-| How a feed's next page is asked for. `PAGE_BY` in `main.ts` is the same list
on the other side of the wire, and the poller refuses a mode that is not on it.
Empty is not a fifth mode: it is the feed that says nothing about
paging, which is one request a poll, and it is what the select clears to. A mode
the server would refuse is refused here too, the way a method is: a select the
page draws empty over a document holding a mode it does not know is a lie about
what the poller asks for.
-}
pageBy : List String
pageBy =
    [ "page", "offset", "cursor", "link" ]


pageByDecoder : String -> D.Decoder String
pageByDecoder name =
    if name == "" || List.member name pageBy then
        D.succeed name

    else
        D.fail ("not a way a feed reads its pages: " ++ name ++ "; expected one of " ++ String.join ", " pageBy ++ ", or nothing for one request a poll")


{-| What a mode reads and where it stops, beside the two fields that set it:
`param` is the query parameter it counts in, `path` the place in the answer it
reads the next cursor out of. One table rather than three, because a mode that
gained a hint and lost the input that fills it in drew a form nobody could
finish.

`pageByDecoder` refuses a mode outside `pageBy`, so the last branch is the empty
select.

-}
pageForm : String -> { param : Bool, path : Bool, hint : String }
pageForm mode =
    case mode of
        "page" ->
            { param = True, path = False, hint = "sends the parameter as 1, then 2, 3 … and stops at a page that is an empty JSON array. Every page must be a JSON array." }

        "offset" ->
            { param = True, path = False, hint = "sends the parameter as 0, then the rows received so far, and stops at a page that is an empty JSON array. Every page must be a JSON array." }

        "cursor" ->
            { param = True, path = True, hint = "sends no parameter first, then the value at the path in the page before, and stops when that path holds nothing. A page is an array, or an object whose first array holds the rows." }

        "link" ->
            { param = False, path = False, hint = "follows the Link header's rel=\"next\" url, which must be on the same origin as the URL above, and stops when no next is named." }

        _ ->
            { param = False, path = False, hint = "one request a poll: what comes back is what is stored." }


{-| What a good run does to the runs before it. `NET_MODES` in `main.ts` is the
same list on the other side of the wire, and `browser_test.ts` fails when the two
disagree. Empty is not a fourth mode: it is the feed that says nothing about it,
which appends the whole log, and it is what the select clears to.
-}
netModes : List String
netModes =
    [ "append", "replace", "upsert" ]


netModeDecoder : String -> D.Decoder String
netModeDecoder name =
    if name == "" || List.member name netModes then
        D.succeed name

    else
        D.fail ("not a way a feed's runs are stored: " ++ name ++ "; expected one of " ++ String.join ", " netModes ++ ", or nothing for a feed that keeps every run")


{-| What a mode keeps, beside the field that identifies a row: `key` is the path
into a row an upsert supersedes by, and no other mode takes one. One table rather
than two, for the reason `pageForm` is one.

`netModeDecoder` refuses a mode outside `netModes`, so the last branch is the
empty select.

-}
storeForm : String -> { key : Bool, hint : String }
storeForm mode =
    case mode of
        "replace" ->
            { key = False, hint = "the newest good run is the sheet: the runs before it go, and the failed polls stay as the log of why." }

        "upsert" ->
            { key = True, hint = "a run supersedes the earlier runs that answered for a record it answered for again, read at the key below on every row." }

        _ ->
            { key = False, hint = "every run is kept, newest first, up to this sheet's retention." }


{-| How a chart is drawn. One table with no wildcard, so a new constructor fails
to compile in `viewChart` rather than falling through to a line; `CHART_KINDS` in
`src/sql.mjs` is the same list on the other side of the wire, `chartSql` refuses
a kind that is not on it, and `browser_test.ts` fails when the two disagree.

`kind` used to be a plain string that nothing checked, so a typo drew a line and
said nothing about it.

-}
type ChartKind
    = Line
    | Bar
    | Area
    | Scatter
    | Kpi
    | Box


kindSpec : ChartKind -> { name : String, label : String }
kindSpec kind =
    case kind of
        Line ->
            { name = "line", label = "a line through every point" }

        Bar ->
            { name = "bar", label = "one bar per point, from zero" }

        Area ->
            { name = "area", label = "the line, filled to the baseline" }

        Scatter ->
            { name = "scatter", label = "one dot per point" }

        Kpi ->
            { name = "kpi", label = "the last value, its change, and a sparkline" }

        Box ->
            { name = "box", label = "the spread of the rows at each x" }


chartKinds : List ChartKind
chartKinds =
    [ Line, Bar, Area, Scatter, Kpi, Box ]


{-| A chart's seven settings. Grouped into three decoders rather than reached
for one at a time, because `D.map7` is the ceiling and the next field after that
is an `andThen` nobody can read: the axes go together, what is drawn beside them
goes together, and the source and the kind say where and how.

Every string field falls back to blank and `kind` to a line, so a chart written
before any of them existed still decodes. `annotations` goes through
`optionalField`, which refuses a field that is present and spelled wrong rather
than painting it as the empty list -- a chart quietly drawing no marks is a chart
lying about its document.

-}
chartDecoder : D.Decoder Doc
chartDecoder =
    let
        str name =
            D.oneOf [ D.field name D.string, D.succeed "" ]
    in
    D.map3
        (\( source, kind ) ( x, y, y2 ) ( series, annotations ) ->
            Chart (Chart_ source kind x y y2 series annotations)
        )
        (D.map2 Tuple.pair
            (str "source")
            (D.oneOf [ D.field "kind" D.string, D.succeed "line" ] |> D.andThen kindDecoder)
        )
        (D.map3 (\x y y2 -> ( x, y, y2 )) (str "x") (str "y") (str "y2"))
        (D.map2 Tuple.pair
            (str "series")
            (optionalField "annotations"
                (D.list (D.map2 Tuple.pair (D.field "at" D.string) (D.field "label" D.string)))
                []
            )
        )


kindDecoder : String -> D.Decoder ChartKind
kindDecoder name =
    case List.filter (\k -> (kindSpec k).name == name) chartKinds of
        [ k ] ->
            D.succeed k

        _ ->
            D.fail ("not a kind of chart: " ++ name ++ "; expected one of " ++ String.join ", " (List.map (kindSpec >> .name) chartKinds))


{-| Everything the table knows about a column type: what it is called, which
way its cells read, and how wide it starts, in px. One table with no wildcard,
so a new constructor fails to compile here rather than rendering left-aligned at
the default width and saying nothing. `width = Nothing` is a column that sizes
itself.
-}
spec : Type -> { name : String, align : H.Attribute Msg, width : Maybe Int }
spec typ =
    case typ of
        Unknown ->
            { name = "unknown", align = S.textAlignLeft, width = Nothing }

        Text ->
            { name = "text", align = S.textAlignLeft, width = Nothing }

        Number ->
            { name = "num", align = S.textAlignRight, width = Just 80 }

        Usd ->
            { name = "usd", align = S.textAlignRight, width = Just 80 }

        Boolean ->
            { name = "bool", align = S.textAlignCenter, width = Just 32 }

        Percentage ->
            { name = "percentage", align = S.textAlignRight, width = Just 64 }

        Date ->
            { name = "date", align = S.textAlignLeft, width = Just 112 }

        Many typ_ ->
            { name = "list " ++ typeName typ_, align = S.textAlignLeft, width = Nothing }

        Link ->
            { name = "link", align = S.textAlignLeft, width = Nothing }

        SheetId ->
            { name = "sheet_id", align = S.textAlignCenter, width = Just 48 }

        Json ->
            { name = "json", align = S.textAlignLeft, width = Nothing }

        Timestamp ->
            { name = "timestamp", align = S.textAlignLeft, width = Nothing }

        Image ->
            { name = "image", align = S.textAlignLeft, width = Nothing }

        Delete ->
            { name = "delete", align = S.textAlignCenter, width = Just 64 }

        Trash ->
            { name = "trash", align = S.textAlignCenter, width = Just 64 }

        Restore ->
            { name = "restore", align = S.textAlignCenter, width = Just 72 }

        Star ->
            { name = "star", align = S.textAlignCenter, width = Just 40 }

        Create ->
            { name = "create", align = S.textAlignRight, width = Just 160 }

        Form ->
            { name = "form", align = S.textAlignCenter, width = Nothing }

        Enum options ->
            { name = "enum:" ++ String.join "," options, align = S.textAlignLeft, width = Nothing }

        Thumb ->
            { name = "thumb", align = S.textAlignLeft, width = Just 64 }


typeName : Type -> String
typeName typ =
    (spec typ).name


{-| Whether a column's cells are numbers, rather than text that happens to
read like one -- the gate `decimals`, `format` and `shade` all share. `format`
needs no render-side copy of this: `cellDecoder`'s `Text` branch never reads
`format` at all. Shading has no such branch to fall through, so without this a
shade word landing on a text column (a collaborator's older client, a document
from elsewhere) painted a background behind whichever cells were digit-shaped
strings, `number`'s own lenient string branch parsing "02139" same as 02139.
-}
numericColumn : Type -> Bool
numericColumn typ =
    List.member typ [ Number, Usd, Percentage ]


{-| A column the page makes up rather than reads: the library's own columns, a
net sheet's created\_at and body, a chart's x and y. There is no document to
have spelled its type, so `spec` supplies the spelling.
-}
madeCol : String -> String -> Type -> Col
madeCol key name typ =
    Col key name typ (typeName typ)



---- PARSER -------------------------------------------------------------------


string : D.Decoder String
string =
    D.oneOf
        [ D.string
        , D.map String.fromInt D.int
        , D.map String.fromFloat D.float
        , D.null "NULL"
        , D.map (String.join ", ") (D.list (D.lazy (\_ -> string)))
        , D.map (String.join ", " << List.map (\( k, v ) -> k ++ ": " ++ v) << Dict.toList) (D.dict (D.lazy (\_ -> string)))
        , D.map (\c -> iif c "true" "false") D.bool
        ]


number : D.Decoder Float
number =
    D.oneOf
        [ D.float
        , D.map toFloat D.int
        , D.andThen (String.toFloat >> Maybe.map D.succeed >> Maybe.withDefault (D.fail "")) D.string
        ]


boolean : D.Decoder Bool
boolean =
    D.oneOf
        [ D.bool
        , D.string |> D.map (\c -> String.toLower c == "true" || c == "t" || c == "1")
        , D.int |> D.map ((/=) 0)
        , D.null False
        , D.succeed False
        ]


docDecoder : D.Decoder Doc
docDecoder =
    D.field "type" D.string
        |> D.andThen
            (\typ ->
                case typ of
                    "library" ->
                        D.succeed Library

                    "shop" ->
                        D.succeed Shop

                    "table" ->
                        D.field "data" <|
                            D.map Tab tableDecoder

                    "net-hook" ->
                        D.succeed NetHook

                    "portal" ->
                        D.field "data" <|
                            D.succeed (Unviewable typ)

                    "net-socket" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map (\url -> NetSocket { url = url })
                                    (D.field "url" D.string)

                    "net-http" ->
                        -- More fields than D.map8 takes: the request and its
                        -- schedule are decoded beside what is done with the answer,
                        -- and the two halves are joined rather than one of them
                        -- going through `andThen` for the sake of more names.
                        D.field "data" <|
                            D.index 0 <|
                                D.map2
                                    (\req keep ->
                                        NetHttp
                                            { url = req.url
                                            , interval = req.interval
                                            , headers = req.headers
                                            , method = req.method
                                            , body = req.body
                                            , pageBy = keep.by
                                            , pageParam = keep.param
                                            , pagePath = keep.path
                                            , mode = keep.mode
                                            , key = keep.key
                                            , rowsPath = keep.rowsPath
                                            , paused = keep.paused
                                            , cron = req.cron
                                            , timezone = req.timezone
                                            }
                                    )
                                    (D.map7
                                        (\url interval headers method body cron timezone ->
                                            { url = url, interval = interval, headers = headers, method = method, body = body, cron = cron, timezone = timezone }
                                        )
                                        (D.field "url" D.string)
                                        (D.field "interval" D.int)
                                        (D.oneOf [ D.field "headers" D.string, D.succeed "" ])
                                        (optionalField "method" (D.string |> D.andThen methodDecoder) "GET")
                                        (optionalField "body" D.string "")
                                        (optionalField "cron" D.string "")
                                        (optionalField "timezone" D.string "")
                                    )
                                    (D.map7
                                        (\by param path mode key rowsPath paused ->
                                            { by = by, param = param, path = path, mode = mode, key = key, rowsPath = rowsPath, paused = paused }
                                        )
                                        (optionalField "page_by" (D.string |> D.andThen pageByDecoder) "")
                                        (optionalField "page_param" D.string "")
                                        (optionalField "page_path" D.string "")
                                        (optionalField "mode" (D.string |> D.andThen netModeDecoder) "")
                                        (optionalField "key" D.string "")
                                        (optionalField "rows_path" D.string "")
                                        (optionalField "paused" D.bool False)
                                    )

                    "alert" ->
                        D.field "data" <|
                            D.index 0 <|
                                -- Split the way the net-http branch is: D.map8 is the ceiling.
                                D.map3
                                    (\cfg cron timezone ->
                                        Alert
                                            { code = cfg.code
                                            , to = cfg.to
                                            , interval = cfg.interval
                                            , digest = cfg.digest
                                            , when = cfg.when
                                            , paused = cfg.paused
                                            , snoozedUntil = cfg.snoozedUntil
                                            , cron = cron
                                            , timezone = timezone
                                            }
                                    )
                                    (D.map7 (\code to interval digest when paused snoozedUntil -> { code = code, to = to, interval = interval, digest = digest, when = when, paused = paused, snoozedUntil = snoozedUntil })
                                        (D.oneOf [ D.field "code" D.string, D.succeed "" ])
                                        (D.oneOf [ D.field "to" D.string, D.succeed "" ])
                                        (D.oneOf [ D.field "interval" D.int, D.succeed 3600 ])
                                        (D.oneOf [ D.field "digest" D.bool, D.succeed False ])
                                        -- Absent is rows, the way a document written before there was a
                                        -- `when` means it. Present and unknown, or present and not a
                                        -- string, is refused by name rather than shown as rows: a select
                                        -- saying "rows" over a document that says otherwise is a lie the
                                        -- server would not tell.
                                        (optionalField "when" (D.string |> D.andThen whenDecoder) OnRows)
                                        (optionalField "paused" D.bool False)
                                        -- A plain string the decoder does not
                                        -- check, the way `page_param` is: the
                                        -- poller's own refusal is the check, and a
                                        -- sheet the server has refused must still
                                        -- draw so its owner can fix the cell.
                                        (optionalField "snoozed_until" D.string "")
                                    )
                                    (optionalField "cron" D.string "")
                                    (optionalField "timezone" D.string "")

                    "chart" ->
                        D.field "data" (D.index 0 chartDecoder)

                    "dashboard" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map Dashboard
                                    (D.oneOf [ D.field "tiles" (D.list D.string), D.succeed [] ])

                    "template" ->
                        D.succeed (Unviewable typ)

                    "query" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map Query
                                    (D.map5 Query_
                                        (D.field "lang" D.string)
                                        (D.field "code" D.string)
                                        -- TODO
                                        (D.maybe (D.field "args" (D.succeed Dict.empty)) |> D.map (Maybe.withDefault Dict.empty))
                                        (D.maybe (D.field "examples" (D.list D.string)) |> D.map (Maybe.withDefault []))
                                        (D.maybe (D.field "cols" D.value) |> D.map (Maybe.withDefault (E.object [])))
                                    )

                    _ ->
                        if String.startsWith "codex-" typ then
                            D.succeed (Unviewable typ)

                        else
                            D.fail ("Unknown sheet type: " ++ typ)
            )


tableDecoder : D.Decoder Table
tableDecoder =
    D.map2 Table
        (D.index 0 (D.array colDecoder))
        (D.map (Array.slice 1 -1 << Array.push Dict.empty) (D.array rowDecoder))


rowDecoder : D.Decoder Row
rowDecoder =
    D.oneOf
        [ D.array D.value |> D.map (Array.toIndexedList >> List.map (\( k, v ) -> ( String.fromInt k, v )) >> Dict.fromList)
        , D.dict D.value
        ]


colDecoder : D.Decoder Col
colDecoder =
    let
        parseType : String -> Type
        parseType typeStr =
            if String.startsWith "enum:" typeStr then
                -- Parse "enum:option1,option2,option3"
                typeStr
                    |> String.dropLeft 5
                    |> String.split ","
                    |> List.map String.trim
                    |> List.filter (not << String.isEmpty)
                    |> Enum

            else
                Dict.get typeStr typeSpellings |> Maybe.withDefault Unknown
    in
    D.oneOf
        [ D.map3 (\key name raw -> Col key name (parseType raw) raw)
            (D.field "key" string)
            (D.field "name" D.string)
            (D.field "type" (D.nullable D.string |> D.map (Maybe.withDefault "")))
        , D.succeed (madeCol "" "" Text)
        ]


{-| Every type a column may declare, in the one spelling the page writes.
`COLUMN_TYPES` in `src/sql.mjs` is the same list on the other side of the wire
and carries the reason; `browser_test.ts` fails in both directions when they
stop agreeing.
-}
columnTypes : List ( String, Type )
columnTypes =
    [ ( "text", Text )
    , ( "num", Number )
    , ( "int", Number )
    , ( "float", Number )
    , ( "usd", Usd )
    , ( "percentage", Percentage )
    , ( "bool", Boolean )
    , ( "date", Date )
    , ( "timestamp", Timestamp )
    , ( "json", Json )
    , ( "link", Link )
    , ( "image", Image )
    , ( "sheet_id", SheetId )
    , ( "form", Form )
    , ( "create", Create )
    ]


{-| Spellings that are read and never written. Documents written before the list
above settled hold them, and dropping them would read those columns as unknown;
admitting them to a **write** is what let the editor store `pct`, which the
engine does not know and so does not check.
-}
typeAliases : List ( String, Type )
typeAliases =
    [ ( "number", Number )
    , ( "string", Text )
    , ( "pct", Percentage )
    , ( "percent", Percentage )
    , ( "datetime", Timestamp )
    ]


typeSpellings : Dict String Type
typeSpellings =
    Dict.fromList (columnTypes ++ typeAliases)


{-| Whether the page may **write** this type string. Aliases are readable and
not writable, so nothing new lands in a spelling the engine skips. An enum
carries its own options, so it is a family rather than a name and is matched by
its prefix -- the same rule `knownType` applies in `src/sql.mjs`.
-}
knownTypeName : String -> Bool
knownTypeName name =
    List.any (Tuple.first >> (==) name) columnTypes || (String.startsWith "enum:" name && String.length name > 5)


{-| What the refusal offers. The names themselves, not the types behind them:
`int`, `float` and `num` are one `Type` here and three types to the engine, so
offering the `Type` names told a user to write `num` where `int` was meant.
-}
canonicalTypeNames : List String
canonicalTypeNames =
    List.map Tuple.first columnTypes


{-| How many listings one shop fetch asks for. The whole catalogue, because it
is small enough to be one page and every column filter on the shop is a filter
over the rows that came back; the answer's `Content-Range` is what says the day
that stops being true.
-}
shopLimit : Int
shopLimit =
    500


shopDecoder : D.Decoder Table
shopDecoder =
    D.field "data" tableDecoder


{-| The rows of `library:freshness`, keyed by the sheet each is about. Every
field is required: a renamed column would otherwise read as a library where
every feed is fine, which is the one answer this read must never invent.
`failures_since_ok` is a count(\*), which postgres sends as a string.
-}
freshnessDecoder : D.Decoder (Dict Id Freshness)
freshnessDecoder =
    D.list
        (D.map4 (\id lastRun failures nextRun -> ( id, Freshness lastRun (round failures) nextRun ))
            (D.field "sheet_id" D.string)
            (D.field "last_run" (D.nullable D.string))
            (D.field "failures_since_ok" number)
            (D.field "next_run" (D.nullable D.string))
        )
        |> D.map Dict.fromList


{-| One library row's worth of feed health. A sheet the read does not answer for
gets nothing -- not a zero and not a timestamp -- because "never polled" and
"polled and fine" are different facts and a blank cell claims neither.
-}
freshnessCell : Maybe Freshness -> String
freshnessCell fresh =
    case fresh of
        Nothing ->
            ""

        Just { lastRun, failures } ->
            String.join " · " <|
                List.filterMap identity
                    [ -- A full ISO timestamp in a library row is noise past the
                      -- minute, and a sheet that has never run says so.
                      Just (lastRun |> Maybe.map (String.left 16 >> String.replace "T" " ") |> Maybe.withDefault "never run")
                    , iif (failures > 0) (Just (String.fromInt failures ++ " failed")) Nothing
                    ]



---- INIT ---------------------------------------------------------------------


type alias Flags =
    D.Value


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    let
        tutorialStep : Int
        tutorialStep =
            D.decodeValue (D.field "tutorial" D.int) flags |> Result.withDefault 0

        -- The one spelling of the API host, handed in by the page rather than
        -- written down a second time here. No default: a blank base would send
        -- every request at whatever origin served the page, which is a wrong
        -- answer wearing the face of a working one.
        ( api, apiError ) =
            case D.decodeValue (D.field "api" D.string) flags of
                Ok base ->
                    ( base, "" )

                Err err ->
                    ( "", "The page did not hand Elm an api base: " ++ D.errorToString err )

        model : Model
        model =
            route url
                { nav = nav
                , api = api
                , id = ""
                , search = ""
                , error = apiError
                , library = Dict.empty
                , sheet = emptySheet
                , auth =
                    { state = Anonymous
                    , email = ""
                    , password = ""
                    }
                , deleteConfirm = Nothing
                , pending = Nothing
                , trash = False
                , importing = Nothing
                , showSettings = False
                , embed = False
                , share = emptyShare
                , showShortcuts = False
                , palette = Nothing
                , history = Nothing
                , freshness = Dict.empty
                , tutorial = iif (tutorialStep < 0) Nothing (Just (clamp 0 4 tutorialStep))
                , now = 0
                }
    in
    ( model, Cmd.batch [ changeId model.id, Task.perform Clock Time.now ] )


route : Url -> Model -> Model
route url model =
    let
        showSettings =
            url.fragment == Just "settings"

        baseModel =
            url
                |> UrlP.parse
                    (UrlP.map
                        (\id search -> { model | id = id, search = Maybe.withDefault "" search })
                        (UrlP.top
                            </> UrlP.oneOf [ UrlP.string, UrlP.map "" UrlP.top ]
                            <?> UrlQ.string "q"
                        )
                    )
                |> Maybe.withDefault model
    in
    -- ?embed= drops every piece of chrome, which is the whole embed: one sheet,
    -- with no app around it. Read off the raw query the way the fragment is,
    -- rather than threaded through the path parser that also carries ?q=.
    { baseModel
        | showSettings = showSettings
        , embed = url.query |> Maybe.map (String.contains "embed") |> Maybe.withDefault False
    }


{-| Settings, the shortcut sheet, the palette, the history and a held rename or
delete each mount an `aria-modal` panel, and a screen reader reads only one of
them. Settings lives in the URL's `#settings`, so it closes through
`SettingsClose` or a reload opens it again. A held edit closes the way Cancel
closes it: unwritten.
-}
closeModals : Model -> ( Model, Cmd Msg )
closeModals model =
    let
        closed =
            { model | palette = Nothing, showShortcuts = False, pending = Nothing, history = Nothing }
    in
    iif model.showSettings (update SettingsClose closed) ( closed, Cmd.none )



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChange Url
    | LinkClick Browser.UrlRequest
    | LibrarySync D.Value
    | DocSelect (Idd { doc : D.Value })
    | DocChange (Idd DocDelta)
    | DocNotify (Idd D.Value)
    | DocQuery (Idd D.Value)
    | DocError String
    | DocMsg DocMsg
    | DocNew E.Value
    | DocFork
    | ChartDownload String
    | DocNewQuery
    | DocNewTable
    | DocTrash Id
    | DocRestore Id
    | TrashToggle
    | DocStar Id Bool
    | TrashSelected
    | TagSelected
    | TagInput String
    | ColumnSplitInput String
    | ColumnNearInput String
    | DocDelete Id
    | DocDeleteConfirm Id
    | DocDeleteCancel
    | SettingsClose
    | ShareLoad D.Value
    | Preflight
    | PreflightLoad (Idd D.Value)
    | Clock Time.Posix
    | AlertSnooze
    | AlertSnoozeAt Time.Posix
    | RunNow
    | RunLoad (Idd D.Value)
    | HistoryMsg HistoryMsg
    | ShareEmailChange String
    | ShareRoleChange String
    | ShareDaysChange String
    | SharePasswordChange String
    | ShareAdd
    | ShareRemove String
    | SharePublic Bool
    | SharePersonal Bool
    | ShareLink
    | ShareHook
    | ShortcutsToggle Bool
    | FreshnessLoad D.Value
    | LineageLoad D.Value
    | PendingConfirm
    | PendingCancel
    | Goto Id
    | PaletteToggle Bool
    | PaletteNav Int
    | PaletteRun Int
    | SettingsNameChange String
    | SettingsTagsChange String
    | KeyDown KeyEvent
    | EditCommitMove Int Int
    | EditCancel
    | CellMouseClick
    | CellMouseDoubleClick String
    | CellMouseDown
    | CellMouseUp
    | CellHover Index
    | ColumnSort Bool String
    | ColumnHide String
    | ColumnsShowAll
    | ColumnPin String
    | ColumnDecimals String String
    | ColumnFormat String String
    | ColumnShade String String
    | ColumnMoveStart String
    | RowMoveStart Int
    | MoveEnd
    | ColumnResizeStart String Int
    | ColumnResizeMove Int
    | ColumnResizeEnd
    | FilterToggle String
    | FilterClear String
    | FilterInput String String
    | FindOpen Bool
    | FindClose
    | FindTextChange String
    | ReplaceTextChange String
    | FindNext
    | FindPrev
    | ReplaceOne
    | ReplaceAll
    | Undo
    | Redo
    | InputChange Input String
    | ShopFetch (Result Http.Error Table)
    | CsvImportFile File
    | CsvImportUpload String String
    | ImportPreviewed D.Value
    | ImportTypeChange String String
    | ImportConfirm
    | ImportCancel
    | AuthMsg AuthMsg
    | AuthResult D.Value
    | ClipboardCopy
    | ClipboardPaste String
    | CopyText String
    | TutorialDismiss
    | SelectAll
    | QueryEditorUpdate { cursorPos : Int, textBeforeCursor : String }
    | ColumnsLoad (Idd (List String))
    | AutocompleteSelect String
    | AutocompleteNav Int
    | AutocompleteClose


type alias KeyEvent =
    { key : String
    , shift : Bool
    , ctrl : Bool
    , meta : Bool
    }


type HistoryMsg
    = HistoryOpen
    | HistoryLoad (Idd D.Value)
    | HistoryPick String
    | HistoryShow (Idd D.Value)
    | HistoryClose


type AuthMsg
    = AuthSubmit
    | AuthLogout


type DocMsg
    = SheetWrite Index
    | SheetRowPush
    | SheetColumnPush
    | SheetRowInsert (List Int)
    | SheetRowDuplicate (List Int)
    | SheetRowDelete (List Int)
    | SheetColumnDelete (List Int)
    | SheetColumnMove Int Int
    | SheetRowMove Int Int -- document rows: data[from] to data[to]
    | SheetClearCells (List Index)
    | SheetFillDown Rect
    | SheetColumnTrim String
    | SheetColumnCase String Casing
    | SheetRowsDropBlank String
    | SheetRowsDedupe
    | SheetRowsDedupeNear String Int
    | SheetColumnSplit String String
    | CellCheck Index Bool


type Casing
    = Upper
    | Lower


type Input
    = SheetSearch
    | CellWrite
    | QueryCode
    | AuthEmail
    | AuthPassword
    | NetUrl
    | NetInterval
    | NetHeaders
    | NetMethod
    | NetBody
    | NetPageBy
    | NetPageParam
    | NetPagePath
    | NetMode
    | NetKey
    | NetRowsPath
    | NetPaused
    | NetCron
    | NetTimezone
    | AlertCode
    | AlertTo
    | AlertDigest
    | AlertWhen
    | AlertSnoozed
    | ChartSource
    | ChartKind
    | ChartX
    | ChartY
    | ChartY2
    | ChartSeries
    | ChartAnnotations
    | DashboardTiles
    | PaletteQuery



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ librarySynced LibrarySync
        , docSelected DocSelect
        , docChanged DocChange
        , docNotified DocNotify
        , docQueried DocQuery
        , docErrored DocError
        , authResult AuthResult
        , Browser.onKeyDown keyEventDecoder
        , pasteFromClipboard ClipboardPaste
        , requestCopy (always ClipboardCopy)
        , queryEditorState QueryEditorUpdate
        , columnsLoaded ColumnsLoad
        , shareLoaded ShareLoad
        , preflightLoaded PreflightLoad
        , historyLoaded (HistoryMsg << HistoryLoad)
        , historyShown (HistoryMsg << HistoryShow)
        , runLoaded RunLoad
        , importPreviewed ImportPreviewed
        , freshnessLoaded FreshnessLoad
        , lineageLoaded LineageLoad
        , case model.sheet.resizing of
            Just _ ->
                Sub.batch
                    [ Browser.onMouseMove (D.map ColumnResizeMove (D.field "clientX" (D.map round D.float)))
                    , Browser.onMouseUp (D.succeed ColumnResizeEnd)
                    ]

            Nothing ->
                Sub.none
        , case model.sheet.moving of
            Just _ ->
                Browser.onMouseUp (D.succeed MoveEnd)

            Nothing ->
                Sub.none
        ]


keyEventDecoder : D.Decoder Msg
keyEventDecoder =
    D.map5
        (\key shift ctrl meta tagName ->
            -- Skip keyboard handling when focus is on input elements
            if List.member tagName [ "INPUT", "TEXTAREA", "SELECT" ] then
                NoOp

            else
                KeyDown { key = key, shift = shift, ctrl = ctrl, meta = meta }
        )
        (D.field "key" D.string)
        (D.field "shiftKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.at [ "target", "tagName" ] D.string |> D.maybe |> D.map (Maybe.withDefault ""))


{-| Cell editor keys. The global onKeyDown ignores INPUT/SELECT focus, so
Enter/Tab/Escape are handled here, on the editor element itself.
-}
onEditorKeydown : H.Attribute Msg
onEditorKeydown =
    A.preventDefaultOn "keydown"
        (D.map2 Tuple.pair (D.field "key" D.string) (D.field "shiftKey" D.bool)
            |> D.andThen
                (\( key, shift ) ->
                    case key of
                        "Enter" ->
                            D.succeed ( EditCommitMove 0 1, True )

                        "Tab" ->
                            D.succeed ( EditCommitMove (iif shift -1 1) 0, True )

                        "Escape" ->
                            D.succeed ( EditCancel, True )

                        _ ->
                            D.fail "unhandled"
                )
        )


{-| Palette keys. It takes the selected index rather than reading it back out of
the model, because the same message runs a row that was clicked.
-}
onPaletteKeydown : Int -> H.Attribute Msg
onPaletteKeydown selected =
    A.preventDefaultOn "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            D.succeed ( PaletteRun selected, True )

                        "ArrowDown" ->
                            D.succeed ( PaletteNav 1, True )

                        "ArrowUp" ->
                            D.succeed ( PaletteNav -1, True )

                        "Escape" ->
                            D.succeed ( PaletteToggle False, True )

                        _ ->
                            D.fail "unhandled"
                )
        )


onFindKeydown : H.Attribute Msg
onFindKeydown =
    A.preventDefaultOn "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            D.succeed ( FindNext, True )

                        "Escape" ->
                            D.succeed ( FindClose, True )

                        _ ->
                            D.fail "unhandled"
                )
        )


{-| The library strip's tag box. The global onKeyDown ignores INPUT focus, so
Enter is handled here, on the box itself.
-}
onTagKeydown : H.Attribute Msg
onTagKeydown =
    A.preventDefaultOn "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            D.succeed ( TagSelected, True )

                        _ ->
                            D.fail "unhandled"
                )
        )



---- UPDATE -------------------------------------------------------------------


{-| The sheet on a library row, read off the rows as the view draws them --
sorted, filtered and searched -- rather than the same position in the unsorted
dictionary, which renamed a stranger whenever the library was sorted. `Nothing`
is a row the library does not draw, a header row say; it is never the empty id,
which is the library's own.
-}
libraryIdAtRow : Model -> Int -> Maybe String
libraryIdAtRow model y =
    resolveTable model
        |> Result.toMaybe
        |> Maybe.andThen (\tbl -> Array.get (y - 1) (filterAndSort model.search model.sheet tbl.rows))
        |> Maybe.andThen (Dict.get "sheet_id")
        |> Maybe.andThen (D.decodeValue D.string >> Result.toMaybe)


{-| The keyboard walks whatever the view draws -- a table, the library as
searched, a query's last result, a feed's run log -- and the one that draws
it is the one that bounds it, so the two cannot disagree.
-}
tableBounds : Model -> TableBounds
tableBounds model =
    case resolveTable model of
        Ok tbl ->
            { maxX = Array.length tbl.cols - 1, maxY = Array.length (filterAndSort model.search model.sheet tbl.rows) }

        Err _ ->
            { maxX = 0, maxY = 0 }


{-| The refusal a keystroke or a double-click earns on a cell the keyboard can
reach but nobody types into: a query's result, a feed's log, a chart, the shop.
-}
computedCell : String
computedCell =
    "Expected an edit to a table cell, received one on a sheet whose rows are computed, not typed. Edit what computes them: a query's SQL, a feed's source, a chart's settings."


advanceTutorial : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
advanceTutorial n ( model, cmd ) =
    if model.tutorial == Just n then
        if n == 4 then
            ( { model | tutorial = Nothing }, Cmd.batch [ cmd, saveTutorial -1 ] )

        else
            ( { model | tutorial = Just (n + 1) }, Cmd.batch [ cmd, saveTutorial (n + 1) ] )

    else
        ( model, cmd )


{-| One exhaustive `case` over every `Msg`, with no wildcard: a new constructor
has to fail to compile here rather than compile and do nothing. That is why this
is not split into per-family `Msg -> Model -> ( Model, Cmd Msg )` functions --
each would need a `_ ->` arm for the messages it does not handle, which is the
silent default all over again. The four branches long enough to be their own
subject are named functions below instead.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet, auth } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            let
                next =
                    route url model
            in
            ( { next
                | palette = iif next.showSettings Nothing next.palette
                , showShortcuts = next.showShortcuts && not next.showSettings
                , pending = iif (next.showSettings || next.id /= model.id) Nothing next.pending
                , history = iif (next.showSettings || next.id /= model.id) Nothing next.history
                , share =
                    -- Whatever the panel holds belongs to the sheet it was
                    -- loaded for, so a navigation empties it. A secret goes
                    -- either way: it must not follow you to another sheet.
                    iif (next.showSettings && next.id == model.id)
                        (\sh -> { sh | hook = Nothing })
                        (always emptyShare)
                        next.share
              }
            , Cmd.batch
                [ changeId next.id
                , -- Load the member list when the settings modal opens, and
                  -- again when it stays open onto a different sheet. Without the
                  -- second case, navigating with the panel up left the first
                  -- sheet's permissions on screen with nothing in flight to
                  -- correct them.
                  iif (next.showSettings && (not model.showSettings || next.id /= model.id))
                    (shareAction { shareAsk | id = next.id, action = "list" })
                    Cmd.none
                ]
            )

        LinkClick (Browser.Internal url) ->
            ( model, Nav.pushUrl model.nav (Url.toString url) )

        LinkClick (Browser.External url) ->
            ( model, Nav.load url )

        LibrarySync data ->
            case
                D.decodeValue
                    (D.dict
                        (D.map8 SheetInfo
                            (D.oneOf [ D.field "name" D.string, D.succeed "" ])
                            (D.oneOf [ D.field "tags" (D.list D.string), D.succeed [] ])
                            (D.oneOf [ D.field "scratch" D.bool, D.succeed False ])
                            (D.oneOf [ D.field "system" D.bool, D.succeed False ])
                            (D.oneOf [ D.field "thumb" D.value, D.succeed E.null ])
                            (D.oneOf [ D.field "seen" D.string, D.succeed "" ])
                            (D.oneOf [ D.field "trashed" D.bool, D.succeed False ])
                            (D.oneOf [ D.field "starred" D.bool, D.succeed False ])
                        )
                    )
                    data
            of
                Ok library ->
                    ( { model | library = library }, Cmd.none )

                Err err ->
                    ( { model | error = "The library failed to sync: " ++ D.errorToString err }, Cmd.none )

        DocSelect data ->
            let
                -- How this sheet was last arranged, off the columns that carry
                -- it. Read here and nowhere else: a collaborator's sort lands on
                -- your next open of the sheet rather than under your cursor, and
                -- re-reading on every change would overwrite a filter mid-word.
                stored =
                    data.data.doc |> D.decodeValue viewDecoder |> Result.withDefault emptyView
            in
            ( { model
                | error = ""
                , sheet =
                    { id = data.id
                    , select = Rect (xy -1 -1) (xy -1 -1)
                    , hover = xy -1 -1
                    , drag = False
                    , write = Nothing
                    , doc = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString
                    , table = Err ""
                    , stats = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString |> Result.andThen computeStats
                    , hidden = stored.hidden
                    , pinned = stored.pinned
                    , sort = stored.sort
                    , filters = stored.filters
                    , filterOpen = Nothing
                    , findReplace = Nothing
                    , queryAutocomplete = Nothing
                    , queryColumns = Dict.empty
                    , undoStack = []
                    , redoStack = []
                    , netStatus = Nothing
                    , preflight = Nothing
                    , run = Nothing
                    , lineage = data.data.doc |> D.decodeValue (D.field "forked_from" D.string) |> Result.toMaybe
                    , widths = stored.widths
                    , decimals = stored.decimals
                    , formats = stored.formats
                    , shades = stored.shades
                    , splitOn = ""
                    , near = ""
                    , tag = ""
                    , resizing = Nothing
                    , moving = Nothing
                    , storedView = stored
                    }
              }
            , case data.data.doc |> D.decodeValue docDecoder of
                Ok Shop ->
                    Http.get
                        { url = model.api ++ "/shop?limit=" ++ String.fromInt shopLimit
                        , expect = Http.expectJson ShopFetch shopDecoder
                        }

                _ ->
                    Cmd.none
            )

        DocChange data ->
            ( if data.id /= model.sheet.id then
                model

              else
                case applyCellPatches data.data.patches sheet.doc of
                    Just updatedDoc ->
                        { model | sheet = { sheet | doc = updatedDoc, stats = Result.andThen computeStats updatedDoc } }

                    Nothing ->
                        let
                            parsedDoc =
                                data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString
                        in
                        { model
                            | sheet =
                                pruneView parsedDoc
                                    { sheet
                                        | doc = parsedDoc
                                        , stats = Result.andThen computeStats parsedDoc
                                        , lineage =
                                            data.data.doc
                                                |> D.decodeValue (D.field "forked_from" D.string)
                                                |> Result.toMaybe
                                    }
                        }
            , Cmd.none
            )

        DocNotify data ->
            -- Handle document notifications (collaboration events, peer updates, etc.)
            if data.id /= model.sheet.id then
                ( model, Cmd.none )

            else
                -- Decode the notification and update model accordingly
                let
                    notificationType =
                        data.data
                            |> D.decodeValue (D.field "type" D.string)
                            |> Result.withDefault ""
                in
                case notificationType of
                    "refresh" ->
                        -- Request a fresh copy of the document
                        ( model, changeId model.sheet.id )

                    "net-status" ->
                        ( { model
                            | sheet =
                                { sheet
                                    | netStatus =
                                        data.data
                                            |> D.decodeValue (D.field "message" D.string)
                                            |> Result.toMaybe
                                }
                          }
                        , Cmd.none
                        )

                    "error" ->
                        let
                            errorMsg =
                                data.data
                                    |> D.decodeValue (D.field "message" D.string)
                                    |> Result.withDefault "Unknown error"
                        in
                        ( { model | error = errorMsg }, Cmd.none )

                    other ->
                        ( { model | error = "Unknown notification type: " ++ other }, Cmd.none )

        DocQuery data ->
            if data.id /= model.sheet.id then
                ( model, Cmd.none )

            else
                let
                    table =
                        data.data |> D.decodeValue tableDecoder |> Result.mapError D.errorToString

                    advance =
                        case ( table, sheet.doc ) of
                            ( Ok _, Ok (Query _) ) ->
                                advanceTutorial 4

                            _ ->
                                identity

                    -- `model.now` is set once at boot and again only on a
                    -- snooze click, so a tab left open past a snooze's moment
                    -- would read the clock as it stood at page load forever.
                    -- index.html's fetchNet already polls an open alert's run
                    -- log every ten seconds; riding that poll is what keeps
                    -- the snooze line honest without a subscription of its own.
                    tick =
                        case sheet.doc of
                            Ok (Alert _) ->
                                Task.perform Clock Time.now

                            _ ->
                                Cmd.none
                in
                advance ( { model | error = "", sheet = { sheet | table = table } }, tick )

        DocError error ->
            ( { model | error = error }
            , Cmd.none
            )

        DocMsg edit ->
            updateDocMsg edit model

        DocTrash id ->
            -- No confirmation: being undoable is the whole point of the trash,
            -- and a dialog in front of a reversible act only teaches people to
            -- click through the one in front of an irreversible one.
            ( model, updateLibrary (Idd id { name = Nothing, tags = Nothing, trashed = Just True, starred = Nothing }) )

        DocRestore id ->
            ( model, updateLibrary (Idd id { name = Nothing, tags = Nothing, trashed = Just False, starred = Nothing }) )

        TrashToggle ->
            ( { model | trash = not model.trash }, Cmd.none )

        DocStar id on ->
            ( model, updateLibrary (Idd id { name = Nothing, tags = Nothing, trashed = Nothing, starred = Just on }) )

        TrashSelected ->
            -- One `updateLibrary` per sheet rather than one carrying a list: the
            -- port writes this browser's facts about *a* sheet, and a second
            -- shape of message on it is a second thing `Library.set` has to know.
            case model.sheet.doc of
                Ok Library ->
                    let
                        norm =
                            normalizeRect model.sheet.select

                        -- Off the rows as drawn -- sorted, filtered, searched --
                        -- the way every other library verb reads them. A y with
                        -- no sheet behind it is a header row or past the end.
                        ids =
                            List.range norm.a.y norm.b.y
                                |> List.filterMap (libraryIdAtRow model)
                    in
                    if List.isEmpty ids then
                        ( { model | error = "Expected a selection over library rows, received one holding no sheet. Source: rows " ++ String.fromInt norm.a.y ++ " to " ++ String.fromInt norm.b.y ++ ". Fix: select the rows to trash in the library table." }, Cmd.none )

                    else
                        ( model
                        , ids
                            |> List.map (\id -> updateLibrary (Idd id { name = Nothing, tags = Nothing, trashed = Just True, starred = Nothing }))
                            |> Cmd.batch
                        )

                _ ->
                    ( { model | error = "Expected the library open, received the sheet " ++ model.sheet.id ++ ". Source: trash selected sheets. Fix: open the library, select the rows, and run it again." }, Cmd.none )

        TagSelected ->
            -- Added to what each sheet already carries, never written over it:
            -- the `demo` and `example` tags a bundled sheet ships with are what
            -- the strip above filters on, and a replace would take them off.
            case ( model.sheet.doc, String.trim model.sheet.tag ) of
                ( Ok Library, "" ) ->
                    ( { model | error = "Expected a tag, received nothing. Source: the library strip's tag box. Fix: type the tag, then run it." }, Cmd.none )

                ( Ok Library, tag ) ->
                    if String.contains "," tag then
                        -- A tags cell is read back by splitting on ", ", so a
                        -- comma here comes back as two tags nobody typed.
                        ( { model | error = "Expected one tag, received " ++ tag ++ ". Source: the library strip's tag box. Fix: run one tag at a time, with no comma in it." }, Cmd.none )

                    else
                        let
                            norm =
                                normalizeRect model.sheet.select

                            ids =
                                List.range norm.a.y norm.b.y
                                    |> List.filterMap (libraryIdAtRow model)

                            tagsOf id =
                                model.library |> Dict.get id |> Maybe.map .tags |> Maybe.withDefault []
                        in
                        if List.isEmpty ids then
                            ( { model | error = "Expected a selection over library rows, received one holding no sheet. Source: rows " ++ String.fromInt norm.a.y ++ " to " ++ String.fromInt norm.b.y ++ ". Fix: select the rows to tag in the library table." }, Cmd.none )

                        else
                            ( model
                            , ids
                                |> List.filter (\id -> not (List.member tag (tagsOf id)))
                                |> List.map (\id -> updateLibrary (Idd id { name = Nothing, tags = Just (tagsOf id ++ [ tag ]), trashed = Nothing, starred = Nothing }))
                                |> Cmd.batch
                            )

                _ ->
                    ( { model | error = "Expected the library open, received the sheet " ++ model.sheet.id ++ ". Source: tag selected sheets. Fix: open the library, select the rows, and run it again." }, Cmd.none )

        TagInput tag ->
            -- The model only, and never the document, for the reason the split
            -- box below is: it is an argument to a verb nobody has run yet.
            ( { model | sheet = { sheet | tag = tag } }, Cmd.none )

        ColumnSplitInput delimiter ->
            -- The model only, and never the document: the box is an argument to
            -- a verb nobody has run yet, and a patch per keystroke is a sync per
            -- keystroke for everybody watching the sheet.
            ( { model | sheet = { sheet | splitOn = delimiter } }, Cmd.none )

        ColumnNearInput closeness ->
            -- The model only, for the reason the delimiter above is.
            ( { model | sheet = { sheet | near = closeness } }, Cmd.none )

        DocDelete id ->
            -- Show confirmation instead of immediately deleting
            ( { model | deleteConfirm = Just id }, Cmd.none )

        DocDeleteConfirm id ->
            -- Actually delete after confirmation
            ( { model | deleteConfirm = Nothing }, deleteDoc id )

        DocDeleteCancel ->
            ( { model | deleteConfirm = Nothing }, Cmd.none )

        ShareLoad value ->
            updateShareLoad value model

        Preflight ->
            case sheet.doc of
                Ok (NetHttp cfg) ->
                    ( { model | sheet = { sheet | preflight = Nothing } }
                    , preflight { id = sheet.id, data = { url = cfg.url, headers = cfg.headers, method = cfg.method, body = cfg.body } }
                    )

                _ ->
                    ( model, Cmd.none )

        PreflightLoad data ->
            if data.id /= sheet.id then
                ( model, Cmd.none )

            else
                ( { model
                    | sheet =
                        { sheet
                            | preflight =
                                Just (D.decodeValue previewDecoder data.data |> Result.mapError D.errorToString |> Result.andThen identity)
                        }
                  }
                , Cmd.none
                )

        Clock at ->
            ( { model | now = Time.posixToMillis at }, Cmd.none )

        AlertSnooze ->
            -- Asked for at the click and not read off `model.now`: a page left
            -- open for three hours would otherwise snooze for twenty-one.
            ( model, Task.perform AlertSnoozeAt Time.now )

        AlertSnoozeAt at ->
            let
                ms : Int
                ms =
                    Time.posixToMillis at
            in
            ( { model | now = ms }
            , changeDoc
                { id = sheet.id

                -- 86400000 is a day in milliseconds, which is what "snooze
                -- a day" buys.
                , data = [ { action = "set", path = [ E.int 0, E.string "snoozed_until" ], value = E.string (isoStamp (ms + 86400000)) } ]
                }
            )

        RunNow ->
            ( { model | sheet = { sheet | run = Nothing } }, runNow sheet.id )

        RunLoad data ->
            if data.id /= sheet.id then
                ( model, Cmd.none )

            else
                ( { model
                    | sheet =
                        { sheet
                            | run =
                                Just (D.decodeValue runDecoder data.data |> Result.mapError D.errorToString |> Result.andThen identity)
                        }
                  }
                , Cmd.none
                )

        HistoryMsg historyMsg ->
            updateHistory historyMsg model

        ShareEmailChange email ->
            ( { model | share = (\s -> { s | email = email }) model.share }, Cmd.none )

        ShareRoleChange role ->
            ( { model | share = (\s -> { s | role = role }) model.share }, Cmd.none )

        ShareDaysChange days ->
            ( { model | share = (\s -> { s | days = days }) model.share }, Cmd.none )

        SharePasswordChange password ->
            ( { model | share = (\s -> { s | password = password }) model.share }, Cmd.none )

        ShareAdd ->
            if String.contains "@" model.share.email then
                ( { model | share = (\s -> { s | email = "" }) model.share }
                , shareAction { shareAsk | id = model.id, action = "add", email = model.share.email, role = model.share.role }
                )

            else
                ( { model | error = "Enter the email address of the person to share with." }, Cmd.none )

        ShareRemove email ->
            ( model, shareAction { shareAsk | id = model.id, action = "remove", email = email } )

        SharePublic isPublic ->
            ( model
            , shareAction
                { shareAsk
                    | id = model.id
                    , action = "public"
                    , public = isPublic
                    , personal = model.share.personal
                }
            )

        SharePersonal personal ->
            -- The claim alone publishes nothing: it rides with the next
            -- SharePublic, so ticking it is not a way to publish by one click
            -- on the checkbox that says the sheet holds personal data.
            ( { model | share = (\s -> { s | personal = personal }) model.share }, Cmd.none )

        ShareLink ->
            -- Blank is "not asked for", and zero days is what the port carries
            -- that as: the server reads an empty body as the thirty-day
            -- unlocked link, so an untouched panel mints what it always did.
            -- Anything typed that is not a whole number of days is refused
            -- here rather than rounded down to blank, which would silently
            -- mint a thirty-day link for someone who asked for seven.
            case ( String.trim model.share.days, String.toInt (String.trim model.share.days) ) of
                ( "", _ ) ->
                    ( model, shareAction { shareAsk | id = model.id, action = "link", password = model.share.password } )

                ( _, Just days ) ->
                    if days > 0 then
                        ( model
                        , shareAction
                            { shareAsk
                                | id = model.id
                                , action = "link"
                                , days = days
                                , password = model.share.password
                            }
                        )

                    else
                        ( { model | error = "A link has to live at least a day. Enter a whole number above 0, or leave it blank for 30." }, Cmd.none )

                ( typed, Nothing ) ->
                    ( { model | error = "\"" ++ typed ++ "\" is not a number of days. Enter a whole number, or leave it blank for 30." }, Cmd.none )

        ShareHook ->
            ( model, shareAction { shareAsk | id = model.id, action = "hook" } )

        SettingsClose ->
            -- model.id, not model.sheet.id: the second is set when the
            -- document loads, so closing the panel before it has would
            -- navigate back to the sheet you just left.
            ( { model | showSettings = False }, Nav.replaceUrl model.nav ("/" ++ model.id) )

        ShortcutsToggle show ->
            -- Neither this nor the palette opens over the import preview, the
            -- delete confirm or a held edit: closing the preview throws away
            -- the file it read, and closing a held edit throws away the edit.
            if show && (model.importing /= Nothing || model.deleteConfirm /= Nothing || model.pending /= Nothing) then
                ( model, Cmd.none )

            else
                let
                    ( closed, leave ) =
                        iif show (closeModals model) ( model, Cmd.none )
                in
                ( { closed | showShortcuts = show }, leave )

        FreshnessLoad value ->
            -- Reported, never swallowed. A column that quietly shows nothing
            -- because the answer changed shape is the failure this whole read
            -- exists to surface, wearing the face of a healthy library.
            case D.decodeValue freshnessDecoder value of
                Ok fresh ->
                    ( { model | freshness = fresh }, Cmd.none )

                Err err ->
                    ( { model | error = "The feed health answer arrived in a shape I could not read: " ++ D.errorToString err }, Cmd.none )

        LineageLoad value ->
            case ( model.pending, D.decodeValue (D.field "id" D.string) value ) of
                ( Just held, Ok id ) ->
                    -- loadLineage answers every ask, so an answer can arrive
                    -- after the edit it was for was cancelled and another held.
                    if id /= held.id || held.stage /= Asking then
                        ( model, Cmd.none )

                    else
                        let
                            found =
                                case D.decodeValue (D.field "error" D.string) value of
                                    Ok reason ->
                                        Err reason

                                    Err _ ->
                                        D.decodeValue (D.field "rows" lineageDecoder) value
                                            |> Result.mapError (\err -> "The answer arrived in a shape I could not read: " ++ D.errorToString err)
                                            |> Result.map (dependents held.id held.columns)
                        in
                        if found == Ok [] then
                            confirmPending held model

                        else
                            let
                                ( closed, leave ) =
                                    closeModals model
                            in
                            ( { closed | pending = Just { held | stage = Warning found } }, leave )

                ( Nothing, Ok _ ) ->
                    -- The answer to an edit already cancelled.
                    ( model, Cmd.none )

                ( _, Err err ) ->
                    ( { model
                        | pending = Nothing
                        , error = "Expected the dependents answer to name the sheet it is about, received one I could not read: " ++ D.errorToString err ++ " Nothing was written. Source: loadLineage in src/index.html. Fix: make the change again."
                      }
                    , Cmd.none
                    )

        PendingConfirm ->
            case model.pending of
                Just held ->
                    confirmPending held model

                Nothing ->
                    ( model, Cmd.none )

        PendingCancel ->
            ( { model | pending = Nothing }, Cmd.none )

        Goto id ->
            ( model, Nav.pushUrl model.nav ("/" ++ id) )

        PaletteToggle open ->
            -- Nothing is selected until an arrow or a query says so. It opened
            -- on the first row, so Enter on a palette nobody had pointed at ran
            -- whatever the shortcut sheet happened to list first -- two
            -- keystrokes, and the first row is a verb that deletes rows.
            if open && (model.importing /= Nothing || model.deleteConfirm /= Nothing || model.pending /= Nothing) then
                ( model, Cmd.none )

            else
                let
                    ( closed, leave ) =
                        iif open (closeModals model) ( model, Cmd.none )
                in
                ( { closed | palette = iif open (Just { query = "", selected = -1 }) Nothing }
                , Cmd.batch [ leave, iif open (Task.attempt (always NoOp) (Dom.focus "palette")) Cmd.none ]
                )

        PaletteNav delta ->
            case model.palette of
                Just p ->
                    let
                        shown =
                            List.length (paletteRows model p.query)

                        -- Nothing selected sits before the first row, so the
                        -- down arrow lands on that row and the up arrow wraps
                        -- to the last one.
                        from =
                            iif (p.selected < 0) (iif (delta < 0) 0 -1) p.selected

                        -- A real `if`: `iif` evaluates both arguments, and
                        -- Elm's `modBy 0` is a runtime crash, not a value.
                        selected =
                            if shown == 0 then
                                -1

                            else
                                modBy shown (from + delta)
                    in
                    ( { model | palette = Just { p | selected = selected } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        PaletteRun i ->
            -- A palette nobody has pointed at runs nothing. It cannot be left to
            -- the lookup: `List.drop` takes a negative count as none dropped, so
            -- -1 would run the first row.
            if i < 0 then
                ( model, Cmd.none )

            else
                case model.palette |> Maybe.andThen (\p -> paletteRows model p.query |> List.drop i |> List.head) of
                    Just command ->
                        -- Closed first, so a command that opens a dialog does not
                        -- open it behind the palette.
                        update command.run { model | palette = Nothing }

                    Nothing ->
                        ( model, Cmd.none )

        SettingsNameChange newName ->
            ( model, updateLibrary (Idd sheet.id { name = Just newName, tags = Nothing, trashed = Nothing, starred = Nothing }) )

        SettingsTagsChange newTags ->
            let
                tags =
                    newTags
                        |> String.split ","
                        |> List.map String.trim
                        |> List.filter (not << String.isEmpty)
            in
            ( model, updateLibrary (Idd sheet.id { name = Nothing, tags = Just tags, trashed = Nothing, starred = Nothing }) )

        DocNew x ->
            ( model, newDoc x )

        DocFork ->
            ( model, forkDoc sheet.id )

        ChartDownload format ->
            ( model, downloadChart format )

        DocNewTable ->
            advanceTutorial 0 ( model, newDoc <| E.object [ ( "type", E.string "table" ), ( "data", E.list identity [ E.list identity [ E.object [ ( "name", E.string "a" ), ( "type", E.string "text" ), ( "key", E.string "a" ) ] ] ] ) ] )

        DocNewQuery ->
            advanceTutorial 2 ( model, newDoc <| E.object [ ( "type", E.string "query" ), ( "data", E.list identity [ E.object [ ( "lang", E.string "sql" ), ( "code", E.string "select 1" ) ] ] ) ] )

        ShopFetch x ->
            ( { model | sheet = { sheet | table = Result.mapError (always "Something went wrong.") x } }, Cmd.none )

        CsvImportFile file ->
            ( model
            , Task.perform (CsvImportUpload (File.name file)) (File.toString file)
            )

        CsvImportUpload filename content ->
            ( model, importCsv { filename = filename, content = content } )

        ImportPreviewed value ->
            case D.decodeValue importingDecoder value of
                Ok importing ->
                    -- The file is read and sent while the page stays live, so
                    -- another modal can open before the preview lands.
                    let
                        ( closed, leave ) =
                            closeModals model
                    in
                    ( { closed | importing = Just importing }, leave )

                Err err ->
                    ( { model | error = "The import preview could not be read: " ++ D.errorToString err }, Cmd.none )

        ImportTypeChange name typ ->
            ( { model
                | importing =
                    model.importing
                        |> Maybe.map
                            (\imp ->
                                { imp
                                    | cols =
                                        List.map (\col -> iif (col.name == name) { col | typ = typ, remembered = False } col) imp.cols
                                }
                            )
              }
            , Cmd.none
            )

        ImportConfirm ->
            case model.importing of
                Just imp ->
                    ( { model | importing = Nothing }
                    , importConfirm { types = List.map (\col -> ( col.name, col.typ )) imp.cols }
                    )

                Nothing ->
                    ( model, Cmd.none )

        ImportCancel ->
            ( { model | importing = Nothing }, Cmd.none )

        InputChange SheetSearch x ->
            ( { model | search = x, sheet = { sheet | table = Err "" } }
            , Cmd.batch
                [ Nav.replaceUrl model.nav ("?q=" ++ Url.percentEncode x)
                , case sheet.doc of
                    Ok (Query query) ->
                        queryDoc (Idd sheet.id { lang = query.lang, code = query.code, cols = query.cols })

                    _ ->
                        Cmd.none
                ]
            )

        InputChange PaletteQuery x ->
            -- A new query renumbers the matches, so the selection returns to the
            -- first rather than pointing at whatever now sits at that index. An
            -- emptied box is the state a freshly opened palette is in and points
            -- at nothing: the unfiltered list starts with a verb that deletes
            -- rows, and typing a character and taking it back armed Enter on it
            -- again, which is the accident opening with -1 was for.
            ( { model
                | palette =
                    model.palette
                        |> Maybe.map (\p -> { p | query = x, selected = iif (String.isEmpty (String.trim x)) -1 0 })
              }
            , Cmd.none
            )

        InputChange CellWrite x ->
            ( { model | sheet = { sheet | write = Just x } }, Cmd.none )

        InputChange AuthEmail x ->
            ( { model | auth = { auth | email = x } }, Cmd.none )

        InputChange AuthPassword x ->
            ( { model | auth = { auth | password = x } }, Cmd.none )

        InputChange QueryCode x ->
            iif (String.contains "@" x) (advanceTutorial 3) identity <|
                ( model
                , changeDoc
                    { id = sheet.id
                    , data =
                        [ { action = "set"
                          , path = [ E.int 0, E.string "code" ]
                          , value = E.string x
                          }
                        ]
                    }
                )

        InputChange NetUrl x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data =
                    [ { action = "set"
                      , path = [ E.int 0, E.string "url" ]
                      , value = E.string x
                      }
                    ]
                }
            )

        InputChange NetInterval x ->
            case String.toInt x of
                Just n ->
                    ( model
                    , changeDoc
                        { id = sheet.id
                        , data =
                            [ { action = "set"
                              , path = [ E.int 0, E.string "interval" ]
                              , value = E.int n
                              }
                            ]
                        }
                    )

                Nothing ->
                    if String.isEmpty x then
                        ( model, Cmd.none )

                    else
                        ( { model | error = "The poll interval must be a whole number of seconds, got: " ++ x }
                        , Cmd.none
                        )

        InputChange NetPaused x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "paused" ], value = E.bool (x /= "") } ]
                }
            )

        InputChange NetCron x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "cron" ], value = E.string x } ]
                }
            )

        InputChange NetTimezone x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "timezone" ], value = E.string x } ]
                }
            )

        InputChange AlertCode x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "code" ], value = E.string x } ]
                }
            )

        InputChange AlertTo x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "to" ], value = E.string x } ]
                }
            )

        InputChange AlertDigest x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "digest" ], value = E.bool (x /= "") } ]
                }
            )

        InputChange AlertWhen x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "when" ], value = E.string x } ]
                }
            )

        InputChange AlertSnoozed x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "snoozed_until" ], value = E.string x } ]
                }
            )

        InputChange ChartSource x ->
            ( model, chartSet sheet.id "source" x )

        InputChange ChartKind x ->
            ( model, chartSet sheet.id "kind" x )

        InputChange ChartX x ->
            ( model, chartSet sheet.id "x" x )

        InputChange ChartY x ->
            ( model, chartSet sheet.id "y" x )

        InputChange ChartY2 x ->
            ( model, chartSet sheet.id "y2" x )

        InputChange ChartSeries x ->
            ( model, chartSet sheet.id "series" x )

        -- A list, so it cannot go through chartSet, which writes strings. One
        -- line is a day, whitespace, and whatever is left over as the label; a
        -- line with nothing after the day is a mark with no name, and a blank
        -- line is no mark, the way a dashboard's tiles read.
        InputChange ChartAnnotations x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data =
                    [ { action = "set"
                      , path = [ E.int 0, E.string "annotations" ]
                      , value =
                            x
                                |> String.lines
                                |> List.filterMap (String.trim >> parseAnnotation)
                                |> E.list
                                    (\( at, label ) ->
                                        E.object [ ( "at", E.string at ), ( "label", E.string label ) ]
                                    )
                      }
                    ]
                }
            )

        InputChange DashboardTiles x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data =
                    [ { action = "set"
                      , path = [ E.int 0, E.string "tiles" ]
                      , value = E.list E.string (x |> String.lines |> List.map String.trim |> List.filter (not << String.isEmpty))
                      }
                    ]
                }
            )

        InputChange NetHeaders x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data =
                    [ { action = "set"
                      , path = [ E.int 0, E.string "headers" ]
                      , value = E.string x
                      }
                    ]
                }
            )

        InputChange NetMethod x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "method" ], value = E.string x } ]
                }
            )

        InputChange NetBody x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "body" ], value = E.string x } ]
                }
            )

        InputChange NetPageBy x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "page_by" ], value = E.string x } ]
                }
            )

        InputChange NetPageParam x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "page_param" ], value = E.string x } ]
                }
            )

        InputChange NetPagePath x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "page_path" ], value = E.string x } ]
                }
            )

        InputChange NetMode x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "mode" ], value = E.string x } ]
                }
            )

        InputChange NetKey x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "key" ], value = E.string x } ]
                }
            )

        InputChange NetRowsPath x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data = [ { action = "set", path = [ E.int 0, E.string "rows_path" ], value = E.string x } ]
                }
            )

        CopyText str ->
            ( model, copyToClipboard str )

        TutorialDismiss ->
            ( { model | tutorial = Nothing }, saveTutorial -1 )

        CellMouseDoubleClick write ->
            let
                openEditor =
                    ( { model | sheet = { sheet | write = Just write } }
                    , Task.attempt (always NoOp) (Dom.focus "new-cell")
                    )
            in
            case sheet.doc of
                Ok Library ->
                    openEditor

                Ok (Tab table) ->
                    case Array.get sheet.hover.x table.cols |> Maybe.map .typ of
                        Just Boolean ->
                            ( model, Cmd.none )

                        _ ->
                            openEditor

                Ok _ ->
                    ( { model | error = computedCell }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CellMouseClick ->
            ( { model | sheet = { sheet | drag = False, select = Rect sheet.hover sheet.hover } }, Cmd.none )

        CellMouseDown ->
            ( { model | sheet = { sheet | drag = True, select = Rect sheet.hover sheet.hover } }, Cmd.none )

        CellMouseUp ->
            ( { model | sheet = { sheet | drag = False, select = Rect sheet.select.a sheet.hover } }, Cmd.none )

        CellHover hover ->
            let
                select =
                    sheet.select

                select_ =
                    iif sheet.drag { select | b = hover } select
            in
            ( { model | sheet = { sheet | hover = hover, select = select_ } }, Cmd.none )

        ColumnHide key ->
            arrange model { sheet | hidden = Set.insert key sheet.hidden, filterOpen = Nothing }

        ColumnsShowAll ->
            arrange model { sheet | hidden = Set.empty }

        ColumnSort shift key ->
            arrange model { sheet | sort = cycleSort shift key sheet.sort }

        ColumnDecimals key input ->
            -- An empty box is no count at all, and the count is clamped here
            -- rather than left to the input's own bounds: a paste does not
            -- respect those, and `fixed` scales by `10 ^ places`.
            --
            -- The model only, the way the filter box beside it works: closing
            -- the panel is when the count reaches the document. Typing "10" is
            -- two keystrokes, and an arrange each was a patch synced to every
            -- viewer for a 1 the typist never meant.
            ( { model
                | sheet =
                    { sheet
                        | decimals =
                            case String.toInt (String.trim input) of
                                Just places ->
                                    Dict.insert key (clamp 0 maxDecimals places) sheet.decimals

                                Nothing ->
                                    Dict.remove key sheet.decimals
                    }
              }
            , Cmd.none
            )

        ColumnFormat key name ->
            -- One click is one arrangement, unlike the count beside it: there
            -- are no keystrokes here to write a patch for each of.
            arrange model
                { sheet
                    | formats =
                        case numberFormat name of
                            Just format ->
                                Dict.insert key format sheet.formats

                            Nothing ->
                                Dict.remove key sheet.formats
                }

        ColumnShade key name ->
            -- One click is one arrangement, the way the format select above it
            -- is: a select has no keystrokes to write a patch for each of.
            arrange model
                { sheet
                    | shades =
                        case shade name of
                            Just shading ->
                                Dict.insert key shading sheet.shades

                            Nothing ->
                                Dict.remove key sheet.shades
                }

        ColumnPin key ->
            let
                pinning =
                    not (Set.member key sheet.pinned)

                -- A pinned column's left edge is the sum of the widths of the
                -- sticky columns before it, and a column that sizes itself has
                -- no width to add. Both get one written: the column being
                -- pinned, and column 0, which is sticky whether or not anybody
                -- pinned it. Guessing column 0's either leaves a gap that
                -- unpinned content scrolls through or slides the pinned column
                -- underneath it. Unpinning leaves every width alone, because by
                -- then it is a width somebody has been looking at.
                sized col widths =
                    iif (colPx sheet col == Nothing) (Dict.insert col.key autoColWidth widths) widths

                sticky =
                    List.filterMap identity
                        [ colOf sheet key
                        , arrangeable sheet |> Maybe.andThen (\target -> Array.get 0 target.cols)
                        ]
            in
            arrange model
                { sheet
                    | pinned = iif pinning (Set.insert key sheet.pinned) (Set.remove key sheet.pinned)
                    , widths = iif pinning (List.foldl sized sheet.widths sticky) sheet.widths
                    , filterOpen = Nothing
                }

        ColumnMoveStart key ->
            ( { model | sheet = { sheet | moving = Just (MovingCol key), filterOpen = Nothing } }, Cmd.none )

        RowMoveStart y ->
            ( { model | sheet = { sheet | moving = Just (MovingRow y), filterOpen = Nothing } }, Cmd.none )

        MoveEnd ->
            -- `dropOf` is the one place a drop becomes an edit; see it for why.
            let
                dropped =
                    { model | sheet = { sheet | moving = Nothing } }
            in
            case ( sheet.moving, sheet.doc ) of
                ( Just moving, Ok (Tab tbl) ) ->
                    let
                        -- A sort taken while the button was down: the row under
                        -- the pointer is a display row again, not a document row.
                        ordered =
                            case moving of
                                MovingRow _ ->
                                    inDocumentOrder model.search sheet tbl.rows

                                MovingCol _ ->
                                    True
                    in
                    iif ordered (dropOf moving sheet.hover tbl) Nothing
                        |> Maybe.map (\edit -> updateDocMsg edit dropped)
                        |> Maybe.withDefault ( dropped, Cmd.none )

                _ ->
                    ( dropped, Cmd.none )

        ColumnResizeStart key startX ->
            ( { model
                | sheet =
                    { sheet
                        | resizing =
                            Just
                                { key = key
                                , startX = startX
                                , startWidth = colOf sheet key |> Maybe.andThen (colPx sheet) |> Maybe.withDefault autoColWidth
                                }
                    }
              }
            , Cmd.none
            )

        ColumnResizeMove x ->
            case sheet.resizing of
                Just r ->
                    -- 32px floor: a column dragged to nothing cannot be grabbed again.
                    ( { model | sheet = { sheet | widths = Dict.insert r.key (max minColWidth (r.startWidth + x - r.startX)) sheet.widths } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ColumnResizeEnd ->
            -- Stored when the drag ends, not while it moves: one width per drag
            -- rather than one per pixel.
            arrange model { sheet | resizing = Nothing }

        FilterToggle key ->
            let
                newFilterOpen =
                    if sheet.filterOpen == Just key then
                        Nothing

                    else
                        Just key
            in
            -- Closing the panel is when the filter it holds reaches the
            -- document. The input writes to the model on every keystroke, and a
            -- patch per keystroke is a sync per keystroke for everyone watching.
            arrange model { sheet | filterOpen = newFilterOpen }

        FilterClear key ->
            arrange model
                { sheet
                    | filters = iif (key == "") Dict.empty (Dict.remove key sheet.filters)
                    , filterOpen = Nothing
                }

        FilterInput key value ->
            -- Create a TextContains filter from the input value
            if String.isEmpty value then
                ( { model | sheet = { sheet | filters = Dict.remove key sheet.filters } }, Cmd.none )

            else
                ( { model | sheet = { sheet | filters = Dict.insert key (TextContains value) sheet.filters } }, Cmd.none )

        FindOpen showReplace ->
            ( { model
                | sheet =
                    { sheet
                        | findReplace =
                            Just
                                { findText = ""
                                , replaceText = ""
                                , showReplace = showReplace
                                , matches = []
                                , currentMatch = 0
                                }
                    }
              }
            , Task.attempt (always NoOp) (Dom.focus "find-input")
            )

        FindClose ->
            ( { model | sheet = { sheet | findReplace = Nothing } }, Cmd.none )

        FindTextChange findText ->
            case sheet.findReplace of
                Just fr ->
                    let
                        -- Find all matching cells, in display coordinates so selection and highlighting line up
                        matches =
                            case sheet.doc of
                                Ok (Tab tbl) ->
                                    if String.isEmpty findText then
                                        []

                                    else
                                        filterAndSortIndexed model.search sheet tbl.rows
                                            |> Array.toIndexedList
                                            |> List.concatMap
                                                (\( dispIdx, ( _, row ) ) ->
                                                    tbl.cols
                                                        |> Array.toIndexedList
                                                        |> List.filterMap
                                                            (\( colIdx, col ) ->
                                                                let
                                                                    cellVal =
                                                                        Dict.get col.key row
                                                                            |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                                                                            |> Maybe.withDefault ""
                                                                in
                                                                if String.contains (String.toLower findText) (String.toLower cellVal) then
                                                                    Just (xy colIdx (dispIdx + 1))

                                                                else
                                                                    Nothing
                                                            )
                                                )

                                _ ->
                                    []

                        newFr =
                            { fr
                                | findText = findText
                                , matches = matches
                                , currentMatch = 0
                            }

                        -- Select the first match if any
                        newSelect =
                            case List.head matches of
                                Just idx ->
                                    Rect idx idx

                                Nothing ->
                                    sheet.select
                    in
                    ( { model | sheet = { sheet | findReplace = Just newFr, select = newSelect } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReplaceTextChange replaceText ->
            case sheet.findReplace of
                Just fr ->
                    ( { model | sheet = { sheet | findReplace = Just { fr | replaceText = replaceText } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FindNext ->
            case sheet.findReplace of
                Just fr ->
                    if List.isEmpty fr.matches then
                        ( model, Cmd.none )

                    else
                        let
                            nextIdx =
                                modBy (List.length fr.matches) (fr.currentMatch + 1)

                            nextMatch =
                                fr.matches |> List.drop nextIdx |> List.head

                            newSelect =
                                case nextMatch of
                                    Just idx ->
                                        Rect idx idx

                                    Nothing ->
                                        sheet.select
                        in
                        ( { model | sheet = { sheet | findReplace = Just { fr | currentMatch = nextIdx }, select = newSelect } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FindPrev ->
            case sheet.findReplace of
                Just fr ->
                    if List.isEmpty fr.matches then
                        ( model, Cmd.none )

                    else
                        let
                            len =
                                List.length fr.matches

                            prevIdx =
                                modBy len (fr.currentMatch - 1 + len)

                            prevMatch =
                                fr.matches |> List.drop prevIdx |> List.head

                            newSelect =
                                case prevMatch of
                                    Just idx ->
                                        Rect idx idx

                                    Nothing ->
                                        sheet.select
                        in
                        ( { model | sheet = { sheet | findReplace = Just { fr | currentMatch = prevIdx }, select = newSelect } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReplaceOne ->
            case ( sheet.findReplace, sheet.doc ) of
                ( Just fr, Ok (Tab tbl) ) ->
                    replaceMatches model sheet fr tbl (fr.matches |> List.drop fr.currentMatch |> List.take 1)

                _ ->
                    ( model, Cmd.none )

        ReplaceAll ->
            case ( sheet.findReplace, sheet.doc ) of
                ( Just fr, Ok (Tab tbl) ) ->
                    replaceMatches model sheet fr tbl fr.matches

                _ ->
                    ( model, Cmd.none )

        Undo ->
            case sheet.undoStack of
                [] ->
                    ( model, Cmd.none )

                entry :: rest ->
                    ( { model
                        | sheet =
                            { sheet
                                | undoStack = rest
                                , redoStack = entry :: sheet.redoStack
                            }
                      }
                    , changeDoc { id = sheet.id, data = entry.backward }
                    )

        Redo ->
            case sheet.redoStack of
                [] ->
                    ( model, Cmd.none )

                entry :: rest ->
                    ( { model
                        | sheet =
                            { sheet
                                | redoStack = rest
                                , undoStack = entry :: sheet.undoStack
                            }
                      }
                    , changeDoc { id = sheet.id, data = entry.forward }
                    )

        EditCommitMove dx dy ->
            -- Commit the in-progress edit to the current cell, then move the selection
            let
                ( committed, cmd ) =
                    update (DocMsg (SheetWrite sheet.select.a)) model

                cs =
                    committed.sheet

                bounds =
                    tableBounds committed

                sel =
                    cs.select.a

                newSel =
                    clampIndex bounds { x = sel.x + dx, y = sel.y + dy }
            in
            ( { committed | sheet = { cs | select = Rect newSel newSel } }, cmd )

        EditCancel ->
            ( { model | sheet = { sheet | write = Nothing } }, Cmd.none )

        KeyDown event ->
            updateKeyDown event model

        QueryEditorUpdate { textBeforeCursor } ->
            -- Handle special keyboard navigation signals
            case textBeforeCursor of
                "__NAV_DOWN__" ->
                    update (AutocompleteNav 1) model

                "__NAV_UP__" ->
                    update (AutocompleteNav -1) model

                "__SELECT__" ->
                    case sheet.queryAutocomplete of
                        Just ac ->
                            ac.suggestions
                                |> List.drop ac.selectedIndex
                                |> List.head
                                |> Maybe.map (\ref -> update (AutocompleteSelect ref) model)
                                |> Maybe.withDefault ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                "__CLOSE__" ->
                    update AutocompleteClose model

                _ ->
                    let
                        trigger =
                            completionTrigger textBeforeCursor

                        -- Asked for once per ref and kept: the answer runs a
                        -- describe through the page's engine, and asking again
                        -- on every keystroke would run it per character typed.
                        ask =
                            case Maybe.andThen completionRef trigger of
                                Just ref ->
                                    iif (Dict.member ref sheet.queryColumns) Cmd.none (columnsFor ref)

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | sheet = { sheet | queryAutocomplete = Maybe.map (completionAt model.library sheet.queryColumns) trigger } }, ask )

        ColumnsLoad { id, data } ->
            -- Held even when it is empty: a ref whose columns could not be read
            -- is a ref this editor must not ask about again on the next
            -- keystroke, and an empty list is exactly "nothing to suggest".
            --
            -- The open list is recomputed rather than left for the next
            -- keystroke to refresh: the columns are asked for by the very
            -- keystroke that would have shown them, so waiting would mean every
            -- completion appeared one character late.
            let
                known =
                    Dict.insert id data sheet.queryColumns
            in
            ( { model
                | sheet =
                    { sheet
                        | queryColumns = known
                        , queryAutocomplete = Maybe.map (completionAt model.library known << .trigger) sheet.queryAutocomplete
                    }
              }
            , Cmd.none
            )

        AutocompleteSelect ref ->
            -- Insert the selected reference and close autocomplete
            case sheet.queryAutocomplete of
                Just ac ->
                    let
                        -- Calculate what to insert (reference minus what's already typed)
                        toInsert =
                            String.dropLeft (String.length ac.trigger) ref
                    in
                    ( { model | sheet = { sheet | queryAutocomplete = Nothing } }
                    , insertAtCursor toInsert
                    )

                Nothing ->
                    ( model, Cmd.none )

        AutocompleteNav delta ->
            case sheet.queryAutocomplete of
                -- modBy 0 is a runtime error in Elm, and a list with nothing in
                -- it is drawn as no dropdown at all, so there is nothing to move
                -- through either.
                Just ac ->
                    let
                        newIndex =
                            modBy (max 1 (List.length ac.suggestions)) (ac.selectedIndex + delta)
                    in
                    ( { model | sheet = { sheet | queryAutocomplete = Just { ac | selectedIndex = newIndex } } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        AutocompleteClose ->
            ( { model | sheet = { sheet | queryAutocomplete = Nothing } }, Cmd.none )

        AuthMsg authMsg ->
            case authMsg of
                AuthSubmit ->
                    if String.isEmpty auth.password then
                        -- Signup flow: send verification email
                        ( { model | auth = { auth | state = LoggingIn } }
                        , signup auth.email
                        )

                    else
                        -- Login flow
                        ( { model | auth = { auth | state = LoggingIn } }
                        , login { email = auth.email, password = auth.password }
                        )

                AuthLogout ->
                    ( { model | auth = { state = Anonymous, email = "", password = "" } }
                    , logout ()
                    )

        AuthResult data ->
            let
                decoded =
                    D.decodeValue
                        (D.oneOf
                            [ D.field "usr_id" D.string |> D.map (\usrId -> LoggedIn { usrId = usrId })
                            , D.field "error" D.string |> D.map (\_ -> Anonymous)
                            , D.succeed Anonymous
                            ]
                        )
                        data

                -- The account's own address, which the page hands back with a
                -- restored session: logging in reloads, so what the form knew
                -- is gone. An answer that carries none leaves what was typed
                -- where it is, rather than emptying the form on a refusal.
                email =
                    D.decodeValue (D.field "email" D.string) data |> Result.withDefault auth.email
            in
            case decoded of
                Ok newState ->
                    ( { model | auth = { auth | state = newState, email = email, password = "" } }, Cmd.none )

                Err _ ->
                    ( { model | auth = { auth | state = Anonymous } }, Cmd.none )

        ClipboardCopy ->
            -- Copy selected cells to clipboard as TSV
            case sheet.doc of
                Ok (Tab tbl) ->
                    let
                        sel =
                            normalizeRect sheet.select

                        -- Extract selected cells as 2D list of strings (map display rows to document rows)
                        rows =
                            List.range sel.a.y sel.b.y
                                |> List.map
                                    (\y ->
                                        List.range sel.a.x sel.b.x
                                            |> List.map
                                                (\x ->
                                                    Array.get x tbl.cols
                                                        |> Maybe.andThen
                                                            (\col ->
                                                                displayYToDocY model.search sheet tbl.rows y
                                                                    |> Maybe.andThen (\docY -> Array.get (docY - 1) tbl.rows)
                                                                    |> Maybe.andThen (Dict.get col.key)
                                                                    |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                                                            )
                                                        |> Maybe.withDefault ""
                                                )
                                    )

                        tsv =
                            serializeToTsv rows
                    in
                    ( model, copyToClipboard tsv )

                _ ->
                    ( model, Cmd.none )

        ClipboardPaste text ->
            -- Refused under a past version, for the reason `updateKeyDown` gives.
            if model.history == Nothing then
                updatePaste text model

            else
                ( model, Cmd.none )

        SelectAll ->
            ( { model | sheet = { sheet | select = selectAll (tableBounds model) } }, Cmd.none )



---- VIEW ---------------------------------------------------------------------


{-| The held edit, replayed through `updateDocMsg` against the document as it
is now, so every refusal it would have met it meets again.
-}
confirmPending : Pending -> Model -> ( Model, Cmd Msg )
confirmPending held ({ sheet } as model) =
    if held.id /= sheet.id then
        ( { model
            | pending = Nothing
            , error = "Expected the rename or delete to land on " ++ held.id ++ ", received it with " ++ sheet.id ++ " open. Nothing was written. Source: the sheet changed while its dependents were read. Fix: open " ++ held.id ++ " and make the change again."
          }
        , Cmd.none
        )

    else
        let
            ( written, cmd ) =
                updateDocMsg held.edit { model | pending = Just { held | stage = Confirmed }, sheet = { sheet | write = held.write } }
        in
        ( { written | pending = Nothing }, cmd )


updateDocMsg : DocMsg -> Model -> ( Model, Cmd Msg )
updateDocMsg edit ({ sheet } as model) =
    case sheet.doc of
        Ok Library ->
            let
                closed =
                    { model | sheet = { sheet | write = Nothing } }
            in
            case edit of
                SheetWrite { x, y } ->
                    case ( libraryIdAtRow model y, Maybe.map .name (Array.get x (libraryCols model)) ) of
                        ( Just id, Just "name" ) ->
                            ( closed, updateLibrary (Idd id { name = sheet.write, tags = Nothing, trashed = Nothing, starred = Nothing }) )

                        ( Just id, Just "tags" ) ->
                            ( closed, updateLibrary (Idd id { name = Nothing, tags = sheet.write |> Maybe.map (String.split ", " >> List.map String.trim), trashed = Nothing, starred = Nothing }) )

                        ( Nothing, _ ) ->
                            -- Written to a row the library does not draw. The empty
                            -- id is the library's own entry, so it must not stand in.
                            ( { closed | error = "Library row " ++ String.fromInt y ++ " is not a sheet. Reload the page and try again." }, Cmd.none )

                        _ ->
                            ( closed, Cmd.none )

                _ ->
                    -- Every other edit, on the one sheet that is a listing and
                    -- not a document: a silent nothing here read as a verb that
                    -- had run, and the palette offers them from the library
                    -- more than from anywhere else.
                    ( { closed | error = "Expected an edit to a table, received one on the library, which lists your sheets rather than holding rows of its own. Source: a keystroke or the command palette. Fix: open the sheet you meant and run it there." }, Cmd.none )

        Ok (Tab table) ->
            let
                -- Map a display row coordinate to its document row coordinate (accounts for sort/filter/search)
                toDoc : Int -> Maybe Int
                toDoc y =
                    displayYToDocY model.search sheet table.rows y

                -- The display rows this edit names that no drawn row stands
                -- behind. Any one refuses the whole edit, so the patches below
                -- skip them only on a path that is never sent.
                undrawn =
                    (case edit of
                        SheetWrite { y } ->
                            iif (y >= 1 && sheet.write /= Nothing) [ y ] []

                        SheetRowInsert indices ->
                            indices

                        SheetRowDuplicate indices ->
                            indices

                        SheetRowDelete indices ->
                            indices

                        SheetFillDown r ->
                            List.range (max 1 (normalizeRect r).a.y) (normalizeRect r).b.y

                        SheetClearCells indices ->
                            List.map .y indices

                        CellCheck i _ ->
                            [ i.y ]

                        SheetRowPush ->
                            []

                        SheetColumnPush ->
                            []

                        SheetColumnDelete _ ->
                            []

                        SheetColumnMove _ _ ->
                            []

                        SheetRowMove _ _ ->
                            []

                        SheetColumnTrim _ ->
                            []

                        SheetColumnCase _ _ ->
                            []

                        SheetRowsDropBlank _ ->
                            []

                        SheetRowsDedupe ->
                            []

                        SheetRowsDedupeNear _ _ ->
                            []

                        SheetColumnSplit _ _ ->
                            []
                    )
                        |> List.filter (\y -> toDoc y == Nothing)

                -- The names this edit takes away from a sheet that reads them.
                atRisk =
                    case edit of
                        SheetWrite { x, y } ->
                            case ( y, Array.get x table.cols, sheet.write ) of
                                ( 0, Just col, Just write ) ->
                                    iif (write == col.name) [] [ col.name ]

                                _ ->
                                    []

                        SheetColumnDelete indices ->
                            List.filterMap (\i -> Array.get i table.cols |> Maybe.map .name) indices

                        SheetRowPush ->
                            []

                        SheetColumnPush ->
                            []

                        SheetRowInsert _ ->
                            []

                        SheetRowDuplicate _ ->
                            []

                        SheetRowDelete _ ->
                            []

                        SheetColumnMove _ _ ->
                            []

                        SheetRowMove _ _ ->
                            []

                        SheetClearCells _ ->
                            []

                        SheetFillDown _ ->
                            []

                        SheetColumnTrim _ ->
                            []

                        SheetColumnCase _ _ ->
                            []

                        SheetRowsDropBlank _ ->
                            []

                        SheetRowsDedupe ->
                            []

                        SheetRowsDedupeNear _ _ ->
                            []

                        SheetColumnSplit _ _ ->
                            []

                        CellCheck _ _ ->
                            []

                -- Helper to get old cell value as E.Value (rowIdx is a document coordinate)
                getOldValue : Int -> String -> E.Value
                getOldValue rowIdx key =
                    table.rows
                        |> Array.get (rowIdx - 1)
                        |> Maybe.andThen (Dict.get key)
                        |> Maybe.map (\v -> D.decodeValue D.value v |> Result.withDefault E.null)
                        |> Maybe.withDefault E.null

                -- A column addressed by its own key rather than by a position,
                -- the way every other control in its panel addresses it. A key
                -- the sheet no longer carries writes nothing.
                withColumn key make =
                    table.cols
                        |> Array.toList
                        |> List.filter (\c -> c.key == key)
                        |> List.head
                        |> Maybe.map make
                        |> Maybe.withDefault ( [], [] )

                -- The split's patches, or why it cannot run. Computed once,
                -- because the refusal below and the patches above are the two
                -- halves of one answer.
                split =
                    case edit of
                        SheetColumnSplit key delimiter ->
                            columnSplit table.cols table.rows key delimiter

                        _ ->
                            Ok ( [], [] )

                -- The near-duplicate rows, or why they could not be looked for.
                -- Computed once, for the reason the split above is: the refusal
                -- and the patches are two halves of one answer. `Ok []` is not a
                -- refusal here, it is a verb with nothing to do -- so it is
                -- turned into one by name, since a button that deletes nothing
                -- and says nothing reads as a button that is broken.
                near =
                    case edit of
                        SheetRowsDedupeNear key closeness ->
                            Array.filter (\col -> col.key == key) table.cols
                                |> Array.get 0
                                |> Result.fromMaybe ("This sheet has no column keyed " ++ key ++ ".")
                                |> Result.andThen (\col -> nearDuplicates col closeness table.rows)
                                |> Result.andThen
                                    (\found ->
                                        iif (List.isEmpty found)
                                            (Err
                                                ("No two rows are within "
                                                    ++ String.fromInt closeness
                                                    ++ "% of each other in this column. Source: the near box. Fix: lower the percentage, or leave the sheet as it is."
                                                )
                                            )
                                            (Ok (rowDeletions table.rows (List.map (\( goes, _, _ ) -> goes) found)))
                                    )

                        _ ->
                            Ok ( [], [] )

                -- Compute forward and backward patches based on edit type
                ( forwardPatches, backwardPatches ) =
                    case edit of
                        SheetWrite { x, y } ->
                            case ( max 0 y, Array.get x table.cols ) of
                                ( 0, Just col ) ->
                                    -- The column's name (row 0) or its type (row -1).
                                    --
                                    -- One field, never a rebuilt column object. Replacing the
                                    -- whole object made a rename rewrite the type beside it, so
                                    -- editing a name turned `int` into `num` and `float` into a
                                    -- spelling the decoder did not know. It also let two people
                                    -- editing one column's name and type clobber each other,
                                    -- where two field writes merge.
                                    let
                                        fieldName =
                                            iif (y == -1) "type" "name"

                                        setField value =
                                            [ { action = "set"
                                              , path = [ E.int 0, E.string (String.fromInt x), E.string fieldName ]
                                              , value = E.string value
                                              }
                                            ]

                                        forward =
                                            sheet.write |> Maybe.map setField |> Maybe.withDefault []

                                        backward =
                                            setField (iif (y == -1) col.raw col.name)
                                    in
                                    ( forward, backward )

                                ( rowY, Just col ) ->
                                    -- Editing data cell (translate display row to document row).
                                    -- No write means a cancelled edit or a blur after cancel: leave the cell untouched.
                                    case sheet.write of
                                        Nothing ->
                                            ( [], [] )

                                        Just write ->
                                            case toDoc rowY of
                                                Just docY ->
                                                    ( [ { action = "set"
                                                        , path = [ E.int docY, E.string col.key ]
                                                        , value = E.string write
                                                        }
                                                      ]
                                                    , [ { action = "set"
                                                        , path = [ E.int docY, E.string col.key ]
                                                        , value = getOldValue docY col.key
                                                        }
                                                      ]
                                                    )

                                                Nothing ->
                                                    ( [], [] )

                                _ ->
                                    ( [], [] )

                        SheetRowPush ->
                            let
                                rowCount =
                                    Array.length table.rows + 1

                                forward =
                                    [ { action = "push"
                                      , path = []
                                      , value = E.list identity [ E.object [] ]
                                      }
                                    ]

                                backward =
                                    [ { action = "splice"
                                      , path = []
                                      , value = E.list E.int [ rowCount, 1 ]
                                      }
                                    ]
                            in
                            ( forward, backward )

                        SheetColumnPush ->
                            let
                                colCount =
                                    Array.length table.cols

                                forward =
                                    [ { action = "push"
                                      , path = [ E.int 0 ]
                                      , value = E.list identity [ E.object [ ( "name", E.string "" ), ( "type", E.string "text" ), ( "key", E.int colCount ) ] ]
                                      }
                                    ]

                                backward =
                                    [ { action = "splice"
                                      , path = [ E.int 0 ]
                                      , value = E.list E.int [ colCount, 1 ]
                                      }
                                    ]
                            in
                            ( forward, backward )

                        SheetRowInsert indices ->
                            rowSplices (\_ -> Just Dict.empty) 0 indices toDoc

                        SheetRowDuplicate indices ->
                            rowSplices (\i -> Array.get (i - 1) table.rows) 1 indices toDoc

                        SheetRowDelete indices ->
                            rowDeletions table.rows (List.filterMap toDoc indices)

                        SheetColumnTrim key ->
                            withColumn key (\col -> cellRewrites col String.trim table.rows)

                        SheetColumnCase key casing ->
                            withColumn key (\col -> cellRewrites col (iif (casing == Upper) String.toUpper String.toLower) table.rows)

                        SheetRowsDropBlank key ->
                            withColumn key (\col -> rowDeletions table.rows (blankRows col table.rows))

                        SheetRowsDedupe ->
                            rowDeletions table.rows (duplicateRows table.rows)

                        SheetRowsDedupeNear _ _ ->
                            -- The empty pair is never what is written, for the
                            -- reason the split's is not: every way this says no
                            -- is a refusal answered before a patch goes out.
                            Result.withDefault ( [], [] ) near

                        SheetColumnSplit _ _ ->
                            -- The empty pair is never what is written: every way
                            -- `columnSplit` says no is a refusal below, and a
                            -- refusal is answered before any patch is sent.
                            Result.withDefault ( [], [] ) split

                        SheetColumnMove from to ->
                            -- Its own inverse, which is the whole reason a move
                            -- is one patch: a splice out and a splice back in
                            -- would have to rebuild the column to undo it, and
                            -- `Col` does not carry all of one.
                            ( [ movePatch [ 0 ] from to ], [ movePatch [ 0 ] to from ] )

                        -- Like the column move, a delete and an insert to
                        -- automerge: a collaborator's concurrent edit to that
                        -- row's cells lands on the object that was deleted.
                        SheetRowMove from to ->
                            ( [ movePatch [] from to ], [ movePatch [] to from ] )

                        SheetColumnDelete indices ->
                            let
                                colKeys =
                                    indices
                                        |> List.filterMap (\i -> Array.get i table.cols)
                                        |> List.map .key

                                colPatches =
                                    indices
                                        |> List.sort
                                        |> List.reverse
                                        |> List.map
                                            (\i ->
                                                { action = "splice"
                                                , path = [ E.int 0 ]
                                                , value = E.list E.int [ i, 1 ]
                                                }
                                            )

                                rowPatches =
                                    table.rows
                                        |> Array.toIndexedList
                                        |> List.concatMap
                                            (\( rowIdx, _ ) ->
                                                colKeys
                                                    |> List.map
                                                        (\key ->
                                                            { action = "del"
                                                            , path = [ E.int (rowIdx + 1), E.string key ]
                                                            , value = E.null
                                                            }
                                                        )
                                            )

                                -- Put each column definition back at its own index
                                -- (lowest first), then restore the cells it held.
                                backColPatches =
                                    indices
                                        |> List.sort
                                        |> List.filterMap
                                            (\i ->
                                                Array.get i table.cols
                                                    |> Maybe.map
                                                        (\col ->
                                                            { action = "splice"
                                                            , path = [ E.int 0 ]
                                                            , value =
                                                                E.list identity
                                                                    [ E.int i
                                                                    , E.int 0
                                                                    , E.object
                                                                        [ ( "name", E.string col.name )
                                                                        , ( "type", E.string col.raw )
                                                                        , ( "key", E.string col.key )
                                                                        ]
                                                                    ]
                                                            }
                                                        )
                                            )

                                backRowPatches =
                                    table.rows
                                        |> Array.toIndexedList
                                        |> List.concatMap
                                            (\( rowIdx, row ) ->
                                                colKeys
                                                    |> List.filterMap
                                                        (\key ->
                                                            Dict.get key row
                                                                |> Maybe.map
                                                                    (\v ->
                                                                        { action = "set"
                                                                        , path = [ E.int (rowIdx + 1), E.string key ]
                                                                        , value = v
                                                                        }
                                                                    )
                                                        )
                                            )
                            in
                            ( colPatches ++ rowPatches, backColPatches ++ backRowPatches )

                        SheetFillDown r ->
                            let
                                norm =
                                    normalizeRect r

                                -- The seeds start at the top data row of the selection: a
                                -- rect that starts on the header or type row fills from row 1.
                                top =
                                    max 1 norm.a.y

                                rows =
                                    List.range top norm.b.y |> List.filterMap toDoc

                                patchPairs =
                                    List.range norm.a.x norm.b.x
                                        |> List.concatMap
                                            (\x ->
                                                case Array.get x table.cols of
                                                    Nothing ->
                                                        []

                                                    Just col ->
                                                        let
                                                            -- Blank is what `blankCell` says it is, never
                                                            -- what `cellText` trims to: a JSON null reads
                                                            -- as the word "NULL", which is what an
                                                            -- imported CSV writes for every gap in a
                                                            -- numeric column. The fold below stopped at
                                                            -- nothing, so [10, 20, null, null] seeded
                                                            -- "NULL" and filled the column with a word
                                                            -- its own type does not allow.
                                                            texts =
                                                                rows
                                                                    |> List.map
                                                                        (\docY ->
                                                                            Array.get (docY - 1) table.rows
                                                                                |> Maybe.map (\row -> iif (blankCell col.key row) "" (cellText col.key row))
                                                                                |> Maybe.withDefault ""
                                                                        )

                                                            -- The seeds are the selection's leading run of
                                                            -- filled cells, never the whole of it, so the
                                                            -- fill always has a row to land on. Fewer than
                                                            -- two of them repeats the top cell, which is
                                                            -- what fill-down always did.
                                                            seeds =
                                                                texts
                                                                    |> List.foldl (\t ( stopped, n ) -> iif (stopped || String.trim t == "") ( True, n ) ( False, n + 1 )) ( False, 0 )
                                                                    |> Tuple.second
                                                                    |> min (List.length rows - 1)
                                                                    |> (\n -> List.take n texts)

                                                            -- Two seeds are what a step is made of, except
                                                            -- a date: one date is a series on its own,
                                                            -- because a day is the step nobody has to name.
                                                            enough =
                                                                case seeds of
                                                                    [ one ] ->
                                                                        parseDay one /= Nothing

                                                                    _ ->
                                                                        List.length seeds >= 2

                                                            values =
                                                                case ( enough, seriesEncoder col.typ ) of
                                                                    ( True, Just encode ) ->
                                                                        fillSeries seeds (List.length rows - List.length seeds) |> List.map encode

                                                                    _ ->
                                                                        case rows of
                                                                            first :: _ ->
                                                                                List.repeat (List.length rows - 1) (getOldValue first col.key)

                                                                            [] ->
                                                                                []
                                                        in
                                                        List.map2 Tuple.pair (List.drop (List.length rows - List.length values) rows) values
                                                            |> List.map
                                                                (\( docY, value ) ->
                                                                    ( { action = "set"
                                                                      , path = [ E.int docY, E.string col.key ]
                                                                      , value = value
                                                                      }
                                                                    , { action = "set"
                                                                      , path = [ E.int docY, E.string col.key ]
                                                                      , value = getOldValue docY col.key
                                                                      }
                                                                    )
                                                                )
                                            )

                                forward =
                                    List.map Tuple.first patchPairs

                                backward =
                                    List.map Tuple.second patchPairs
                            in
                            ( forward, backward )

                        SheetClearCells indices ->
                            let
                                patchPairs =
                                    indices
                                        |> List.filterMap
                                            (\idx ->
                                                Maybe.map2
                                                    (\col docY ->
                                                        ( { action = "set"
                                                          , path = [ E.int docY, E.string col.key ]
                                                          , value = E.string ""
                                                          }
                                                        , { action = "set"
                                                          , path = [ E.int docY, E.string col.key ]
                                                          , value = getOldValue docY col.key
                                                          }
                                                        )
                                                    )
                                                    (Array.get idx.x table.cols)
                                                    (toDoc idx.y)
                                            )

                                forward =
                                    List.map Tuple.first patchPairs

                                backward =
                                    List.map Tuple.second patchPairs
                            in
                            ( forward, backward )

                        CellCheck i c ->
                            case ( Array.get i.x table.cols, toDoc i.y ) of
                                ( Just col, Just docY ) ->
                                    let
                                        oldValue =
                                            getOldValue docY col.key

                                        forward =
                                            [ { action = "set"
                                              , path = [ E.int docY, E.string col.key ]
                                              , value = E.bool c
                                              }
                                            ]

                                        backward =
                                            [ { action = "set"
                                              , path = [ E.int docY, E.string col.key ]
                                              , value = oldValue
                                              }
                                            ]
                                    in
                                    ( forward, backward )

                                _ ->
                                    ( [], [] )

                -- Why this edit cannot be stored, if it cannot. The two header
                -- ones are about the same cell: y is 0 for a rename and -1 for a
                -- type, which is not a name at all.
                writeRefusal =
                    case ( edit, sheet.write ) of
                        ( SheetColumnSplit _ _, _ ) ->
                            case split of
                                Err message ->
                                    Just message

                                Ok _ ->
                                    Nothing

                        ( SheetRowsDedupeNear _ _, _ ) ->
                            case near of
                                Err message ->
                                    Just message

                                Ok _ ->
                                    Nothing

                        ( SheetWrite { x, y }, Just write ) ->
                            if y == 0 then
                                nameClash table.cols x write
                                    |> Maybe.map
                                        (\name ->
                                            "This sheet already has a column called \"" ++ name ++ "\". Two columns of one name have no row a reader can key, so every export and every query over this sheet would refuse it."
                                        )

                            else if
                                (y == -1)
                                    && ((Array.get x table.cols |> Maybe.map .raw) /= Just write)
                                    && not (knownTypeName write)
                            then
                                Just <|
                                    -- Not a refusal when the value is what the
                                    -- column already says: an alias is readable,
                                    -- so opening the type row on a `pct` column
                                    -- and clicking away must not be an error.
                                    "I do not know a column type called \""
                                        ++ write
                                        ++ "\". A type nobody knows reads back as unknown, and a numeric column that reads back as unknown stops being checked -- its blanks start summing as zeros. Known types: "
                                        ++ String.join ", " canonicalTypeNames
                                        ++ ", or enum: followed by the options, e.g. enum:small,large."

                            else
                                Nothing

                        _ ->
                            Nothing

                -- Update undo stack if we have patches to track
                newUndoStack =
                    if List.isEmpty forwardPatches || List.isEmpty backwardPatches then
                        sheet.undoStack

                    else
                        { forward = forwardPatches, backward = backwardPatches } :: sheet.undoStack |> List.take 50

                -- Clear redo stack on new changes (unless no undo tracking)
                newRedoStack =
                    if List.isEmpty backwardPatches then
                        sheet.redoStack

                    else
                        []
            in
            case iif (List.isEmpty undrawn) writeRefusal (Just (undrawnRows undrawn)) of
                Just message ->
                    ( { model | sheet = { sheet | write = Nothing }, error = message }, Cmd.none )

                Nothing ->
                    let
                        refuse message =
                            ( { model | sheet = { sheet | write = Nothing }, error = message }, Cmd.none )

                        busy =
                            "Expected one open question at a time, received a rename or a delete while another dialog is open or a check on who reads this sheet is still out. Nothing was written. Source: a keystroke behind an open dialog. Fix: answer or close that dialog, then make the change again."

                        written =
                            if List.isEmpty forwardPatches then
                                ( { model | sheet = { sheet | write = Nothing } }, Cmd.none )

                            else
                                advanceTutorial 1
                                    ( { model
                                        | sheet =
                                            { sheet
                                                | write = Nothing
                                                , undoStack = newUndoStack
                                                , redoStack = newRedoStack
                                            }
                                      }
                                    , changeDoc { id = sheet.id, data = forwardPatches }
                                    )
                    in
                    case ( model.pending, atRisk ) of
                        ( Just held, _ ) ->
                            if held.stage == Confirmed then
                                if held.columns == atRisk then
                                    written

                                else
                                    refuse
                                        ("Expected to change "
                                            ++ String.join ", " (List.map quoted held.columns)
                                            ++ ", received "
                                            ++ iif (List.isEmpty atRisk) "no column" (String.join ", " (List.map quoted atRisk))
                                            ++ " where those were. Nothing was written. Source: the columns changed while their dependents were read. Fix: make the change again."
                                        )

                            else if List.isEmpty atRisk then
                                written

                            else
                                refuse busy

                        ( Nothing, [] ) ->
                            written

                        ( Nothing, _ ) ->
                            if model.importing /= Nothing || model.deleteConfirm /= Nothing || model.history /= Nothing then
                                refuse busy

                            else
                                ( { model
                                    | sheet = { sheet | write = Nothing }
                                    , pending = Just { id = sheet.id, edit = edit, write = sheet.write, columns = atRisk, stage = Asking }
                                  }
                                , lineageFor sheet.id
                                )

        Ok _ ->
            -- Every edit the keyboard offers, on a sheet nobody types into.
            ( { model | error = computedCell, sheet = { sheet | write = Nothing } }, Cmd.none )

        Err _ ->
            ( { model | sheet = { sheet | write = Nothing } }, Cmd.none )


{-| No query reads these on either host: the page's engine takes only table and
query refs, and the server refuses a template and a portal and reads a codex
through its own route.
-}
unviewable : String -> String -> String
unviewable typ id =
    typ
        ++ " sheets sync but have no view yet, and a query cannot read one. "
        ++ (if String.startsWith "codex-" typ then
                "GET /codex/" ++ id ++ " answers its tables."

            else if typ == "portal" then
                "Read it live over GET /portal/" ++ (String.split ":" id |> List.drop 1 |> String.join ":") ++ "/sync."

            else
                "Buy it from the shop, then open the copy you get back."
           )


updateHistory : HistoryMsg -> Model -> ( Model, Cmd Msg )
updateHistory msg model =
    case msg of
        HistoryOpen ->
            -- Not over the import preview, the delete confirm or a held edit,
            -- for the reason `ShortcutsToggle` gives.
            if model.importing /= Nothing || model.deleteConfirm /= Nothing || model.pending /= Nothing then
                ( model, Cmd.none )

            else
                let
                    ( closed, leave ) =
                        closeModals model
                in
                ( { closed | history = Just { id = model.sheet.id, versions = Nothing, left = 0, hash = Nothing, past = Nothing } }
                , Cmd.batch [ leave, historyLoad model.sheet.id ]
                )

        HistoryLoad data ->
            -- An answer lands only on the history it was asked for, because the
            -- sheet may have changed while it was in flight.
            ( { model
                | history =
                    Maybe.map
                        (\h ->
                            if data.id /= h.id then
                                h

                            else
                                let
                                    version =
                                        D.map5 Version
                                            (D.field "hash" D.string)
                                            (D.field "time" D.int)
                                            (D.field "actor" D.string)
                                            (D.field "seq" D.int)
                                            (D.field "message" (D.nullable D.string))
                                in
                                case D.decodeValue (D.map2 Tuple.pair (D.field "versions" (D.list version)) (D.field "left" D.int)) data.data of
                                    Ok ( versions, left ) ->
                                        { h | versions = Just (Ok versions), left = left }

                                    Err err ->
                                        { h | versions = Just (Err ("The sheet's history arrived in a shape I could not read: " ++ D.errorToString err)) }
                        )
                        model.history
              }
            , Cmd.none
            )

        HistoryPick hash ->
            case model.history of
                Just h ->
                    ( { model | history = Just { h | hash = Just hash, past = Nothing } }, historyView { id = h.id, hash = hash } )

                Nothing ->
                    ( model, Cmd.none )

        HistoryShow data ->
            -- By sheet, for the reason `HistoryLoad` gives, and by version,
            -- because another one may have been picked while it was in flight.
            ( { model
                | history =
                    Maybe.map
                        (\h ->
                            if data.id /= h.id then
                                h

                            else
                                let
                                    -- A cell reads the way `cellText` reads one, so a
                                    -- missing cell is the blank the live table draws for it.
                                    past =
                                        D.map3 (\hash columns rows -> ( hash, { columns = columns, rows = rows } ))
                                            (D.field "hash" D.string)
                                            (D.field "columns" (D.list D.string))
                                            (D.field "rows" (D.list (D.list (D.map (D.decodeValue string >> Result.withDefault "") D.value))))
                                in
                                case D.decodeValue past data.data of
                                    Ok ( hash, shown ) ->
                                        iif (h.hash == Just hash) { h | past = Just (Ok shown) } h

                                    Err err ->
                                        { h | past = Just (Err ("A past version arrived in a shape I could not read: " ++ D.errorToString err)) }
                        )
                        model.history
              }
            , Cmd.none
            )

        HistoryClose ->
            ( { model | history = Nothing }, Cmd.none )


updateKeyDown : KeyEvent -> Model -> ( Model, Cmd Msg )
updateKeyDown event ({ sheet } as model) =
    -- A past version is drawn over the live sheet, and every key below writes
    -- to the live one. So under it Escape closes it and no other key does a thing.
    if model.history /= Nothing then
        if event.key == "Escape" then
            updateHistory HistoryClose model

        else
            ( model, Cmd.none )

    else if (event.ctrl || event.meta) && event.key == "k" then
        update (PaletteToggle (model.palette == Nothing)) model

    else if (event.ctrl || event.meta) && event.key == "/" then
        update (ShortcutsToggle (not model.showShortcuts)) model

    else if
        event.key
            == "?"
            && (case sheet.doc of
                    Ok (Tab _) ->
                        False

                    _ ->
                        True
               )
    then
        update (ShortcutsToggle True) model

    else if (event.ctrl || event.meta) && event.key == "f" then
        update (FindOpen False) model

    else if (event.ctrl || event.meta) && event.key == "h" then
        update (FindOpen True) model

    else if (event.ctrl || event.meta) && String.toLower event.key == "d" && event.shift then
        -- The whole sheet, not the selection, which is why it is here and in the
        -- palette rather than in a column's panel. Lowercased because a browser
        -- reports the shifted key as "D".
        update (DocMsg SheetRowsDedupe) model

    else if (event.ctrl || event.meta) && event.key == "Backspace" && event.shift then
        -- Here rather than beside Ctrl/⌘+Backspace in the navigation case: that
        -- one deletes columns and this one is not an edit at all, so it must
        -- reach the library, whose every DocMsg is refused.
        update TrashSelected model

    else if (event.ctrl || event.meta) && event.key == "d" then
        update (DocMsg (SheetFillDown sheet.select)) model

    else if event.key == "Escape" && model.palette /= Nothing then
        update (PaletteToggle False) model

    else if event.key == "Escape" && model.showShortcuts then
        update (ShortcutsToggle False) model

    else if event.key == "Escape" && model.showSettings then
        update SettingsClose model

    else if event.key == "Escape" && model.deleteConfirm /= Nothing then
        update DocDeleteCancel model

    else if event.key == "Escape" && model.pending /= Nothing then
        update PendingCancel model

    else if event.key == "Escape" && sheet.filterOpen /= Nothing then
        -- Escape closes the panel, which is one of the moments the filter it
        -- holds is stored. It does not clear the filter -- that is what the
        -- panel's own Clear is for -- so what is stored is what is on screen.
        arrange model { sheet | filterOpen = Nothing }

    else if event.key == "Escape" && sheet.findReplace /= Nothing then
        update FindClose model

    else if event.key == "Enter" && sheet.findReplace /= Nothing then
        update FindNext model

    else if (event.ctrl || event.meta) && event.key == "z" && not event.shift then
        update Undo model

    else if (event.ctrl || event.meta) && event.key == "z" && event.shift then
        update Redo model

    else if (event.ctrl || event.meta) && event.key == "y" then
        update Redo model

    else
        let
            sel =
                sheet.select.a

            bounds =
                tableBounds model

            -- Move selection, clamping to bounds and stepping over hidden columns
            move : Int -> Int -> ( Model, Cmd Msg )
            move dx dy =
                let
                    clamped =
                        clampIndex bounds { x = sel.x + dx, y = sel.y + dy }

                    newSel =
                        xy (skipHidden sheet bounds dx clamped.x) clamped.y
                in
                ( { model | sheet = { sheet | select = Rect newSel newSel } }
                , Cmd.none
                )

            -- Start editing the current cell
            startEdit =
                case sheet.doc of
                    Ok (Tab tbl) ->
                        let
                            col =
                                Array.get sel.x tbl.cols

                            row =
                                displayYToDocY model.search sheet tbl.rows sel.y |> Maybe.andThen (\docY -> Array.get (docY - 1) tbl.rows)
                        in
                        case ( col, row ) of
                            ( Just c, Just r ) ->
                                let
                                    val =
                                        r
                                            |> Dict.get c.key
                                            |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                                            |> Maybe.withDefault ""
                                in
                                ( { model | sheet = { sheet | write = Just val } }
                                , Task.attempt (always NoOp) (Dom.focus "new-cell")
                                )

                            ( Just _, Nothing ) ->
                                iif (sel.y >= 1) ( { model | error = undrawnRows [ sel.y ] }, Cmd.none ) ( model, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    Ok Library ->
                        libraryIdAtRow model sel.y
                            |> Maybe.map (\id -> update (Goto id) model)
                            |> Maybe.withDefault ( model, Cmd.none )

                    Ok _ ->
                        ( { model | error = computedCell }, Cmd.none )

                    Err _ ->
                        ( model, Cmd.none )
        in
        if sel.x < 0 || sel.y < 0 then
            -- No selection yet, ignore navigation
            ( model, Cmd.none )

        else
            -- When not editing, navigate with keys
            let
                -- Expand selection instead of moving when shift is held
                expand dx dy =
                    let
                        newSelect =
                            expandSelection bounds dx dy sheet.select
                    in
                    ( { model | sheet = { sheet | select = newSelect } }, Cmd.none )

                -- Get selected row indices for deletion (never the header/type rows y <= 0)
                selectedRows =
                    let
                        norm =
                            normalizeRect sheet.select
                    in
                    List.range norm.a.y norm.b.y |> List.filter (\y -> y >= 1)

                -- Get selected column indices for deletion
                selectedCols =
                    let
                        norm =
                            normalizeRect sheet.select
                    in
                    List.range norm.a.x norm.b.x

                -- Get all selected cell indices for clearing (data rows only)
                selectedCells =
                    rectToIndices sheet.select |> List.filter (\i -> i.y >= 1)
            in
            case event.key of
                "ArrowUp" ->
                    if event.shift then
                        expand 0 -1

                    else
                        move 0 -1

                "ArrowDown" ->
                    if event.shift then
                        expand 0 1

                    else
                        move 0 1

                "ArrowLeft" ->
                    if event.shift then
                        expand -1 0

                    else
                        move -1 0

                "ArrowRight" ->
                    if event.shift then
                        expand 1 0

                    else
                        move 1 0

                "Tab" ->
                    move (iif event.shift -1 1) 0

                "Enter" ->
                    -- Ctrl+Enter inserts blank rows above the selection, which is the
                    -- only way to reach row 1; the footer click appends at the end.
                    if event.ctrl || event.meta then
                        update (DocMsg (iif event.shift (SheetRowDuplicate selectedRows) (SheetRowInsert selectedRows))) model

                    else
                        startEdit

                "Delete" ->
                    -- Ctrl+Delete deletes the selected rows, plain Delete clears the cells
                    if event.ctrl || event.meta then
                        update (DocMsg (SheetRowDelete selectedRows)) model

                    else
                        update (DocMsg (SheetClearCells selectedCells)) model

                "Backspace" ->
                    -- Ctrl+Backspace deletes the selected columns. Ctrl+Shift+Delete would be
                    -- the symmetric key, but Chrome keeps it for "Clear browsing data".
                    if event.ctrl || event.meta then
                        update (DocMsg (SheetColumnDelete selectedCols)) model

                    else
                        update (DocMsg (SheetClearCells selectedCells)) model

                "Home" ->
                    -- Jump to beginning of row, or top-left with Ctrl
                    if event.ctrl || event.meta then
                        let
                            newSel =
                                xy 0 1
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                    else
                        let
                            newSel =
                                xy 0 sel.y
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                "End" ->
                    -- Jump to end of row, or bottom-right with Ctrl
                    if event.ctrl || event.meta then
                        let
                            newSel =
                                clampIndex bounds (xy bounds.maxX bounds.maxY)
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                    else
                        let
                            newSel =
                                xy bounds.maxX sel.y
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                _ ->
                    if event.key == "a" && (event.ctrl || event.meta) then
                        update SelectAll model

                    else if String.length event.key == 1 && not event.ctrl && not event.meta then
                        -- A printable character starts editing with it
                        case sheet.doc of
                            Ok (Tab tbl) ->
                                case ( Array.get sel.x tbl.cols, sel.y >= 1 && displayYToDocY model.search sheet tbl.rows sel.y == Nothing ) of
                                    ( Just _, True ) ->
                                        ( { model | error = undrawnRows [ sel.y ] }, Cmd.none )

                                    ( Just _, False ) ->
                                        ( { model | sheet = { sheet | write = Just event.key } }
                                        , Task.attempt (always NoOp) (Dom.focus "new-cell")
                                        )

                                    _ ->
                                        ( model, Cmd.none )

                            Ok _ ->
                                ( { model | error = computedCell }, Cmd.none )

                            Err _ ->
                                ( model, Cmd.none )

                    else
                        ( model, Cmd.none )


updatePaste : String -> Model -> ( Model, Cmd Msg )
updatePaste text ({ sheet } as model) =
    -- Parse pasted data and insert into table, expanding bounds as needed
    case sheet.doc of
        Ok (Tab tbl) ->
            let
                -- Clamp paste target to a data cell so an empty/header selection never overwrites column defs
                sel =
                    { x = max 0 sheet.select.a.x, y = max 1 sheet.select.a.y }

                -- Detect format and parse
                data =
                    case detectFormat text of
                        Tsv ->
                            parseTsv text

                        Csv ->
                            parseCsv text

                        JsonArray ->
                            parseJson text
                                |> Result.withDefault [ [ text ] ]

                        PlainText ->
                            [ [ text ] ]

                -- Calculate required dimensions
                pasteWidth =
                    data |> List.map List.length |> List.maximum |> Maybe.withDefault 0

                pasteHeight =
                    List.length data

                currentColCount =
                    Array.length tbl.cols

                currentRowCount =
                    Array.length tbl.rows

                -- Rows currently visible (after sort/filter/search); paste targets display positions
                visibleCount =
                    Array.length (filterAndSortIndexed model.search sheet tbl.rows)

                -- `sel.y` is at least 1, so a row that maps to nothing is past the drawn rows, and the paste appends it.
                docRowFor : Int -> Int
                docRowFor dispY =
                    case displayYToDocY model.search sheet tbl.rows dispY of
                        Just docY ->
                            docY

                        Nothing ->
                            currentRowCount + (dispY - visibleCount)

                -- How many new columns/rows needed?
                neededCols =
                    max 0 (sel.x + pasteWidth - currentColCount)

                neededRows =
                    max 0 (sel.y + pasteHeight - 1 - visibleCount)

                -- Generate patches to add new columns
                newColPatches =
                    List.range 0 (neededCols - 1)
                        |> List.map
                            (\i ->
                                let
                                    newKey =
                                        String.fromInt (currentColCount + i)
                                in
                                { action = "push"
                                , path = [ E.int 0 ]
                                , value =
                                    E.list identity
                                        [ E.object
                                            [ ( "name", E.string "" )
                                            , ( "type", E.string "text" )
                                            , ( "key", E.string newKey )
                                            ]
                                        ]
                                }
                            )

                -- Generate patches to add new rows
                newRowPatches =
                    List.range 0 (neededRows - 1)
                        |> List.map
                            (\_ ->
                                { action = "push"
                                , path = []
                                , value = E.list identity [ E.object [] ]
                                }
                            )

                -- Build column key lookup (existing + new)
                colKey : Int -> String
                colKey x =
                    if x < currentColCount then
                        Array.get x tbl.cols
                            |> Maybe.map .key
                            |> Maybe.withDefault (String.fromInt x)

                    else
                        String.fromInt x

                -- Generate patches for each cell value
                cellPatches =
                    data
                        |> List.indexedMap
                            (\rowOffset row ->
                                row
                                    |> List.indexedMap
                                        (\colOffset value ->
                                            let
                                                x =
                                                    sel.x + colOffset

                                                y =
                                                    docRowFor (sel.y + rowOffset)
                                            in
                                            { action = "set"
                                            , path = [ E.int y, E.string (colKey x) ]
                                            , value = E.string value
                                            }
                                        )
                            )
                        |> List.concat

                -- Combine all patches: columns first, then rows, then values
                allPatches =
                    newColPatches ++ newRowPatches ++ cellPatches
            in
            if List.isEmpty allPatches then
                ( model, Cmd.none )

            else
                ( model, changeDoc { id = sheet.id, data = allPatches } )

        Ok _ ->
            ( { model | error = computedCell }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


updateShareLoad : D.Value -> Model -> ( Model, Cmd Msg )
updateShareLoad value model =
    -- Every answer names the sheet it is about and the action that
    -- asked. Without the id, a list for sheet A that resolved after the
    -- user opened sheet B wrote A's members and public flag into B's
    -- panel, silently. Without the action, a field the server renamed
    -- away read exactly like a field this action never sends, so the
    -- one failure this payload most needed to report was the one it
    -- could not see.
    case value |> D.decodeValue (D.map2 Tuple.pair (D.field "id" D.string) (D.field "action" D.string)) of
        Err err ->
            ( { model
                | error =
                    "A share answer arrived without saying which sheet and which action it is about: "
                        ++ D.errorToString err
              }
            , Cmd.none
            )

        Ok ( id, action ) ->
            if id /= model.id then
                -- A correct answer to a question about another sheet.
                -- Dropped rather than reported: navigating while a
                -- request is open is ordinary, and the bug was ever
                -- letting it land.
                ( model, Cmd.none )

            else
                let
                    share =
                        model.share

                    -- Whether a field was sent at all, which separates a
                    -- field this action does not carry from one that
                    -- arrived and could not be read.
                    sent field =
                        value |> D.decodeValue (D.field field D.value) |> Result.toMaybe

                    members =
                        value
                            |> D.decodeValue
                                (D.field "members"
                                    (D.list (D.map2 Member (D.field "email" D.string) (D.field "role" D.string)))
                                )

                    public =
                        value |> D.decodeValue (D.field "public" D.bool)

                    link =
                        value |> D.decodeValue (D.field "link" D.string)

                    hook =
                        value
                            |> D.decodeValue
                                (D.field "hook"
                                    (D.map3 Hook
                                        (D.field "url" D.string)
                                        (D.field "secret" D.string)
                                        (D.field "repro" D.string)
                                    )
                                )

                    -- What this action promises to carry. A promised
                    -- field that is absent is an error, which is what
                    -- makes a rename an error rather than a silence.
                    promised =
                        case action of
                            "hook" ->
                                [ "hook" ]

                            "link" ->
                                [ "members", "public", "link" ]

                            _ ->
                                [ "members", "public" ]

                    checked what field decoded =
                        if not (List.member field promised) then
                            Nothing

                        else
                            case ( sent field, decoded ) of
                                ( Nothing, _ ) ->
                                    Just
                                        ("A "
                                            ++ action
                                            ++ " answer carries no "
                                            ++ what
                                            ++ ": the \""
                                            ++ field
                                            ++ "\" field is missing, not empty."
                                        )

                                ( Just _, Err err ) ->
                                    Just ("The " ++ what ++ " arrived in a shape I could not read: " ++ D.errorToString err)

                                _ ->
                                    Nothing

                    unreadable =
                        List.filterMap identity
                            [ checked "member list" "members" (Result.map (always ()) members)
                            , checked "public flag" "public" (Result.map (always ()) public)
                            , checked "share link" "link" (Result.map (always ()) link)
                            , checked "signing secret" "hook" (Result.map (always ()) hook)
                            ]
                in
                ( { model
                    | share =
                        { share
                            | members = members |> Result.withDefault share.members
                            , public = public |> Result.withDefault share.public
                            , link = link |> Result.toMaybe |> orElse share.link
                            , hook = hook |> Result.toMaybe |> orElse share.hook
                        }

                    -- Every unreadable field, not the first: a banner
                    -- about the member list while the public flag
                    -- quietly shows the previous answer's value is worse
                    -- than one that names both.
                    , error =
                        if List.isEmpty unreadable then
                            model.error

                        else
                            String.join " " unreadable
                  }
                , Cmd.none
                )


libraryCols : Model -> Array Col
libraryCols model =
    Array.fromList <|
        List.concat
            [ [ madeCol "sheet_id" "" SheetId
              , madeCol "thumb" "" Thumb
              , madeCol "name" "name" Text
              , madeCol "tags" "tags" (Many Text)

              -- When this browser last opened the sheet. Sortable by the header
              -- click; "" for never-opened sorts to the far end either way.
              , madeCol "opened" "opened" Timestamp
              ]
            , -- `library:freshness` answers for the sheets whose runs are
              -- written down, and for a caller it can identify, so an anonymous
              -- visitor gets no rows at all. No rows means no column: a blank
              -- one over every sheet reads as "nothing is wrong", which is the
              -- claim this column exists to stop the page making.
              iif (Dict.isEmpty model.freshness) [] [ madeCol "freshness" "freshness" Text ]
            , -- Trash is offered on every row, a bundled one included: the flag is
              -- this browser's, so a demo it does not want is a demo it can put
              -- away. Purging is not, because a system entry has nothing of its
              -- own to purge -- dropping it would only un-trash it.
              iif model.trash [ madeCol "restore" "" Restore, madeCol "delete" "" Delete ] [ madeCol "star" "" Star, madeCol "trash" "" Trash ]
            ]


{-| Every modal in the app, so the label is the one thing each caller has to
supply: a screen reader announces the panel by it, and there is no heading
inside `content` this function could read.
-}
viewModal : String -> Msg -> List (Html Msg) -> Html Msg
viewModal label closeMsg content =
    H.div
        [ A.class "scrim", A.onClick closeMsg ]
        [ H.div
            [ A.class "panel"
            , A.attribute "role" "dialog"
            , A.attribute "aria-modal" "true"
            , A.attribute "aria-label" label
            , S.paddingRem 1
            , S.minWidthRem 20
            , A.stopPropagationOn "click" (D.succeed ( NoOp, True ))
            ]
            content
        ]


viewSettings : Bool -> SheetInfo -> Share -> Html Msg
viewSettings show info share =
    if not show then
        text ""

    else
        viewModal "sheet settings"
            SettingsClose
            [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter, S.marginBottom "1rem" ]
                [ H.h3 [] [ text "Sheet Settings" ]
                , H.button [ A.class "x", A.attribute "aria-label" "close the sheet settings", A.onClick SettingsClose ] [ text "×" ]
                ]
            , H.div [ S.marginBottom "1rem" ]
                [ H.label [ S.display "block", S.marginBottom "0.25rem", S.fontWeight "600" ] [ text "Name" ]
                , H.input
                    [ A.type_ "text"
                    , A.value info.name
                    , A.onInput SettingsNameChange
                    , A.placeholder "Sheet name"
                    , A.attribute "onfocus" "this.select()"
                    , S.width "100%"
                    ]
                    []
                ]
            , H.div [ S.marginBottom "1rem" ]
                [ H.label [ S.display "block", S.marginBottom "0.25rem", S.fontWeight "600" ] [ text "Tags" ]
                , H.input
                    [ A.type_ "text"
                    , A.value (String.join ", " info.tags)
                    , A.onInput SettingsTagsChange
                    , A.placeholder "tag1, tag2, tag3"
                    , S.width "100%"
                    ]
                    []
                , H.small [ S.color "#666" ] [ text "Separate tags with commas" ]
                ]
            , H.div [ S.marginBottom "1rem" ]
                [ H.label [ S.display "block", S.marginBottom "0.25rem", S.fontWeight "600" ] [ text "Sharing" ]
                , H.div [ S.displayFlex, S.gapRem 0.25, S.marginBottom "0.5rem" ]
                    [ H.input
                        [ A.type_ "email"
                        , A.value share.email
                        , A.onInput ShareEmailChange
                        , A.placeholder "name@example.com"
                        , S.flexGrow "1"
                        ]
                        []
                    , H.select [ A.onInput ShareRoleChange, A.value share.role ]
                        (List.map (\r -> H.option [ A.value r, A.selected (r == share.role) ] [ text r ]) [ "viewer", "editor", "owner" ])
                    , H.button [ A.onClick ShareAdd ] [ text "Share" ]
                    ]
                , H.div [ S.fontSizeRem 0.875 ]
                    (if List.isEmpty share.members then
                        [ H.small [ S.color "#666" ] [ text "Only you." ] ]

                     else
                        List.map
                            (\m ->
                                H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter, S.gapRem 0.5 ]
                                    [ H.span [ S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap ] [ text m.email ]
                                    , H.span [ S.color "#666" ] [ text m.role ]
                                    , if m.role == "owner" then
                                        text ""

                                      else
                                        H.button [ A.class "x", A.onClick (ShareRemove m.email), A.title ("remove " ++ m.email) ] [ text "×" ]
                                    ]
                            )
                            share.members
                    )
                ]
            , H.div [ S.marginBottom "1rem" ]
                [ H.label [ S.display "block", S.marginBottom "0.25rem", S.fontWeight "600" ] [ text "Anyone with the link" ]
                , H.div [ S.displayFlex, S.alignItemsCenter, S.gapRem 0.5 ]
                    [ H.label [ S.displayFlex, S.alignItemsCenter, S.gapRem 0.25 ]
                        [ H.input [ A.type_ "checkbox", A.checked share.public, A.onCheck SharePublic ] []
                        , text (iif share.public "public: anyone can read" "private")
                        ]
                    , H.button [ A.onClick ShareLink ] [ text "mint view-only link" ]
                    ]

                -- The server refuses a publish over an email address, a phone
                -- number, an SSN or a card number, and this is the claim that
                -- answers it. Under the public box because it is about what
                -- publishing does, and never read back off the server: a
                -- navigation empties the panel, so nothing claims this on a
                -- publisher's behalf on a sheet they have not looked at.
                , H.label [ S.displayFlex, S.alignItemsCenter, S.gapRem 0.25, S.marginTop "0.25rem" ]
                    [ H.input [ A.type_ "checkbox", A.checked share.personal, A.onCheck SharePersonal ] []
                    , text "holds personal data, publish anyway"
                    ]

                -- Both blank mint the link this button always minted: thirty
                -- days, openable by anyone holding the url. The server reads an
                -- empty body as exactly that, so an untouched panel sends one.
                , H.div [ S.displayFlex, S.gapRem 0.25, S.marginTop "0.25rem" ]
                    [ H.input
                        [ A.type_ "number"
                        , A.value share.days
                        , A.onInput ShareDaysChange
                        , A.placeholder "30 days"
                        , A.title "how many days the link lives"
                        , S.width "8rem"
                        ]
                        []
                    , H.input
                        [ A.type_ "password"
                        , A.value share.password
                        , A.onInput SharePasswordChange
                        , A.placeholder "password (optional)"
                        , A.title "a password the reader has to type to open the link"
                        , S.flexGrow "1"
                        ]
                        []
                    ]
                , case share.link of
                    Just link ->
                        H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.marginTop "0.25rem" ]
                            [ H.div [ S.displayFlex, S.gapRem 0.25 ]
                                [ H.input [ A.type_ "text", A.value link, A.readonly True, A.attribute "onfocus" "this.select()", S.flexGrow "1", S.fontSizeRem 0.75 ] []
                                , H.button [ A.onClick (CopyText link) ] [ text "copy" ]
                                ]

                            -- The password is not in the link, on purpose: a
                            -- lock the url carries is not a lock. Whoever mints
                            -- it has to send it by some other channel, and
                            -- nothing here can do that for them.
                            , iif (String.isEmpty share.password)
                                (text "")
                                (H.small [ S.color "#666" ]
                                    [ text "This link is locked. The password is not in it — send it separately, or nobody can open the sheet." ]
                                )
                            ]

                    Nothing ->
                        text ""
                ]
            , H.button
                [ A.onClick SettingsClose
                , S.width "100%"
                , S.padding "0.5rem 1rem"
                , S.border "none"
                , S.background "#0a58ca"
                , S.color "#fff"
                ]
                [ text "Done" ]
            ]


{-| The columns the server read, the type each was guessed to carry, and the
first rows under them, before the sheet exists. A select per column is where
the guess is corrected; a remembered type says so beside it.
-}
viewImport : Maybe Importing -> Html Msg
viewImport importing =
    case importing of
        Nothing ->
            text ""

        Just imp ->
            viewModal "import a file"
                ImportCancel
                [ H.p [ S.marginBottom "1rem" ]
                    [ text (imp.filename ++ ": " ++ String.fromInt imp.count ++ " rows. Correct a type before the sheet is made.") ]
                , H.table [ A.class "import-preview", S.marginBottom "1rem" ]
                    [ H.thead []
                        [ H.tr [] (List.map (\col -> H.th [] [ text col.name ]) imp.cols)
                        , H.tr []
                            (List.map
                                (\col ->
                                    H.th []
                                        [ H.select [ A.onInput (ImportTypeChange col.name), A.value col.typ, A.title ("the type of " ++ col.name) ]
                                            (List.map
                                                (\( spelling, _ ) -> H.option [ A.value spelling, A.selected (spelling == col.typ) ] [ text spelling ])
                                                importTypes
                                            )
                                        , iif col.remembered (H.span [ S.fontSizeRem 0.75, S.color "#666" ] [ text " remembered" ]) (text "")
                                        ]
                                )
                                imp.cols
                            )
                        ]
                    , H.tbody [] (List.map (\row -> H.tr [] (List.map (\cell -> H.td [] [ text cell ]) row)) imp.rows)
                    ]
                , H.div [ S.displayFlex, S.gapRem 0.5, S.justifyContentFlexEnd ]
                    [ H.button [ A.onClick ImportCancel, S.padding "0.5rem 1rem" ] [ text "Cancel" ]
                    , H.button [ A.onClick ImportConfirm, S.padding "0.5rem 1rem" ] [ text "Import" ]
                    ]
                ]


{-| The types a file's column may be imported as: every type a column may
declare, less the three that are the app's own furniture and not data.
-}
importTypes : List ( String, Type )
importTypes =
    List.filter (\( spelling, _ ) -> not (List.member spelling [ "sheet_id", "form", "create" ])) columnTypes


viewDeleteConfirm : Maybe String -> Html Msg
viewDeleteConfirm maybeId =
    case maybeId of
        Nothing ->
            text ""

        Just id ->
            viewModal "delete this sheet"
                DocDeleteCancel
                [ H.p [ S.marginBottom "1rem" ]
                    [ text "Delete this sheet from this browser's library for good? Trashing it instead is undoable; this is not." ]
                , H.div [ S.displayFlex, S.gapRem 0.5, S.justifyContentFlexEnd ]
                    [ H.button [ A.onClick DocDeleteCancel, S.padding "0.5rem 1rem" ] [ text "Cancel" ]
                    , H.button
                        [ A.onClick (DocDeleteConfirm id)
                        , S.padding "0.5rem 1rem"
                        , S.border "none"
                        , S.background "#dc3545"
                        , S.color "#fff"
                        ]
                        [ text "Delete" ]
                    ]
                ]


viewPending : Maybe Pending -> Html Msg
viewPending pending =
    case pending of
        Just { edit, write, columns, stage } ->
            case stage of
                Warning found ->
                    let
                        ( verb, change ) =
                            case ( edit, write ) of
                                ( SheetColumnDelete _, _ ) ->
                                    ( "Delete", "Deleting " ++ String.join ", " (List.map quoted columns) )

                                ( _, Just name ) ->
                                    ( "Rename", "Renaming " ++ String.join ", " (List.map quoted columns) ++ " to " ++ quoted name )

                                ( _, Nothing ) ->
                                    ( "Rename", "Renaming " ++ String.join ", " (List.map quoted columns) )
                    in
                    viewModal "sheets this change can break"
                        PendingCancel
                        [ case found of
                            Ok rows ->
                                H.div []
                                    [ H.p [] [ text (change ++ " can break these sheets:") ]
                                    , H.ul [ S.marginBottom "1rem" ]
                                        (List.map (\( name, why ) -> H.li [] [ H.strong [] [ text name ], text (" " ++ why) ]) rows)
                                    ]

                            Err reason ->
                                H.p [ S.marginBottom "1rem" ]
                                    [ text (change ++ " can break a sheet that reads it, and which sheets do could not be read: " ++ reason) ]
                        , H.div [ S.displayFlex, S.gapRem 0.5, S.justifyContentFlexEnd ]
                            [ H.button [ A.onClick PendingCancel, S.padding "0.5rem 1rem" ] [ text "Cancel" ]
                            , H.button
                                [ A.onClick PendingConfirm
                                , S.padding "0.5rem 1rem"
                                , S.border "none"
                                , S.background "#dc3545"
                                , S.color "#fff"
                                ]
                                [ text verb ]
                            ]
                        ]

                Asking ->
                    text ""

                Confirmed ->
                    text ""

        Nothing ->
            text ""


{-| The list of what exists, and the palette's source. A third field carries the
message the palette runs, so the two cannot drift into disagreeing about what the
app can do -- `Nothing` is a key that only means something against a selection,
which no palette row can supply. The palette itself is listed and not runnable:
it is already open.
-}
shortcutGroups : List ( String, List ( String, String, Maybe Msg ) )
shortcutGroups =
    [ ( "Navigate"
      , [ ( "↑ ↓ ← →", "move", Nothing )
        , ( "Shift+arrows", "extend selection", Nothing )
        , ( "Tab / Shift+Tab", "next/prev cell", Nothing )
        , ( "Home / End", "row start/end", Nothing )
        , ( "Ctrl/⌘+Home / Ctrl/⌘+End", "sheet corners", Nothing )
        ]
      )
    , ( "Edit"
      , [ ( "Enter", "edit cell", Nothing )
        , ( "any character", "edit with that character", Nothing )
        , ( "Enter", "commit + down", Nothing )
        , ( "Tab", "commit + right", Nothing )
        , ( "Esc", "cancel edit", Nothing )
        ]
      )
    , ( "Cells"
      , [ ( "Delete / Backspace", "clear cells", Nothing )
        , ( "Ctrl/⌘+Delete", "delete rows", Nothing )
        , ( "Ctrl/⌘+Backspace", "delete columns", Nothing )
        , ( "Ctrl/⌘+Enter", "insert rows above", Nothing )
        , ( "Ctrl/⌘+Shift+Enter", "duplicate rows", Nothing )
        , ( "Ctrl/⌘+D", "fill down", Nothing )
        , ( "Ctrl/⌘+Shift+D", "delete duplicate rows", Just (DocMsg SheetRowsDedupe) )
        ]
      )
    , ( "Select & clipboard"
      , [ ( "Ctrl/⌘+A", "select all", Just SelectAll )
        , ( "Ctrl/⌘+C", "copy", Just ClipboardCopy )
        , ( "Ctrl/⌘+V", "paste", Nothing )
        , ( "click / shift-click header", "sort / add a sort key", Nothing )
        ]
      )
    , ( "Find"
      , [ ( "Ctrl/⌘+F", "find", Just (FindOpen False) )
        , ( "Ctrl/⌘+H", "replace", Just (FindOpen True) )
        , ( "Enter", "next match", Nothing )
        , ( "Esc", "close", Nothing )
        ]
      )
    , ( "History"
      , [ ( "Ctrl/⌘+Z", "undo", Just Undo )
        , ( "Ctrl/⌘+Shift+Z / Ctrl/⌘+Y", "redo", Just Redo )
        ]
      )
    , ( "Library"
      , [ ( "Enter", "open selected sheet", Nothing )
        , ( "Ctrl/⌘+Shift+Backspace", "trash selected sheets", Just TrashSelected )
        ]
      )
    , ( "Help"
      , [ ( "Ctrl/⌘+K", "command palette", Nothing )
        , ( "Ctrl/⌘+/ or ?", "shortcut sheet", Just (ShortcutsToggle True) )
        , ( "Esc", "close dialogs", Nothing )
        ]
      )
    ]


type alias Command =
    { label : String, hint : String, run : Msg }


{-| What the palette offers for what has been typed: the runnable shortcuts
first, then the sheets, each matched on both the words on screen and the id or
key behind them. The shortcut sheet is the list of what the app can do, so this
reads it rather than keeping a second copy beside it. Bounded, because a palette
is a shortlist -- past a dozen rows you are reading rather than choosing, and
typing one more letter is the way through.
-}
paletteCommands : Library -> String -> List Command
paletteCommands shelf query =
    let
        needle =
            String.toLower (String.trim query)

        matches haystacks =
            List.any (String.contains needle << String.toLower) haystacks

        commands =
            shortcutGroups
                |> List.concatMap Tuple.second
                |> List.filterMap
                    (\( key, description, run ) ->
                        run
                            |> Maybe.andThen
                                (\msg -> iif (matches [ description, key ]) (Just (Command description key msg)) Nothing)
                    )

        sheets =
            shelf
                |> Dict.filter (\k v -> k /= "" && not v.scratch && not v.trashed && matches [ k, v.name ])
                |> Dict.toList
                -- Starred first, the way the library table draws them, and the
                -- rest in the order the dictionary gave: `List.sortBy` is
                -- stable, so this moves the starred ones and nothing else.
                |> List.sortBy (\( _, v ) -> iif v.starred 0 1)
                |> List.map
                    (\( k, v ) -> Command (iif (String.isEmpty (String.trim v.name)) k v.name) k (Goto k))
    in
    List.take 12 (commands ++ sheets)


{-| The palette as it is drawn: what the app can do anywhere, plus the one verb
that only means something over the sheet that is open. `paletteCommands` reads
`shortcutGroups` and knows nothing but the library, which is what keeps the
shortcut sheet and the palette from drifting; a verb with no key belongs to
neither list and is added here instead.

Subscribing is offered over a sheet whose rows are data -- a table, a query, a
feed, a hook -- and not over a run log, a listing or a chart. It is the footer's
own new-alert door with the query and the destination filled in, so it is
offered to a signed-in reader alone: the address the alert is sent to is the
account's own.

Building a cohort table is the footer's own new-query door with the columns
guessed: the first date column, the first column named like an id, the first
money column, by month. They are a first draft the reader then edits, which is
what makes this a helper that writes the SQL rather than a wizard; the statement
itself is `cohortSql` in src/sql.mjs, which `index.html` calls because Elm
cannot import that module. A sheet with no date or no key column is not offered
it at all: a command that can only fail is not a command.

Scoring customers is the same door over the same guesses, into `rfmSql`: recency,
frequency and money scored per key. A score needs money to score, so a sheet
with no money column is not offered it either.

-}
paletteRows : Model -> String -> List Command
paletteRows model query =
    let
        label : String
        label =
            "subscribe to this sheet"

        watchable : Bool
        watchable =
            case model.sheet.doc of
                Ok (Tab _) ->
                    True

                Ok (Query _) ->
                    True

                Ok (NetHttp _) ->
                    True

                Ok NetHook ->
                    True

                _ ->
                    False

        -- The state and not the address: what a visitor typed into the login
        -- form is a string in the same field, and an alert addressed to it is
        -- one the server would refuse to make.
        addressed : Bool
        addressed =
            case model.auth.state of
                LoggedIn _ ->
                    model.auth.email /= ""

                _ ->
                    False

        subscribe : List Command
        subscribe =
            if watchable && addressed && String.contains (String.toLower (String.trim query)) label then
                [ Command label model.sheet.id <|
                    DocNew <|
                        E.object
                            [ ( "type", E.string "alert" )
                            , ( "data"
                              , E.list identity
                                    [ E.object
                                        [ ( "code", E.string ("select * from @" ++ model.sheet.id) )
                                        , ( "to", E.string model.auth.email )
                                        , ( "interval", E.int 3600 )
                                        , ( "digest", E.bool False )
                                        , ( "when", E.string "added" )
                                        ]
                                    ]
                              )
                            ]
                ]

            else
                []

        cohortLabel : String
        cohortLabel =
            "build a cohort table from this sheet"

        -- `arrangeable` already answers for the two sheets a cohort can be built
        -- from and for no others, and it is the one place that knows where each
        -- keeps its columns: a table's are its document's, a query's are
        -- whatever its last run returned.
        columns : List Col
        columns =
            arrangeable model.sheet |> Maybe.map (.cols >> Array.toList) |> Maybe.withDefault []

        firstColumn : (Col -> Bool) -> Maybe String
        firstColumn ok =
            columns |> List.filter ok |> List.head |> Maybe.map .name

        keyed : Maybe String
        keyed =
            case firstColumn (\c -> String.endsWith "_id" c.name) of
                Nothing ->
                    firstColumn (\c -> c.typ == Text)

                named ->
                    named

        dated : Maybe String
        dated =
            firstColumn (\c -> c.typ == Date || c.typ == Timestamp)

        money : Maybe String
        money =
            firstColumn (\c -> c.typ == Usd)

        cohort : List Command
        cohort =
            case ( dated, keyed ) of
                ( Just when, Just who ) ->
                    if String.contains (String.toLower (String.trim query)) cohortLabel then
                        [ Command cohortLabel model.sheet.id <|
                            DocNew <|
                                E.object
                                    [ ( "type", E.string "query" )
                                    , ( "data"
                                      , E.list identity
                                            [ E.object
                                                [ ( "lang", E.string "sql" )
                                                , ( "cohort"
                                                  , E.object
                                                        [ ( "source", E.string ("@" ++ model.sheet.id) )
                                                        , ( "date", E.string when )
                                                        , ( "key", E.string who )
                                                        , ( "value", E.string (Maybe.withDefault "" money) )
                                                        , ( "grain", E.string "month" )
                                                        ]
                                                  )
                                                ]
                                            ]
                                      )
                                    ]
                        ]

                    else
                        []

                _ ->
                    []

        rfmLabel : String
        rfmLabel =
            "score this sheet's customers (RFM)"

        rfm : List Command
        rfm =
            case ( dated, keyed, money ) of
                ( Just when, Just who, Just spent ) ->
                    if String.contains (String.toLower (String.trim query)) (String.toLower rfmLabel) then
                        [ Command rfmLabel model.sheet.id <|
                            DocNew <|
                                E.object
                                    [ ( "type", E.string "query" )
                                    , ( "data"
                                      , E.list identity
                                            [ E.object
                                                [ ( "lang", E.string "sql" )
                                                , ( "rfm"
                                                  , E.object
                                                        [ ( "source", E.string ("@" ++ model.sheet.id) )
                                                        , ( "date", E.string when )
                                                        , ( "key", E.string who )
                                                        , ( "value", E.string spent )
                                                        , ( "buckets", E.int 5 )
                                                        ]
                                                  )
                                                ]
                                            ]
                                      )
                                    ]
                        ]

                    else
                        []

                _ ->
                    []
    in
    List.take 12 (subscribe ++ cohort ++ rfm ++ paletteCommands model.library query)


viewShortcuts : Bool -> Html Msg
viewShortcuts show =
    if not show then
        text ""

    else
        viewModal "keyboard shortcuts" (ShortcutsToggle False) <|
            H.h3 [] [ text "Keyboard shortcuts" ]
                :: List.concatMap
                    (\( group, keys ) ->
                        [ H.h4 [ S.marginTop "0.75rem", S.marginBottom "0.25rem" ] [ text group ]
                        , H.div [ S.displayGrid, S.gridTemplateColumns "auto 1fr", S.gap "0.25rem 1rem", S.fontSizeRem 0.875 ] <|
                            List.concatMap
                                (\( key, description, _ ) ->
                                    [ H.span [ A.class "mono" ] [ text key ]
                                    , H.span [] [ text description ]
                                    ]
                                )
                                keys
                        ]
                    )
                    shortcutGroups


viewPalette : Model -> Html Msg
viewPalette model =
    case model.palette of
        Nothing ->
            text ""

        Just p ->
            let
                commands =
                    paletteRows model p.query
            in
            viewModal "command palette"
                (PaletteToggle False)
                [ H.input
                    [ A.id "palette"
                    , A.placeholder "jump to a sheet, or run a command"
                    , A.attribute "aria-label" "jump to a sheet, or run a command"
                    , A.attribute "role" "combobox"
                    , A.attribute "aria-expanded" "true"
                    , A.attribute "aria-controls" "palette-list"
                    , iif (p.selected >= 0 && p.selected < List.length commands)
                        (A.attribute "aria-activedescendant" ("palette-" ++ String.fromInt p.selected))
                        (A.classList [])
                    , A.value p.query
                    , A.onInput (InputChange PaletteQuery)
                    , onPaletteKeydown p.selected
                    , S.width "100%"
                    ]
                    []
                , H.div [ A.id "palette-list", A.attribute "role" "listbox", S.displayFlex, S.flexDirectionColumn, S.marginTopRem 0.5, S.maxHeight "60vh", S.overflowYAuto, S.fontSizeRem 0.875 ] <|
                    case commands of
                        [] ->
                            [ H.span [ S.color "#666" ] [ text "nothing matches" ] ]

                        _ ->
                            List.indexedMap
                                (\i command ->
                                    H.button
                                        [ A.onClick (PaletteRun i)
                                        , A.id ("palette-" ++ String.fromInt i)
                                        , A.attribute "role" "option"
                                        , A.attribute "aria-selected" (iif (i == p.selected) "true" "false")
                                        , S.displayFlex
                                        , S.justifyContentSpaceBetween
                                        , S.gapRem 1
                                        , S.textAlignLeft
                                        , S.border "none"
                                        , S.padding "0.25rem 0.375rem"
                                        , S.background (iif (i == p.selected) "#e8e8f8" "transparent")
                                        ]
                                        [ text command.label
                                        , H.span [ A.class "mono", S.color "#666" ] [ text command.hint ]
                                        ]
                                )
                                commands
                ]


{-| The past rows are text in a plain `table`, built from nothing a live sheet
view uses: no cell takes a click, an input or a drag. The glue's refusals land
in `model.error`, drawn here too because the scrim covers the banner.
-}
viewHistory : String -> Maybe History -> Html Msg
viewHistory refusal history =
    case history of
        Nothing ->
            text ""

        Just h ->
            let
                failed error =
                    H.pre [ S.color "#b00", S.whiteSpacePreWrap, S.fontSizeRem 0.75 ] [ text error ]
            in
            viewModal "sheet history"
                (HistoryMsg HistoryClose)
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter, S.marginBottom "0.5rem" ]
                    [ H.h3 [] [ text "History" ]
                    , H.button [ A.class "x", A.attribute "aria-label" "close the history", A.onClick (HistoryMsg HistoryClose) ] [ text "×" ]
                    ]
                , viewError refusal
                , case h.versions of
                    Nothing ->
                        H.span [ S.color "#666" ] [ text "reading the history…" ]

                    Just (Err error) ->
                        failed error

                    Just (Ok versions) ->
                        H.div [ A.id "versions", S.displayFlex, S.flexDirectionColumn, S.maxHeight "30vh", S.overflowYAuto, S.fontSizeRem 0.875 ] <|
                            List.map
                                (\v ->
                                    H.button
                                        [ A.onClick (HistoryMsg (HistoryPick v.hash))
                                        , A.attribute "aria-pressed" (iif (h.hash == Just v.hash) "true" "false")
                                        , S.displayFlex
                                        , S.gapRem 1
                                        , S.textAlignLeft
                                        , S.border "none"
                                        , S.padding "0.25rem 0.375rem"
                                        , S.background (iif (h.hash == Just v.hash) "#e8e8f8" "transparent")
                                        ]
                                        [ text (iif (v.time == 0) ("change " ++ String.fromInt v.seq) (isoStamp (v.time * 1000)))
                                        , H.span [ A.class "mono", S.color "#666" ] [ text (String.left 8 v.actor) ]
                                        , text (Maybe.withDefault "" v.message)
                                        ]
                                )
                                versions
                                ++ iif (h.left > 0)
                                    [ H.span [ S.color "#666" ] [ text (String.fromInt h.left ++ iif (h.left == 1) " older version is" " older versions are" ++ " not listed.") ] ]
                                    []
                , case ( h.hash, h.past ) of
                    ( Nothing, _ ) ->
                        text ""

                    ( Just _, Nothing ) ->
                        H.span [ S.color "#666" ] [ text "reading that version…" ]

                    ( Just _, Just (Err error) ) ->
                        failed error

                    ( Just _, Just (Ok past) ) ->
                        H.div [ S.maxHeight "40vh", S.overflowAuto, S.marginTopRem 0.5 ]
                            [ H.table [ A.id "past", S.fontSizeRem 0.875 ]
                                [ H.thead [] [ H.tr [] (List.map (\name -> H.th [] [ text name ]) past.columns) ]
                                , H.tbody [] (List.map (\row -> H.tr [] (List.map (\cell -> H.td [] [ text cell ]) row)) past.rows)
                                ]
                            ]
                ]


viewFindReplace : Maybe FindReplace -> Html Msg
viewFindReplace maybeFindReplace =
    case maybeFindReplace of
        Nothing ->
            text ""

        Just fr ->
            H.div
                [ A.class "panel"
                , S.positionFixed
                , S.topRem 3
                , S.rightRem 1
                , S.padding "0.5rem"
                , S.displayFlex
                , S.flexDirectionColumn
                , S.gapRem 0.5
                , S.zIndex "100"
                , S.fontSizeRem 0.875
                ]
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter ]
                    [ H.span [ S.fontWeight "600" ]
                        [ text (iif fr.showReplace "Find & Replace" "Find") ]
                    , H.button [ A.class "x", A.attribute "aria-label" "close find and replace", A.onClick FindClose ] [ text "×" ]
                    ]
                , H.div [ S.displayFlex, S.gapRem 0.25 ]
                    [ H.input
                        [ A.placeholder "Find..."
                        , A.value fr.findText
                        , A.onInput FindTextChange
                        , A.id "find-input"
                        , onFindKeydown
                        , S.widthRem 12
                        ]
                        []
                    , H.button [ A.onClick FindPrev, A.title "previous match" ] [ text "◀" ]
                    , H.button [ A.onClick FindNext, A.title "next match" ] [ text "▶" ]
                    ]
                , H.div [ S.fontSizeRem 0.75, S.color "#666" ]
                    [ text
                        (if List.isEmpty fr.matches then
                            iif (String.isEmpty fr.findText) "" "No matches"

                         else
                            String.fromInt (fr.currentMatch + 1)
                                ++ " of "
                                ++ String.fromInt (List.length fr.matches)
                        )
                    ]
                , if fr.showReplace then
                    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25 ]
                        [ H.input
                            [ A.placeholder "Replace with..."
                            , A.value fr.replaceText
                            , A.onInput ReplaceTextChange
                            , S.widthRem 12
                            ]
                            []
                        , H.div [ S.displayFlex, S.gapRem 0.25 ]
                            [ H.button [ A.onClick ReplaceOne ] [ text "Replace" ]
                            , H.button [ A.onClick ReplaceAll ] [ text "Replace All" ]
                            ]
                        ]

                  else
                    text ""
                ]


computeNumericStats : Array Row -> String -> Stat
computeNumericStats rows key =
    rows
        |> Array.foldl
            (\row stat ->
                row
                    |> Dict.get key
                    |> Maybe.andThen (D.decodeValue number >> Result.toMaybe)
                    |> Maybe.map
                        (\n ->
                            { histogram = stat.histogram |> Dict.update (String.fromFloat n) (Maybe.withDefault 0 >> (+) 1 >> Just)
                            , count = stat.count + 1
                            , sum = stat.sum + n
                            , min = stat.min |> Maybe.withDefault n |> min n |> Just
                            , max = stat.max |> Maybe.withDefault n |> max n |> Just
                            }
                        )
                    |> Maybe.withDefault stat
            )
            { histogram = Dict.empty, count = 0, sum = 0, min = Nothing, max = Nothing }
        |> Numeric


computeTextStats : Array Row -> String -> Stat
computeTextStats rows key =
    rows
        |> Array.foldl
            (\row stat ->
                row
                    |> Dict.get key
                    |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                    |> Maybe.map
                        (\s ->
                            { lengths = stat.lengths |> Dict.update (String.length s) (Maybe.withDefault 0 >> (+) 1 >> Just)
                            , keywords = s |> String.split " " |> List.foldl (\k -> Dict.update k (Maybe.withDefault 0 >> (+) 1 >> Just)) stat.keywords
                            , count = stat.count + 1
                            , sum = stat.sum + String.length s
                            , min = min (String.length s) (Maybe.withDefault (String.length s) stat.min) |> Just
                            , max = max (String.length s) stat.max
                            }
                        )
                    |> Maybe.withDefault stat
            )
            { lengths = Dict.empty, keywords = Dict.empty, count = 0, sum = 0, min = Nothing, max = 0 }
        |> Descriptive


{-| Days since 1970-01-01. Howard Hinnant's days\_from\_civil, so date columns can
report a span and count their gaps without a date library.
-}
civilDays : Int -> Int -> Int -> Int
civilDays year month day =
    let
        y =
            iif (month <= 2) (year - 1) year

        era =
            iif (y >= 0) y (y - 399) // 400

        yoe =
            y - era * 400

        doy =
            (153 * iif (month > 2) (month - 3) (month + 9) + 2) // 5 + day - 1

        doe =
            yoe * 365 + yoe // 4 - yoe // 100 + doy
    in
    era * 146097 + doe - 719468


{-| The YYYY-MM-DD prefix of an ISO date or timestamp. Anything else is not a date.
-}
parseDay : String -> Maybe Int
parseDay s =
    case String.split "-" (String.left 10 (String.trim s)) of
        [ y, m, d ] ->
            Maybe.map3 civilDays (String.toInt y) (String.toInt m) (String.toInt d)

        _ ->
            Nothing


{-| Epoch milliseconds as the UTC ISO timestamp the alert document holds. The
calendar is `elm/time`'s, so no month arithmetic happens here; only the digits
are written out.
-}
isoStamp : Int -> String
isoStamp ms =
    let
        at : Time.Posix
        at =
            Time.millisToPosix ms

        pad : Int -> String
        pad n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    String.padLeft 4 '0' (String.fromInt (Time.toYear Time.utc at))
        ++ "-"
        ++ pad (Date.monthToNumber (Time.toMonth Time.utc at))
        ++ "-"
        ++ pad (Time.toDay Time.utc at)
        ++ "T"
        ++ pad (Time.toHour Time.utc at)
        ++ ":"
        ++ pad (Time.toMinute Time.utc at)
        ++ ":"
        ++ pad (Time.toSecond Time.utc at)
        ++ "Z"


computeTemporalStats : Array Row -> String -> Stat
computeTemporalStats rows key =
    rows
        |> Array.foldl
            (\row stat ->
                row
                    |> Dict.get key
                    |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                    |> Maybe.andThen (\s -> Maybe.map (Tuple.pair s) (parseDay s))
                    |> Maybe.map
                        (\( s, day ) ->
                            { days = Set.insert day stat.days
                            , count = stat.count + 1
                            , first = Just (Maybe.withDefault s (Maybe.map (min s) stat.first))
                            , last = Just (Maybe.withDefault s (Maybe.map (max s) stat.last))
                            }
                        )
                    |> Maybe.withDefault stat
            )
            { days = Set.empty, count = 0, first = Nothing, last = Nothing }
        |> Temporal


{-| The shared `boolean` decoder falls back to False for null and for anything it
cannot read, so it can never report a blank. Counting blanks needs a decoder that
says Nothing instead of guessing.
-}
maybeBoolean : D.Decoder (Maybe Bool)
maybeBoolean =
    D.oneOf
        [ D.map Just D.bool
        , D.map (\n -> Just (n /= 0)) D.int
        , D.map
            (\s ->
                case String.toLower (String.trim s) of
                    "true" ->
                        Just True

                    "t" ->
                        Just True

                    "1" ->
                        Just True

                    "false" ->
                        Just False

                    "f" ->
                        Just False

                    "0" ->
                        Just False

                    _ ->
                        Nothing
            )
            D.string
        , D.succeed Nothing
        ]


computeBoolishStats : Array Row -> String -> Stat
computeBoolishStats rows key =
    rows
        |> Array.foldl
            (\row stat ->
                case row |> Dict.get key |> Maybe.andThen (D.decodeValue maybeBoolean >> Result.toMaybe) of
                    Just (Just True) ->
                        { stat | true = stat.true + 1 }

                    Just (Just False) ->
                        { stat | false = stat.false + 1 }

                    _ ->
                        { stat | blank = stat.blank + 1 }
            )
            { true = 0, false = 0, blank = 0 }
        |> Boolish


{-| Replace text at the given matches, recording the inverse so Undo covers it.
Find/replace used to call changeDoc directly, which left the undo stack silently
skipping every replacement it made.
-}
replaceMatches : Model -> Sheet -> FindReplace -> Table -> List Index -> ( Model, Cmd Msg )
replaceMatches model sheet fr tbl matches =
    let
        toDoc =
            displayYToDocY model.search sheet tbl.rows

        -- A match found before the search or a filter changed can name a row
        -- that is no longer drawn. Any one refuses the whole replace.
        undrawn =
            matches |> List.map .y |> List.filter (\y -> toDoc y == Nothing)

        patchPairs =
            matches
                |> List.filterMap
                    (\matchIdx ->
                        Maybe.map2
                            (\col docY ->
                                let
                                    old =
                                        Array.get (docY - 1) tbl.rows
                                            |> Maybe.andThen (Dict.get col.key)
                                            |> Maybe.withDefault E.null
                                in
                                ( { action = "set"
                                  , path = [ E.int docY, E.string col.key ]
                                  , value = E.string fr.replaceText
                                  }
                                , { action = "set"
                                  , path = [ E.int docY, E.string col.key ]
                                  , value = old
                                  }
                                )
                            )
                            (Array.get matchIdx.x tbl.cols)
                            (toDoc matchIdx.y)
                    )

        forward =
            List.map Tuple.first patchPairs
    in
    if not (List.isEmpty undrawn) then
        ( { model | error = undrawnRows undrawn }, Cmd.none )

    else if List.isEmpty forward then
        ( model, Cmd.none )

    else
        ( { model
            | sheet =
                { sheet
                    | undoStack = { forward = forward, backward = List.map Tuple.second patchPairs } :: sheet.undoStack |> List.take 50
                    , redoStack = []
                }
          }
        , changeDoc { id = sheet.id, data = forward }
        )


{-| The column this rename would collide with, if any. Every read of a sheet is
keyed by column name, so two columns of one name have no row a reader can key:
the rename syncs happily, the table on screen looks right, and every export,
every `select * from @this` and the sheet's own API answer 400 from then on,
far from the edit that caused it. `POST /import/csv` refuses the same shape in a
file, for the same reason.

Renaming a column to what it is already called is not a clash, and neither is
renaming one of an already-colliding pair to something new -- that one is the
repair.

-}
nameClash : Array Col -> Int -> String -> Maybe String
nameClash cols x write =
    if Maybe.map .name (Array.get x cols) == Just write then
        Nothing

    else
        cols |> Array.filter (\col -> col.name == write) |> Array.get 0 |> Maybe.map .name


{-| The `library:lineage` rows that name a sheet they depend on. A row with a
null `depends_on` names none, so it is not one.
-}
lineageDecoder : D.Decoder (List { name : String, dependsOn : String, columns : String })
lineageDecoder =
    D.list
        (D.field "depends_on" (D.nullable D.string)
            |> D.andThen
                (\on ->
                    case on of
                        Just dependsOn ->
                            D.map2 (\name columns -> Just { name = name, dependsOn = dependsOn, columns = columns })
                                (D.field "name" D.string)
                                (D.field "columns" D.string)

                        Nothing ->
                            D.succeed Nothing
                )
        )
        |> D.map (List.filterMap identity)


{-| Each sheet that reads one of `names` off sheet `id`, and why it is at risk.
`columns` is the server's comma-and-space join, `*` or `?`. A name is found
only between separators, never inside another name, and a name that itself
holds ", " can match where it should not: a warning too many, never one too few.
-}
dependents : Id -> List String -> List { name : String, dependsOn : String, columns : String } -> List ( String, String )
dependents id names =
    List.filterMap
        (\row ->
            let
                listed name =
                    (row.columns == name)
                        || String.startsWith (name ++ ", ") row.columns
                        || String.endsWith (", " ++ name) row.columns
                        || String.contains (", " ++ name ++ ", ") row.columns
            in
            if row.dependsOn /= id then
                Nothing

            else if row.columns == "*" then
                Just ( row.name, "selects every column" )

            else if row.columns == "?" then
                Just ( row.name, "reads columns nobody could list" )

            else if row.columns == "" then
                -- No claimed name. The server claims a name for neither ref
                -- when two joined refs both hold it, so such a row can still
                -- read the column, and no warning names it.
                Nothing

            else
                case List.filter listed names of
                    [] ->
                        Nothing

                    hit ->
                        Just ( row.name, "reads " ++ String.join ", " (List.map quoted hit) )
        )


quoted : String -> String
quoted name =
    E.encode 0 (E.string name)


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback first =
    case first of
        Just _ ->
            first

        Nothing ->
            fallback


computeStats : Doc -> Result String (Array Stat)
computeStats doc =
    case doc of
        Tab tbl ->
            Ok <|
                Array.map
                    (\col ->
                        case col.typ of
                            Number ->
                                computeNumericStats tbl.rows col.key

                            Usd ->
                                computeNumericStats tbl.rows col.key

                            Text ->
                                computeTextStats tbl.rows col.key

                            Date ->
                                computeTemporalStats tbl.rows col.key

                            Timestamp ->
                                computeTemporalStats tbl.rows col.key

                            Boolean ->
                                computeBoolishStats tbl.rows col.key

                            _ ->
                                Enumerative
                    )
                    tbl.cols

        _ ->
            Err ""


emptyNetTable : Table
emptyNetTable =
    { cols = Array.fromList [ madeCol "created_at" "created_at" Timestamp, madeCol "body" "body" Json ]
    , rows = Array.empty
    }


resolveTable : Model -> Result String Table
resolveTable model =
    case ( model.sheet.doc, model.sheet.table ) of
        ( Ok (Tab tbl), _ ) ->
            Ok tbl

        ( Ok (Unviewable typ), _ ) ->
            Err (unviewable typ model.sheet.id)

        ( Ok Library, _ ) ->
            Ok
                { cols = libraryCols model
                , rows =
                    model.library
                        |> Dict.filter (\k v -> k /= "" && not v.scratch && v.trashed == model.trash && List.any (String.contains model.search) (k :: v.name :: v.tags))
                        |> Dict.toList
                        -- Starred first, and only while nobody has chosen a
                        -- sort: `filterAndSortIndexed`'s own sort is stable, so
                        -- leaving this pre-sort in place once a real sort is
                        -- active would keep starred rows first among that
                        -- sort's ties -- a header click choosing its own order
                        -- and losing to one nobody asked it to keep.
                        |> iif (List.isEmpty model.sheet.sort) (List.sortBy (\( _, v ) -> iif v.starred 0 1)) identity
                        |> List.map
                            (\( k, v ) ->
                                Dict.fromList
                                    [ ( "sheet_id", E.string k )
                                    , ( "star", E.object [ ( "id", E.string k ), ( "on", E.bool v.starred ) ] )
                                    , ( "thumb", v.thumb )
                                    , ( "type", E.string (Maybe.withDefault "" <| List.head <| String.split ":" k) )
                                    , ( "name", E.string (iif (String.isEmpty (String.trim v.name)) "(untitled)" v.name) )
                                    , ( "tags", E.list E.string v.tags )
                                    , ( "opened", E.string v.seen )
                                    , ( "freshness", E.string (freshnessCell (Dict.get k model.freshness)) )
                                    , ( "trash", E.string k )
                                    , ( "restore", E.string k )
                                    , ( "delete", iif v.system E.null (E.string k) )
                                    ]
                            )
                        |> Array.fromList
                }

        ( _, Ok tbl ) ->
            Ok tbl

        ( Ok NetHook, Err "" ) ->
            Ok emptyNetTable

        ( Ok (NetHttp _), Err "" ) ->
            Ok emptyNetTable

        ( Ok (NetSocket _), Err "" ) ->
            Ok emptyNetTable

        ( Ok (Alert _), Err "" ) ->
            Ok emptyNetTable

        ( Ok (Chart _), Err "" ) ->
            Ok { cols = Array.fromList [ madeCol "x" "x" Text, madeCol "y" "y" Number, madeCol "y2" "y2" Number ], rows = Array.empty }

        ( Ok (Dashboard tiles), _ ) ->
            Ok
                { cols = Array.fromList [ madeCol "tile" "tile" SheetId ]
                , rows = tiles |> List.map (\t -> Dict.singleton "tile" (E.string (String.dropLeft 1 t))) |> Array.fromList
                }

        ( Err err1, Err err2 ) ->
            -- Both empty means nothing has loaded yet, which the view shows as "loading"
            Err (String.trim (err1 ++ " " ++ err2))

        ( _, Err err ) ->
            Err err


{-| A cell edit on a synced sheet, applied as the patches automerge emitted
rather than by decoding the whole document again: a `put` at
`["data", row, key]`, and for text the `put` of `""` that automerge follows
with a `splice` of the characters at `["data", row, key, offset]`. Anything
else -- a column, a row inserted, a view field, a patch this browser made up
-- answers `Nothing`, and the caller decodes the document. Columns live in
`data[0]`, which no patch admitted here addresses, so the column set is what
it was and `pruneView` has nothing to prune on this path.
-}
applyCellPatches : List D.Value -> Result String Doc -> Maybe (Result String Doc)
applyCellPatches patches doc =
    case ( patches, doc ) of
        ( [], _ ) ->
            Nothing

        ( _, Ok (Tab table) ) ->
            List.foldl (\patch acc -> Maybe.andThen (applyCellPatch patch) acc) (Just table) patches
                |> Maybe.map (Tab >> Ok)

        _ ->
            Nothing


applyCellPatch : D.Value -> Table -> Maybe Table
applyCellPatch patch table =
    let
        cell : Int -> (Row -> Maybe Row) -> Maybe Table
        cell ri edit =
            if ri >= 1 then
                Array.get (ri - 1) table.rows
                    |> Maybe.andThen edit
                    |> Maybe.map (\row -> { table | rows = Array.set (ri - 1) row table.rows })

            else
                Nothing

        at : Int -> D.Decoder a -> D.Decoder a
        at i decoder =
            D.field "path" (D.index i decoder)

        put =
            D.map3 (\ri ck val -> cell ri (Dict.insert ck val >> Just))
                (at 1 D.int)
                (at 2 D.string)
                (D.field "value" D.value)

        splice =
            D.map4
                (\ri ck offset chars ->
                    cell ri
                        (\row ->
                            Dict.get ck row
                                |> Maybe.andThen (D.decodeValue D.string >> Result.toMaybe)
                                |> Maybe.map (\was -> Dict.insert ck (E.string (String.left offset was ++ chars ++ String.dropLeft offset was)) row)
                        )
                )
                (at 1 D.int)
                (at 2 D.string)
                (at 3 D.int)
                (D.field "value" D.string)

        -- The action and the exact depth under "data", so a column's field at
        -- depth 4 is not read as a cell at depth 3 with something after it.
        shape action depth decoder =
            D.map3 (\a root path -> a == action && root == "data" && List.length path == depth)
                (D.field "action" D.string)
                (at 0 D.string)
                (D.field "path" (D.list D.value))
                |> D.andThen (\matches -> iif matches decoder (D.fail "not this patch"))
    in
    D.decodeValue (D.oneOf [ shape "put" 3 put, shape "splice" 4 splice ]) patch
        |> Result.toMaybe
        |> Maybe.andThen identity


applyFilter : Filter -> String -> Row -> Bool
applyFilter filter key row =
    let
        val =
            Dict.get key row |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""
    in
    case filter of
        TextContains substr ->
            String.contains (String.toLower substr) (String.toLower val)


matchesSearch : String -> Row -> Bool
matchesSearch search row =
    if String.isEmpty search then
        True

    else
        let
            vals =
                Dict.values row |> List.filterMap (D.decodeValue string >> Result.toMaybe)

            lower =
                String.toLower
        in
        case String.uncons search of
            Just ( '+', term ) ->
                List.any (\v -> String.contains (lower term) (lower v)) vals

            Just ( '-', term ) ->
                not (List.any (\v -> String.contains (lower term) (lower v)) vals)

            Just ( '=', term ) ->
                List.any (\v -> lower v == lower term) vals

            _ ->
                List.any (\v -> String.contains (lower search) (lower v)) vals


filterAndSort : String -> Sheet -> Array Row -> Array Row
filterAndSort search sheet rows =
    filterAndSortIndexed search sheet rows |> Array.map Tuple.second


filterAndSortIndexed : String -> Sheet -> Array Row -> Array ( Int, Row )
filterAndSortIndexed search sheet rows =
    let
        passes ( _, row ) =
            matchesSearch search row
                && Dict.foldl (\key filter acc -> acc && applyFilter filter key row) True sheet.filters

        filtered =
            Array.indexedMap Tuple.pair rows |> Array.filter passes
    in
    if List.isEmpty sheet.sort then
        filtered

    else
        Array.fromList (List.sortWith (compareBySort sheet.sort) (Array.toList filtered))



-- Insert rows by splice, the inverse of the splice SheetRowDelete already undoes with.
-- `offset` 0 puts the new row above its source, 1 below. Doc row i is rows[i - 1].


{-| The patches that take a set of document rows out, and the ones that put them
back. Out highest first, so an earlier splice never shifts a later target; back
lowest first, so each row lands at the index it left. Document row i is
`rows[i - 1]`, because row 0 is the column list.

Written once because the off-by-one is the whole difficulty and two verbs want
it: deleting the selected rows, and dropping the rows a column has nothing in.

-}
rowDeletions : Array Row -> List Int -> ( List Patch, List Patch )
rowDeletions rows indices =
    let
        -- Both sides come off one list, so a delete cannot outlive its undo. An
        -- index with no row behind it -- 0, which is the column list, or one past
        -- the end -- used to splice something out and put nothing back.
        targets =
            indices
                |> Set.fromList
                |> Set.toList
                |> List.filterMap (\i -> Array.get (i - 1) rows |> Maybe.map (Tuple.pair i))
    in
    ( targets
        |> List.reverse
        |> List.map (\( i, _ ) -> { action = "splice", path = [], value = E.list E.int [ i, 1 ] })
    , targets
        |> List.map
            (\( i, row ) ->
                { action = "splice"
                , path = []
                , value = E.list identity [ E.int i, E.int 0, E.dict identity identity row ]
                }
            )
    )


{-| One column's cells, rewritten. A cell that is not text is left alone -- trim
and case have nothing to say about a number -- and so is one the change does not
move: a patch that writes back what was already there is a step on the undo
stack that undoes nothing, and a change event every other viewer has to answer.

Every row the document holds, not the rows on screen: this is the column's own
verb, offered in the column's own panel, and a filter left on would otherwise
clean half a column and leave no sign of which half.

-}
cellRewrites : Col -> (String -> String) -> Array Row -> ( List Patch, List Patch )
cellRewrites col change rows =
    let
        pairs =
            rows
                |> Array.toIndexedList
                |> List.filterMap
                    (\( i, row ) ->
                        Dict.get col.key row
                            |> Maybe.andThen (D.decodeValue D.string >> Result.toMaybe)
                            |> Maybe.andThen
                                (\was ->
                                    let
                                        now =
                                            change was

                                        at value =
                                            { action = "set", path = [ E.int (i + 1), E.string col.key ], value = E.string value }
                                    in
                                    iif (now == was) Nothing (Just ( at now, at was ))
                                )
                    )
    in
    ( List.map Tuple.first pairs, List.map Tuple.second pairs )


{-| Nothing in this cell: the column is missing from the row, its value is null,
or it is text that is only whitespace. A zero is not blank.
-}
blankCell : String -> Row -> Bool
blankCell key row =
    case Dict.get key row of
        Nothing ->
            True

        Just value ->
            (D.decodeValue (D.nullable D.string) value == Ok Nothing)
                || (D.decodeValue D.string value |> Result.map (String.trim >> String.isEmpty) |> Result.withDefault False)


{-| The document rows whose cell in this column holds nothing.
-}
blankRows : Col -> Array Row -> List Int
blankRows col rows =
    rows
        |> Array.toIndexedList
        |> List.filterMap (\( i, row ) -> iif (blankCell col.key row) (Just (i + 1)) Nothing)


{-| The document rows that repeat a row above them, keyed on every cell they
hold: the first of a repeat stays and the ones under it are what
`SheetRowsDedupe` deletes. Every row the document holds, not the rows on screen,
the way the column's cleaning verbs read them -- a filter left on would
otherwise dedupe half a sheet against itself.

The row's own keys, never the column list: a cell whose key `data[0]` no longer
names is still a cell, and two peers -- one splicing the column out, one writing
that column's cell -- merge to exactly that. Signing off the columns instead
deleted the row holding it, and a sheet whose column list was empty signed every
row the same and collapsed to its first.

A blank cell reads as blank whichever way it is spelled, so two empty rows are
one row. Everything else compares as the JSON the document holds, so two strings
repeat only when they are the same string, code point for code point: nothing
here folds case or normalizes an accent, because a row this deletes cannot be
had back except through undo.

-}
duplicateRows : Array Row -> List Int
duplicateRows rows =
    let
        -- A row as one string, cell by cell, in the key order `Dict.toList`
        -- keeps so the same row signs the same way whatever order it was
        -- written in. Key and value are both JSON text and NUL joins them,
        -- because NUL is the one character JSON text cannot carry unescaped:
        -- no pair of cells can spell another pair's signature between them.
        signature row =
            row
                |> Dict.toList
                |> List.filterMap (\( key, value ) -> iif (blankCell key row) Nothing (Just (E.encode 0 (E.string key) ++ E.encode 0 value)))
                |> String.join "\u{0000}"
    in
    rows
        |> Array.toIndexedList
        |> List.foldl
            (\( i, row ) ( seen, repeats ) ->
                let
                    key =
                        signature row
                in
                iif (Set.member key seen) ( seen, (i + 1) :: repeats ) ( Set.insert key seen, repeats )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse


{-| Two strings' trigram overlap, 0 to 1. The same rule `similarity()` is in
src/sql.mjs -- padded, lower-cased, three characters at a time, and the Jaccard
of the two sets -- because the verb in the column's panel and the UDF in a query
must call the same two rows the same distance apart. `MainTest.elm` and
`main_test.ts` assert the same pairs, so the two cannot drift quietly.
-}
similarity : String -> String -> Float
similarity a b =
    let
        shared =
            Set.size (Set.intersect (trigrams a) (trigrams b))

        total =
            Set.size (trigrams a) + Set.size (trigrams b) - shared
    in
    -- Two empty strings are the same string; anything against an empty one
    -- shares nothing.
    iif (total == 0) 1 (toFloat shared / toFloat total)


trigrams : String -> Set.Set String
trigrams s =
    let
        padded =
            "  " ++ String.toLower s ++ " "
    in
    List.range 0 (String.length padded - 3)
        |> List.map (\i -> String.slice i (i + 3) padded)
        |> Set.fromList


{-| The soundex code of a word, the same four characters `soundex()` answers in
src/sql.mjs. It is what buckets the rows before anything is compared: comparing
every pair of a sheet is quadratic, and a browser cannot do that to a real sheet.
-}
soundex : String -> String
soundex word =
    case String.toList (String.filter Char.isAlpha (String.toUpper word)) of
        [] ->
            ""

        first :: rest ->
            let
                -- H and W are transparent: they do not break a run of
                -- same-coded letters.
                step ch ( out, last ) =
                    let
                        c =
                            soundexCode ch
                    in
                    if String.length out == 4 then
                        ( out, last )

                    else
                        ( iif (c /= "" && c /= last) (out ++ c) out, iif (ch == 'H' || ch == 'W') last c )
            in
            List.foldl step ( String.fromChar first, soundexCode first ) rest
                |> Tuple.first
                |> String.padRight 4 '0'


soundexCode : Char -> String
soundexCode ch =
    if String.any ((==) ch) "BFPV" then
        "1"

    else if String.any ((==) ch) "CGJKQSXZ" then
        "2"

    else if String.any ((==) ch) "DT" then
        "3"

    else if ch == 'L' then
        "4"

    else if String.any ((==) ch) "MN" then
        "5"

    else if ch == 'R' then
        "6"

    else
        ""


{-| The most rows a near-duplicate search reads.
-}
maxFuzzyRows : Int
maxFuzzyRows =
    5000


{-| The most comparisons it makes, which is the bound that actually matters.

Bucketing by soundex is what keeps this out of the quadratic case, and the row
count alone assumed it works. It does not always: `soundex` keeps only letters,
so a column of invoice numbers, zip codes or phone numbers held as text codes to
`""` on every row and the whole sheet is one bucket -- which is the commonest
column anybody would point this verb at. Five thousand such rows is twelve
million comparisons and tens of seconds of a frozen tab, and since the preview is
drawn from `view` it is tens of seconds **per keystroke** in the closeness box.

So the work is bounded rather than the input, and exceeding it is a refusal
carrying the counter. The number is what keeps one keystroke's worth of
comparing under a tenth of a second.

-}
maxFuzzyPairs : Int
maxFuzzyPairs =
    50000


{-| The document rows whose cell in this column is nearly, but not exactly, the
cell of a row above them: the row that would go, the row it matched, and how
close the two are as a percentage.

The first of a near-group stays and the ones under it go, the way the exact verb
already works -- and against the rows that stay, not against the ones already
going, so three spellings of one name collapse to the first rather than to a
chain of pairs.

Rows are bucketed by `soundex` first and compared only inside a bucket. That is
what makes this runnable in a browser, and it is also a rule: two strings whose
first consonants differ are not near each other however many trigrams they share.

Every way it cannot run is an `Err` rather than an empty answer, because the
panel draws this as a preview and "nothing to do" and "this cannot be done" are
not the same sentence.

-}
nearDuplicates : Col -> Int -> Array Row -> Result String (List ( Int, Int, Int ))
nearDuplicates col closeness rows =
    let
        texts =
            rows
                |> Array.toIndexedList
                |> List.filterMap
                    (\( i, row ) ->
                        Dict.get col.key row
                            |> Maybe.andThen (D.decodeValue D.string >> Result.toMaybe)
                            |> Maybe.andThen (\v -> iif (String.trim v == "") Nothing (Just ( i + 1, v )))
                    )
    in
    if closeness < 1 || closeness > 100 then
        Err
            ("Expected how close counts as a percentage between 1 and 100, received "
                ++ String.fromInt closeness
                ++ ". Source: the near box on this column. Fix: type a number between 1 and 100, e.g. 85."
            )

    else if Array.length rows > maxFuzzyRows then
        Err
            ("Expected at most "
                ++ String.fromInt maxFuzzyRows
                ++ " rows to compare, received "
                ++ String.fromInt (Array.length rows)
                ++ ". Source: the near box on "
                ++ col.name
                ++ ". Fix: filter the sheet down, or dedupe it with a query."
            )

    else if List.isEmpty texts then
        Err
            ("Expected text to compare in "
                ++ col.name
                ++ ", received none. Source: every row's cell in this column is blank or is not text. Fix: run this on a column of names."
            )

    else
        let
            ( _, found, compared ) =
                List.foldl
                    (\( i, value ) ( kept, going, pairs ) ->
                        -- Past the bound nothing more is compared, so the cost
                        -- stops where the bound is rather than after it.
                        if pairs > maxFuzzyPairs then
                            ( kept, going, pairs )

                        else
                            let
                                bucket =
                                    soundex value

                                held =
                                    Dict.get bucket kept |> Maybe.withDefault []

                                spent =
                                    pairs + List.length held
                            in
                            case firstNear closeness value held of
                                Just ( j, pct ) ->
                                    ( kept, ( i, j, pct ) :: going, spent )

                                Nothing ->
                                    ( Dict.update bucket (\was -> Just (Maybe.withDefault [] was ++ [ ( i, value ) ])) kept, going, spent )
                    )
                    ( Dict.empty, [], 0 )
                    texts
        in
        if compared > maxFuzzyPairs then
            Err
                ("Expected at most "
                    ++ String.fromInt maxFuzzyPairs
                    ++ " comparisons, received more than that from "
                    ++ String.fromInt (List.length texts)
                    ++ " rows of "
                    ++ col.name
                    ++ ". Source: too many of these values sound alike to compare them pair by pair -- a column of numbers held as text sounds alike on every row. Fix: filter the sheet down, or dedupe it with a query."
                )

        else
            Ok (List.reverse found)


{-| The first row already kept that this value is near enough to, or nothing.

Recursive rather than a filter and a head, because only the first match is used
and a filter reads the whole bucket to find it -- which is the quadratic half of
this verb, paid in full on every row even when the answer was the first one.

An exact repeat is the other verb's: this one is for the rows it misses.

-}
firstNear : Int -> String -> List ( Int, String ) -> Maybe ( Int, Int )
firstNear closeness value held =
    case held of
        [] ->
            Nothing

        ( j, other ) :: rest ->
            let
                pct =
                    round (100 * similarity value other)
            in
            if pct >= closeness && other /= value then
                Just ( j, pct )

            else
                firstNear closeness value rest


{-| The most columns one split may push. A cell with more parts than this says
the delimiter matches something the values are made of rather than something
between them -- a space against a column of sentences -- and the sheet it would
push is unreadable and no faster to undo than to rebuild.
-}
maxSplitColumns : Int
maxSplitColumns =
    64


{-| One text column, split into columns of its own. Every row's cell is split on
the delimiter as text -- `String.split` matches the characters it is given, so a
"." is a dot and a "|" is a bar, never a pattern -- and the widest split says how
many columns are pushed: `<name> 1` .. `<name> n`, each typed text and keyed the
way `SheetColumnPush` keys a new one. A row with fewer parts than the widest
leaves its later cells unwritten, a cell that is not text has nothing to split,
and the column split from stays where it is.

The undo is the mirror: one splice taking the pushed columns back off `data[0]`,
and a `del` per cell written, the way `SheetColumnDelete` puts back exactly what
it took.

Nothing at all is written when the split cannot be one every reader can key: an
empty delimiter, a key this sheet does not carry, a column with no text in it, a
delimiter no cell holds, a split past `maxSplitColumns`, or a name one of the new
columns would collide with. Half a split -- the columns before the clash pushed
and the clashing one left out -- is the shape `nameClash` exists to refuse.

-}
columnSplit : Array Col -> Array Row -> String -> String -> Result String ( List Patch, List Patch )
columnSplit cols rows key delimiter =
    let
        -- Where the pushed columns land, which is the end of the column list,
        -- and what they are keyed. The key is a number the way `SheetColumnPush`
        -- writes one, but past every number this sheet already uses: a sheet
        -- that has had a column deleted carries a key at its own length, and a
        -- second column of that key would take its cells over.
        at =
            Array.length cols

        base =
            cols
                |> Array.toList
                |> List.filterMap (.key >> String.toInt)
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
                |> max at
    in
    case cols |> Array.filter (\c -> c.key == key) |> Array.get 0 of
        Nothing ->
            Err ("Expected a column of this sheet to split, received the key \"" ++ key ++ "\", which it does not carry. Source: the split button in a column panel. Fix: reopen the sheet and split the column again.")

        Just col ->
            if delimiter == "" then
                Err ("Expected the text that separates the parts of \"" ++ col.name ++ "\", received an empty box. Source: the split button in that column's panel. Fix: type what its values are separated by -- a comma, a space, a word -- and split again.")

            else
                let
                    -- Every row the document holds, not the rows on screen, the
                    -- way the cleaning verbs beside this one read them.
                    rowParts =
                        rows
                            |> Array.toIndexedList
                            |> List.filterMap
                                (\( i, row ) ->
                                    Dict.get col.key row
                                        |> Maybe.andThen (D.decodeValue D.string >> Result.toMaybe)
                                        |> Maybe.map (\text -> ( i + 1, String.split delimiter text ))
                                )

                    widest =
                        rowParts |> List.map (Tuple.second >> List.length) |> List.maximum |> Maybe.withDefault 0

                    names =
                        List.range 1 widest |> List.map (\n -> col.name ++ " " ++ String.fromInt n)
                in
                if List.isEmpty rowParts then
                    Err ("Expected a column with text in it, received \"" ++ col.name ++ "\", whose " ++ String.fromInt (Array.length rows) ++ " rows hold no text cell at all. Source: the split button in that column's panel. Fix: split a column whose values are text -- a number, a date and a blank have no parts.")

                else if widest < 2 then
                    Err ("Expected a value of \"" ++ col.name ++ "\" carrying \"" ++ delimiter ++ "\", received " ++ String.fromInt (List.length rowParts) ++ " text cells and not one of them holding it. Source: the split button in that column's panel. Fix: type the separator these values actually use, then split again.")

                else if widest > maxSplitColumns then
                    Err ("Expected a split into at most " ++ String.fromInt maxSplitColumns ++ " columns, received one row of \"" ++ col.name ++ "\" that splits into " ++ String.fromInt widest ++ ". Source: the split button in that column's panel. Fix: split on a separator that stands between the values rather than inside them.")

                else
                    case names |> List.filterMap (nameClash cols at) |> List.head of
                        Just taken ->
                            Err ("Expected room for the columns \"" ++ col.name ++ " 1\" .. \"" ++ col.name ++ " " ++ String.fromInt widest ++ "\", received a sheet that already has a column called \"" ++ taken ++ "\". Source: the split button in that column's panel. Fix: rename that column, or rename \"" ++ col.name ++ "\", then split again.")

                        Nothing ->
                            let
                                cells =
                                    rowParts
                                        |> List.concatMap
                                            (\( y, values ) ->
                                                values
                                                    |> List.indexedMap
                                                        (\n value ->
                                                            { action = "set"
                                                            , path = [ E.int y, E.string (String.fromInt (base + n)) ]
                                                            , value = E.string value
                                                            }
                                                        )
                                            )
                            in
                            Ok
                                ( { action = "push"
                                  , path = [ E.int 0 ]
                                  , value =
                                        names
                                            |> List.indexedMap (\n name -> E.object [ ( "name", E.string name ), ( "type", E.string "text" ), ( "key", E.int (base + n) ) ])
                                            |> E.list identity
                                  }
                                    :: cells
                                , List.map (\cell -> { action = "del", path = cell.path, value = E.null }) cells
                                    ++ [ { action = "splice", path = [ E.int 0 ], value = E.list E.int [ at, widest ] } ]
                                )


{-| How a filled-down series is written back, and Nothing for a column no series
belongs in.

The seeds come out of `cellText`, which renders any JSON as display text, so a
series is text whatever the column holds. Writing that text back is only right
where the column holds text: a num column that held 10 and 20 got the strings
"30" and "40", a bool column got "true" where `E.bool True` had been, and a json
column's objects were overwritten with "a: 3". `checkColumnTypes` coerces the
numeric types on the way out and leaves the rest, so those survived every read --
and MCP's `write_cells` refuses exactly the value the page's own fill-down wrote.
A date and a timestamp are text on the way in and out, so a filled one is written
the way `fillSeries` spelled it.

Exhaustive on purpose: a new column type has to say which of the three it is.

-}
seriesEncoder : Type -> Maybe (String -> E.Value)
seriesEncoder typ =
    let
        asNumber text =
            String.toFloat text |> Maybe.map E.float |> Maybe.withDefault (E.string text)
    in
    case typ of
        Text ->
            Just E.string

        Link ->
            Just E.string

        SheetId ->
            Just E.string

        Image ->
            Just E.string

        Thumb ->
            Just E.string

        Enum _ ->
            Just E.string

        Number ->
            Just asNumber

        Usd ->
            Just asNumber

        Percentage ->
            Just asNumber

        Unknown ->
            Nothing

        Boolean ->
            Nothing

        Date ->
            Just E.string

        Timestamp ->
            Just E.string

        Json ->
            Nothing

        Many _ ->
            Nothing

        Delete ->
            Nothing

        Trash ->
            Nothing

        Restore ->
            Nothing

        Star ->
            Nothing

        Create ->
            Nothing

        Form ->
            Nothing


{-| The values that carry a column on below its seeds. Two or more dates step by
the days or the months between the last pair, one date steps by a day, two or
more numbers continue the step between the last pair, one number counts up by
one, text ending in digits counts those digits up, and anything else repeats the
last seed.

The calendar is `justinmimbs/date`, never arithmetic on the text: the trailing
digits of "2026-01-31" counted January on to a 32nd day. What comes back is an
ISO day, then whatever the last seed carried after its first ten characters, so
a timestamp column keeps its time of day.

Everything this writes is a value a float carries and a cell can hold. A step
that overflowed, a counter past what a float counts exactly, and a seed written
in exponent form all used to write a word, a repeated id or a zero into the
document instead.

-}
fillSeries : List String -> Int -> List String
fillSeries seeds count =
    let
        last =
            List.reverse seeds |> List.head |> Maybe.withDefault ""

        numbers =
            List.filterMap (String.trim >> String.toFloat) seeds

        -- `parseDay` is what says a seed is a date, here and in `countable`
        -- both. It counts days from 1970-01-01 and `Date` counts them from
        -- 0001-01-01, which is rata die 1: 1970-01-01 is rata die 719163.
        days =
            List.filterMap parseDay seeds

        asDate epochDay =
            Date.fromRataDie (epochDay + 719163)

        -- The seeds' own precision: 1, 2 counts in whole numbers and 0.5, 1.0
        -- in tenths. Bounded by the count a column may ask for, and for the
        -- same reason: two cells pasted at a double's full precision asked for
        -- twenty-two places, which `fixed` cannot write.
        decimals =
            seeds
                |> List.map (String.split "." >> List.drop 1 >> List.head >> Maybe.map String.length >> Maybe.withDefault 0)
                |> List.maximum
                |> Maybe.withDefault 0
                |> min maxDecimals

        tail =
            last
                |> String.foldr (\c ( stopped, acc ) -> iif (stopped || not (Char.isDigit c)) ( True, acc ) ( False, String.cons c acc )) ( False, "" )
                |> Tuple.second

        -- The trailing digits, when they are a counter this can carry on. Not a
        -- date, which `parseDay` is what says. Not more than fifteen digits
        -- either: `String.toInt` accumulates in a float and `String.fromInt`
        -- writes one, so a nineteen-digit id came back rounded and every filled
        -- row got the same one.
        countable =
            iif (parseDay last == Nothing && String.length tail <= 15) (String.toInt tail) Nothing

        -- What a step of the series is written as. A value the seeds' own
        -- precision cannot spell is written the way Elm writes it: a seed in
        -- exponent form carries no dot at all -- `String.fromFloat` writes 1e-8
        -- that way and that is what a num cell hands back -- so `fixed 0` filled
        -- the column with zeros. A step that left the floats behind is not a
        -- number at all, so it repeats the seed the way anything else this
        -- cannot continue does: "Infinity" and "NaN" were landing in cells.
        written value =
            if isNaN value || isInfinite value then
                last

            else if decimals == 0 && value /= toFloat (round value) then
                String.fromFloat value

            else
                fixed decimals value
    in
    -- Every seed a date, or none of them: one date among numbers is a column
    -- somebody is still typing into, and repeating is what that asks for.
    case ( List.length days == List.length seeds, List.reverse days ) of
        ( True, latest :: earlier ) ->
            let
                start =
                    asDate latest

                -- Months when the later date is the earlier one plus whole
                -- months landing on the same day of the month, which is the
                -- only thing that tells a month step from a step of the days
                -- between the two. One date steps by a day.
                ( unit, step ) =
                    case earlier of
                        before :: _ ->
                            let
                                from =
                                    asDate before

                                months =
                                    12 * (Date.year start - Date.year from) + (Date.monthNumber start - Date.monthNumber from)
                            in
                            iif (months /= 0 && Date.toRataDie (Date.add Date.Months months from) == Date.toRataDie start)
                                ( Date.Months, months )
                                ( Date.Days, latest - before )

                        [] ->
                            ( Date.Days, 1 )
            in
            -- Off the last seed every time, never off the value written before
            -- it: a month past 2026-01-31 is 02-28, and a walk that carried the
            -- 28th on clamped every row after it to the 28th too.
            List.range 1 count
                |> List.map (\i -> Date.toIsoString (Date.add unit (step * i) start) ++ String.dropLeft 10 (String.trim last))

        _ ->
            -- Two numbers are what a step is, so two is what the pattern asks
            -- for. The caller comes here with one seed only for a date, and a
            -- default step of one for a shape that cannot arrive was a number
            -- this could not have known.
            case ( List.length numbers == List.length seeds, List.reverse numbers ) of
                ( True, latest :: previous :: _ ) ->
                    let
                        step =
                            latest - previous
                    in
                    List.range 1 count |> List.map (\i -> written (latest + step * toFloat i))

                _ ->
                    case countable of
                        Just counted ->
                            List.range 1 count
                                |> List.map (\i -> String.dropRight (String.length tail) last ++ (String.fromInt (counted + i) |> String.padLeft (String.length tail) '0'))

                        Nothing ->
                            List.repeat count last


rowSplices : (Int -> Maybe Row) -> Int -> List Int -> (Int -> Maybe Int) -> ( List Patch, List Patch )
rowSplices source offset indices toDoc =
    let
        -- `updateDocMsg` refuses the whole edit through `undrawnRows` before
        -- these patches go out when any index maps to nothing.
        targets =
            indices
                |> List.filterMap toDoc
                |> Set.fromList
                |> Set.toList
                |> List.filterMap (\i -> source i |> Maybe.map (\row -> ( i + offset, row )))

        -- Highest first, so an earlier splice never shifts a later target.
        forward =
            targets
                |> List.reverse
                |> List.map
                    (\( t, row ) ->
                        { action = "splice"
                        , path = []
                        , value = E.list identity [ E.int t, E.int 0, E.dict identity identity row ]
                        }
                    )

        -- Once every insert has landed, the i-th target (ascending) sits at t + i.
        backward =
            targets
                |> List.indexedMap (\i ( t, _ ) -> { action = "splice", path = [], value = E.list E.int [ t + i, 1 ] })
                |> List.reverse
    in
    ( forward, backward )


{-| Walk past hidden columns in the direction of travel. Hidden columns keep their
x coordinate, so navigation is what has to step over them.
-}
skipHidden : Sheet -> TableBounds -> Int -> Int -> Int
skipHidden sheet bounds dx x =
    let
        step n candidate =
            if n <= 0 || not (columnHiddenAt sheet candidate) then
                candidate

            else
                let
                    next =
                        candidate + iif (dx < 0) -1 1
                in
                if next < 0 || next > bounds.maxX then
                    x

                else
                    step (n - 1) next
    in
    iif (dx == 0) x (step (bounds.maxX + 1) x)


columnHiddenAt : Sheet -> Int -> Bool
columnHiddenAt sheet x =
    arrangeable sheet
        |> Maybe.andThen (\target -> Array.get x target.cols)
        |> Maybe.map (\col -> Set.member col.key sheet.hidden)
        |> Maybe.withDefault False


undrawnRows : List Int -> String
undrawnRows ys =
    let
        named =
            ys |> Set.fromList |> Set.toList |> List.map String.fromInt
    in
    "Expected an edit on rows this view draws, received one on "
        ++ iif (List.length named == 1) "row " "rows "
        ++ String.join ", " named
        ++ ", which this view does not draw. Nothing was written. Source: the search or a filter hides that row, or the rows changed after you selected it or found it. Fix: select a drawn row, or clear the search or the filter, and edit again."


displayYToDocY : String -> Sheet -> Array Row -> Int -> Maybe Int
displayYToDocY search sheet rows y =
    filterAndSortIndexed search sheet rows
        |> Array.get (y - 1)
        |> Maybe.map (\( orig, _ ) -> orig + 1)


{-| A dragged width wins over the type's own, and a type with no width of its
own sizes itself. Both come out of `spec`, in px, so a width cannot be changed
in one place and missed in another.
-}
colPx : Sheet -> Col -> Maybe Int
colPx sheet col =
    Dict.get col.key sheet.widths |> orElse (spec col.typ).width


colWidth : Sheet -> Col -> H.Attribute Msg
colWidth sheet col =
    case colPx sheet col of
        Just px ->
            S.width (String.fromInt px ++ "px")

        Nothing ->
            S.widthAuto


colOf : Sheet -> String -> Maybe Col
colOf sheet key =
    arrangeable sheet |> Maybe.andThen (\target -> target.cols |> Array.filter (\c -> c.key == key) |> Array.get 0)


{-| The left edge of every column that stays put while the table scrolls
sideways, in px: the widths of the sticky columns before it.

Column 0 is in the sum whether or not anybody pinned it -- `.c0` in
src/style.css keeps the row label in view regardless -- because a pinned column
that did not count it would land underneath it. A hidden column renders
`display: none` and so adds nothing.

-}
pinLeft : Sheet -> Array Col -> Dict String Int
pinLeft sheet cols =
    cols
        |> Array.toIndexedList
        |> List.filter (\( x, col ) -> (x == 0 || Set.member col.key sheet.pinned) && not (Set.member col.key sheet.hidden))
        |> List.foldl
            (\( _, col ) ( left, acc ) ->
                ( left + Maybe.withDefault autoColWidth (colPx sheet col), Dict.insert col.key left acc )
            )
            ( 0, Dict.empty )
        |> Tuple.second


pinAttrs : Dict String Int -> Col -> List (H.Attribute Msg)
pinAttrs pins col =
    case Dict.get col.key pins of
        Just left ->
            [ A.class "pin", S.left (String.fromInt left ++ "px") ]

        Nothing ->
            []


{-| Both corners at -1 on one axis span that whole axis. Every corner at -1 is
no selection.
-}
inSelection : Rect -> Int -> Int -> Bool
inSelection ({ a, b } as select) i n =
    let
        between a_ b_ i_ =
            min a_ b_ <= i_ && i_ <= max a_ b_

        eq a_ b_ i_ =
            a_ == i_ && i_ == b_
    in
    (select /= rect -1 -1 -1 -1) && (between a.x b.x i || eq a.x b.x -1) && (between a.y b.y n || eq a.y b.y -1)


cellClasses : Sheet -> Int -> Int -> H.Attribute Msg
cellClasses sheet i n =
    let
        cellIdx =
            xy i n

        isMatch =
            case sheet.findReplace of
                Just fr ->
                    List.member cellIdx fr.matches

                Nothing ->
                    False

        isCurrentMatch =
            case sheet.findReplace of
                Just fr ->
                    fr.matches |> List.drop fr.currentMatch |> List.head |> (==) (Just cellIdx)

                Nothing ->
                    False
    in
    A.classList
        [ ( "selected", inSelection sheet.select i n )
        , ( "r0", n == 0 )
        , ( "c0", i == 0 )
        , ( "match-highlight", isMatch && not isCurrentMatch )
        , ( "match-current", isCurrentMatch )
        ]


cellDecoder : Type -> Maybe Int -> Maybe NumberFormat -> Int -> Int -> D.Decoder (Maybe (Html Msg))
cellDecoder typ decimals format i n =
    D.maybe
        (case typ of
            Unknown ->
                D.map text string

            SheetId ->
                D.string |> D.map (\id -> H.a [ A.href ("/" ++ id), S.overflowVisible, S.whiteSpaceNowrap, S.paddingRightRem 0.5 ] [ text "view" ])

            Link ->
                D.string |> D.map (\href -> H.a [ A.href href, A.target "_blank", A.rel "noopener noreferrer", S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.wordBreakKeepAll, S.hyphensNone ] [ text "link" ])

            Image ->
                D.string |> D.map (\src -> H.img [ A.src src ] [])

            Text ->
                D.map text string

            Boolean ->
                boolean |> D.map (\c -> H.input [ A.type_ "checkbox", A.checked c, A.onCheck (DocMsg << CellCheck { x = i, y = n }) ] [])

            Number ->
                D.oneOf [ D.map (text << formatNumber Number decimals format) number, D.map text string ]

            Usd ->
                D.oneOf [ D.map (text << formatNumber Usd decimals format) number, D.map text string ]

            Percentage ->
                D.oneOf [ D.map (text << formatNumber Percentage decimals format) number, D.map text string ]

            Date ->
                D.map text string

            Json ->
                -- A flat array of numbers is a series, and a series is drawn.
                -- Everything else -- an object, a nested array, an array of one
                -- or of mixed values -- falls through to the lenient `string`,
                -- which is what a json cell has always rendered as.
                D.oneOf
                    [ D.list D.float
                        |> D.andThen
                            (sparkValues
                                >> Maybe.map (D.succeed << viewSpark)
                                >> Maybe.withDefault (D.fail "not two or more finite numbers")
                            )
                    , D.map text string
                    ]

            Enum _ ->
                D.map text string

            Delete ->
                D.string |> D.map (\sheet_id -> H.button [ A.onClick (DocDelete sheet_id) ] [ text "delete" ])

            Trash ->
                D.string |> D.map (\sheet_id -> H.button [ A.onClick (DocTrash sheet_id), A.title "move to the trash" ] [ text "trash" ])

            Restore ->
                D.string |> D.map (\sheet_id -> H.button [ A.onClick (DocRestore sheet_id) ] [ text "restore" ])

            Star ->
                -- The cell carries both halves, because the button is a toggle:
                -- which sheet, and what it is now. Reading the state off the
                -- glyph on screen is how a star that failed to write kept
                -- offering to unstar.
                D.map2
                    (\sheet_id on ->
                        H.button
                            [ A.onClick (DocStar sheet_id (not on))
                            , A.title (iif on "unstar this sheet" "star this sheet, to keep it at the top of the library")
                            ]
                            [ text (iif on "★" "☆") ]
                    )
                    (D.field "id" D.string)
                    (D.field "on" D.bool)

            Thumb ->
                D.map3 viewThumb
                    (D.field "cols" D.int)
                    (D.field "rows" D.int)
                    (D.oneOf [ D.field "spark" (D.list D.float), D.succeed [] ])

            Create ->
                D.value |> D.map (\val -> H.button [ A.onClick (DocNew val) ] [ text "add to library" ])

            Form ->
                D.map3
                    (\method _ fields ->
                        H.form [ A.onSubmit NoOp, S.displayGrid, S.gridTemplateColumns "auto 1fr", S.paddingRem 1 ] <|
                            List.concatMap (\f -> [ H.label [] [ text f.label ], H.input [] [] ]) fields
                                ++ [ H.span [] [], H.button [ A.type_ "submit" ] [ text method ] ]
                    )
                    (D.field "method" D.string)
                    (D.field "action" D.string)
                    (D.field "fields" (D.list (D.map (\label -> { label = label }) (D.field "label" D.string))))

            _ ->
                D.map text string
        )


{-| The most values one sparkline draws. A cell is a fixed width, so a longer
array loses its head and not its tail: the recent end of a series is the end a
reader reads.
-}
sparkMax : Int
sparkMax =
    64


{-| The last `sparkMax` numbers of a `json` cell, scaled to 0..1 by their own
extremes -- `Nothing` where there is no line to draw, and the cell falls back to
the text it always drew. All-equal values draw level: there is no range to
divide by.
-}
sparkValues : List Float -> Maybe (List Float)
sparkValues vs =
    let
        -- The window drawn, and the only values anything below asks about: a
        -- value older than `sparkMax` entries is off the picture, and one that
        -- is not finite there must not blank a window that is entirely finite.
        kept =
            List.drop (List.length vs - sparkMax) vs
    in
    case ( List.minimum kept, List.maximum kept ) of
        ( Just lo, Just hi ) ->
            if List.length kept < 2 || List.any (\v -> isNaN v || isInfinite v) kept then
                Nothing

            else if hi == lo then
                Just (List.map (\_ -> 0.5) kept)

            else
                Just (List.map (\v -> (v - lo) / (hi - lo)) kept)

        -- No values, so no extremes and no line.
        _ ->
            Nothing


{-| One thin bar per value, each already scaled to 0..1. The library thumbnail
(scaled by `docThumb` in src/page.mjs) and a `json` cell draw the one line.
-}
viewSpark : List Float -> Html Msg
viewSpark scaled =
    H.div [ S.displayFlex, S.alignItemsFlexEnd, S.gap "1px", S.heightPx 14 ] <|
        List.map (\h -> H.div [ S.widthPx 3, S.height (String.fromFloat (4 + h * 10) ++ "px"), S.backgroundColor "#bbb" ] []) scaled


viewThumb : Int -> Int -> List Float -> Html Msg
viewThumb cols rows spark =
    if not (List.isEmpty spark) then
        viewSpark spark

    else if cols > 0 then
        H.span [ S.color "#666", S.fontSizeRem 0.75 ] [ text (String.fromInt cols ++ "×" ++ String.fromInt rows) ]

    else
        text ""


viewStatCell : Type -> Maybe Int -> Maybe NumberFormat -> Maybe Stat -> List (Html Msg)
viewStatCell typ decimals format maybeStat =
    let
        grid =
            H.div [ S.displayGrid, S.gridTemplateColumns "auto auto", S.gap "0 0.5rem", S.justifyContentFlexStart, S.fontSizeRem 0.75 ]

        kv k v =
            [ H.span [] [ text k ], H.span [] [ text v ] ]
    in
    case maybeStat of
        Just (Numeric stat) ->
            [ grid <|
                kv "min" (Maybe.withDefault "" (Maybe.map (formatNumber typ decimals format) stat.min))
                    ++ kv "max" (Maybe.withDefault "" (Maybe.map (formatNumber typ decimals format) stat.max))
                    ++ kv "mean" (iif (stat.count == 0) "" (formatNumber typ decimals format (stat.sum / toFloat stat.count)))
                    ++ kv "count" (String.fromInt stat.count)
            ]

        Just (Descriptive stat) ->
            [ grid <|
                kv "min" (Maybe.withDefault "" (Maybe.map String.fromInt stat.min))
                    ++ kv "max" (String.fromInt stat.max)
                    ++ kv "mean" (iif (stat.count == 0) "" (String.fromInt (stat.sum // stat.count)))
                    ++ kv "count" (String.fromInt stat.count)
                    ++ [ H.span [] [ text "keywords" ], H.span [ S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.maxWidthRem 12, S.displayBlock ] [ text (String.join " " (Dict.keys (Dict.filter (\k v -> String.length k >= 4 && v >= 2) stat.keywords))) ] ]
            ]

        Just (Temporal stat) ->
            let
                span =
                    Maybe.map2 (\lo hi -> hi - lo + 1) (List.minimum (Set.toList stat.days)) (List.maximum (Set.toList stat.days))
            in
            [ grid <|
                kv "first" (Maybe.withDefault "" stat.first)
                    ++ kv "last" (Maybe.withDefault "" stat.last)
                    ++ kv "span" (Maybe.withDefault "" (Maybe.map (\d -> String.fromInt d ++ "d") span))
                    -- Days inside the range with no row: the holes a date spine would fill.
                    ++ kv "gaps" (Maybe.withDefault "" (Maybe.map (\d -> String.fromInt (d - Set.size stat.days)) span))
                    ++ kv "count" (String.fromInt stat.count)
            ]

        Just (Boolish stat) ->
            [ grid <|
                kv "true" (String.fromInt stat.true)
                    ++ kv "false" (String.fromInt stat.false)
                    ++ kv "blank" (String.fromInt stat.blank)
                    ++ kv "count" (String.fromInt (stat.true + stat.false))
            ]

        _ ->
            []


{-| The rows the near box would delete, named one by one, and the button that
does it. A refusal is drawn as the sentence it is: the same sentence
`updateDocMsg` would answer with, out of the same call, so the panel cannot
promise what the verb refuses.
-}
viewNearPreview : Col -> Int -> Array Row -> Html Msg
viewNearPreview col closeness rows =
    case nearDuplicates col closeness rows of
        Err message ->
            H.p [ S.fontSizeRem 0.8125, S.color "#c00", S.marginTop "0.25rem" ] [ text message ]

        Ok found ->
            let
                -- Rows this could not look at: the cell holds something, but not
                -- text, so there is nothing to compare. Counted rather than left
                -- for the reader to notice, the way a chart says how many points
                -- a fold swallowed -- a preview that says "1 row would go" while
                -- it never read half the sheet is a preview lying about the
                -- sheet.
                unread =
                    rows
                        |> Array.toList
                        |> List.filter
                            (\row ->
                                not (blankCell col.key row)
                                    && (Dict.get col.key row |> Maybe.andThen (D.decodeValue D.string >> Result.toMaybe))
                                    == Nothing
                            )
                        |> List.length
            in
            H.div [ S.marginTop "0.25rem", S.fontSizeRem 0.8125 ]
                [ H.button
                    [ A.onClick (DocMsg (SheetRowsDedupeNear col.key closeness))
                    , A.title "delete the rows listed below, keeping the first of each group"
                    ]
                    [ text ("Delete " ++ String.fromInt (List.length found) ++ " near-duplicate rows") ]
                , iif (unread == 0)
                    (text "")
                    (H.p [ S.color "#666", S.marginTop "0.25rem" ]
                        [ text
                            (iif (unread == 1)
                                "1 row holds no text in this column and was not compared"
                                (String.fromInt unread ++ " rows hold no text in this column and were not compared")
                            )
                        ]
                    )
                , H.ul [ S.color "#666", S.marginTop "0.25rem" ]
                    (found
                        |> List.take 6
                        |> List.map
                            (\( goes, kept, pct ) ->
                                H.li []
                                    [ text
                                        ("row " ++ String.fromInt goes ++ " matches row " ++ String.fromInt kept ++ " (" ++ String.fromInt pct ++ "%)")
                                    ]
                            )
                    )
                , iif (List.length found > 6)
                    (H.p [ S.color "#666" ] [ text ("and " ++ String.fromInt (List.length found - 6) ++ " more") ])
                    (text "")
                ]


viewHeaderCell : Sheet -> Col -> List (Html Msg)
viewHeaderCell sheet col =
    case col.name of
        "" ->
            []

        _ ->
            let
                -- The rank only appears once a second key is active, so a single
                -- sort still reads as a bare arrow.
                sortIndicator =
                    case ( sortOrderOf col.key sheet.sort, sortRankOf col.key sheet.sort ) of
                        ( Just order, Just rank ) ->
                            iif (order == Ascending) " ▲" " ▼"
                                ++ iif (List.length sheet.sort > 1) (String.fromInt rank) ""

                        _ ->
                            ""

                hasFilter =
                    Dict.member col.key sheet.filters

                isPinned =
                    Set.member col.key sheet.pinned

                controls =
                    arrangeControls sheet

                -- A table's columns are the document's own order, so only they
                -- can be moved, and only their cells are this document's to
                -- rewrite. A query's are its select list's, computed from
                -- somewhere else -- which is why the cleaning verbs in the panel
                -- below ask the same question the drag handle does.
                movable =
                    case sheet.doc of
                        Ok (Tab _) ->
                            True

                        _ ->
                            False

                -- Only a column whose cells are numbers has a decimal count to
                -- ask for; every other type reads its cells as the text they
                -- already are.
                numeric =
                    numericColumn col.typ

                isFilterOpen =
                    sheet.filterOpen == Just col.key

                currentFilterValue =
                    case Dict.get col.key sheet.filters of
                        Just (TextContains v) ->
                            v

                        _ ->
                            ""
            in
            [ H.div [ S.displayFlex, S.flexDirectionColumn, S.positionRelative ]
                [ H.div [ S.displayFlex, S.alignItemsCenter, S.gapRem 0.25 ]
                    [ iif movable
                        (H.span
                            [ A.class "grab"
                            , A.title "drag onto the column it should sit at"
                            , A.stopPropagationOn "mousedown" (D.succeed ( ColumnMoveStart col.key, True ))
                            ]
                            []
                        )
                        (text "")
                    , iif isPinned (H.span [ A.class "pinned", A.title "pinned" ] [ text "📌" ]) (text "")
                    , iif controls
                        (H.span [ A.class "sort", S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.fontWeight "600", S.cursorPointer, A.on "click" (D.map (\shift -> ColumnSort shift col.key) (D.field "shiftKey" D.bool)), A.title "shift-click to add a sort key" ]
                            [ text (col.name ++ sortIndicator) ]
                        )
                        (H.span [ S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.fontWeight "600" ] [ text col.name ])
                    , iif controls
                        (H.span [ A.classList [ ( "funnel", True ), ( "on", hasFilter ) ], S.cursorPointer, S.fontSizeRem 0.75, A.onClick (FilterToggle col.key), A.title "filter" ]
                            [ text (iif hasFilter "⧩" "▽") ]
                        )
                        (text "")
                    , iif controls
                        (H.span
                            [ A.class "grip"
                            , A.title "drag to resize"
                            , A.on "mousedown" (D.map (ColumnResizeStart col.key) (D.field "clientX" (D.map round D.float)))
                            ]
                            []
                        )
                        (text "")
                    ]
                , if isFilterOpen then
                    H.div [ A.class "panel", S.positionAbsolute, S.top "100%", S.left "0", S.padding "0.5rem", S.zIndex "100", S.minWidth "150px", S.fontSizeRem 0.875 ]
                        [ H.input [ A.placeholder "contains...", A.attribute "aria-label" ("filter " ++ col.name), A.value currentFilterValue, A.onInput (FilterInput col.key), S.width "100%" ] []
                        , H.button [ A.onClick (ColumnHide col.key), S.marginTop "0.25rem" ] [ text "Hide column" ]
                        , H.button [ A.onClick (ColumnPin col.key), S.marginTop "0.25rem" ] [ text (iif isPinned "Unpin column" "Pin column") ]
                        , iif numeric
                            (H.label [ A.class "decimals", S.displayFlex, S.alignItemsCenter, S.gapRem 0.25, S.marginTop "0.25rem" ]
                                [ text "decimals"
                                , H.input
                                    [ A.type_ "number"
                                    , A.min "0"
                                    , A.max (String.fromInt maxDecimals)
                                    , A.placeholder "auto"
                                    , A.value (Dict.get col.key sheet.decimals |> Maybe.map String.fromInt |> Maybe.withDefault "")
                                    , A.onInput (ColumnDecimals col.key)
                                    , S.widthRem 4
                                    ]
                                    []
                                ]
                            )
                            (text "")
                        , iif numeric
                            (H.label [ A.class "format", S.displayFlex, S.alignItemsCenter, S.gapRem 0.25, S.marginTop "0.25rem" ]
                                [ text "format"
                                , H.select [ A.onInput (ColumnFormat col.key) ]
                                    (H.option [ A.value "", A.selected (Dict.get col.key sheet.formats == Nothing) ] [ text "auto" ]
                                        :: List.map
                                            (\format ->
                                                H.option
                                                    [ A.value (formatSpec format).name
                                                    , A.selected (Dict.get col.key sheet.formats == Just format)
                                                    ]
                                                    [ text (formatSpec format).label ]
                                            )
                                            numberFormats
                                    )
                                ]
                            )
                            (text "")
                        , iif numeric
                            (H.label [ A.class "shading", S.displayFlex, S.alignItemsCenter, S.gapRem 0.25, S.marginTop "0.25rem" ]
                                [ text "shade"
                                , H.select [ A.onInput (ColumnShade col.key) ]
                                    (H.option [ A.value "", A.selected (Dict.get col.key sheet.shades == Nothing) ] [ text "none" ]
                                        :: List.map
                                            (\shading ->
                                                H.option
                                                    [ A.value (shadeSpec shading).name
                                                    , A.selected (Dict.get col.key sheet.shades == Just shading)
                                                    ]
                                                    [ text (shadeSpec shading).label ]
                                            )
                                            shades
                                    )
                                ]
                            )
                            (text "")
                        , if movable then
                            H.div [ S.displayFlex, S.flexWrapWrap, S.gapRem 0.25, S.marginTop "0.25rem" ]
                                [ H.button [ A.onClick (DocMsg (SheetColumnTrim col.key)), A.title "drop the spaces around every value in this column" ] [ text "Trim" ]
                                , H.button [ A.onClick (DocMsg (SheetColumnCase col.key Upper)) ] [ text "UPPER" ]
                                , H.button [ A.onClick (DocMsg (SheetColumnCase col.key Lower)) ] [ text "lower" ]
                                , H.button [ A.onClick (DocMsg (SheetRowsDropBlank col.key)), A.title "delete every row with nothing in this column" ] [ text "Drop blank rows" ]
                                , H.label [ A.class "split", S.displayFlex, S.alignItemsCenter, S.gapRem 0.25 ]
                                    [ H.input
                                        [ A.placeholder "split on"
                                        , A.title "the text between the parts, matched as the characters it is -- a . is a dot and a | is a bar"
                                        , A.value sheet.splitOn
                                        , A.onInput ColumnSplitInput
                                        , S.widthRem 4
                                        ]
                                        []
                                    , H.button [ A.onClick (DocMsg (SheetColumnSplit col.key sheet.splitOn)), A.title "add a column per part, leaving this one where it is" ] [ text "Split" ]
                                    ]
                                , H.label [ A.class "near", S.displayFlex, S.alignItemsCenter, S.gapRem 0.25 ]
                                    [ text "near %"
                                    , H.input
                                        [ A.type_ "number"
                                        , A.min "1"
                                        , A.max "100"
                                        , A.placeholder "85"
                                        , A.title "how close two spellings count as the same row; 100 is the exact repeat the palette already deletes"
                                        , A.value sheet.near
                                        , A.onInput ColumnNearInput
                                        , S.widthRem 4
                                        ]
                                        []
                                    ]
                                ]

                          else
                            text ""
                        , -- What the near box would do, before it is done.
                          -- Unlike every other verb in this panel, nobody can
                          -- see the answer by looking at the sheet: the rows it
                          -- deletes are the ones that do not look alike enough
                          -- to spot. So it is drawn rather than described, and
                          -- the button carries the count it would take.
                          -- Off `sheet.doc` and not `sheet.table`: a table
                          -- sheet's rows are its document, which is also where
                          -- `updateDocMsg` reads them, so the preview and the
                          -- verb cannot be looking at two different sheets.
                          case ( movable, String.toInt sheet.near, sheet.doc ) of
                            ( True, Just closeness, Ok (Tab tbl) ) ->
                                viewNearPreview col closeness tbl.rows

                            _ ->
                                text ""
                        , if hasFilter then
                            H.button [ A.onClick (FilterClear col.key), S.marginTop "0.25rem" ] [ text "Clear" ]

                          else
                            text ""
                        ]

                  else
                    text ""
                ]
            ]


viewEditCell : Sheet -> Col -> List (Html Msg)
viewEditCell sheet col =
    case col.typ of
        Enum options ->
            [ H.select
                [ A.id "new-cell", onEditorKeydown, A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (DocMsg (SheetWrite sheet.select.a)), S.width "100%", S.height "100%", S.border "none", S.borderRadius "0", S.padding "0" ]
                (H.option [ A.value "" ] [ text "-- select --" ]
                    :: List.map (\opt -> H.option [ A.value opt, A.selected (Just opt == sheet.write) ] [ text opt ]) options
                )
            ]

        _ ->
            [ H.input [ A.id "new-cell", onEditorKeydown, A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (DocMsg (SheetWrite sheet.select.a)), S.width "100%", S.height "100%", S.minWidthRem 8, S.border "none", S.borderRadius "0", S.padding "0" ] [] ]


{-| The background one shaded cell's value sits on.

It goes on a wrapper inside the `td` and never on the `td` itself: an inline
background there outranks `td.selected`, `td:hover` and the match highlight by
specificity, so selection and find would vanish on every shaded column.

The colour is the theme's own `--accent`, mixed into what is behind it rather
than spelled out here, so the shade and the rest of the app cannot disagree
about which blue this is.

-}
shadeAttr : Shade -> ( Float, Float ) -> Float -> H.Attribute Msg
shadeAttr shading ( lo, hi ) v =
    let
        -- Where the value sits between the column's ends, or nothing at all
        -- for a column whose every value is the same: there is no spread to
        -- read, and the division would be by zero. Every value of such a
        -- column is its largest as well as its smallest, which is why the
        -- bar that stands in for the missing fraction is a whole one.
        part =
            iif (hi == lo) Nothing (Just ((v - lo) / (hi - lo)))
    in
    case shading of
        Scale ->
            -- Faint at the bottom of the column and no stronger than the
            -- cell's own text stays readable over at the top.
            S.backgroundColor
                ("color-mix(in srgb, var(--accent) "
                    ++ String.fromInt (round (8 + 40 * Maybe.withDefault 0.5 part))
                    ++ "%, transparent)"
                )

        Bars ->
            let
                across =
                    String.fromInt (round (100 * Maybe.withDefault 1 part)) ++ "%"
            in
            S.backgroundImage
                ("linear-gradient(to right, color-mix(in srgb, var(--accent) 30%, transparent) "
                    ++ across
                    ++ ", transparent "
                    ++ across
                    ++ ")"
                )


viewCell : Sheet -> Result String (Array Stat) -> Dict String Int -> Dict String ( Float, Float ) -> Bool -> Int -> Int -> Col -> Row -> Html Msg
viewCell sheet stats pins extents grab i n col row =
    H.td
        ([ A.onClick CellMouseClick
         , A.onDoubleClick <|
            CellMouseDoubleClick <|
                case String.fromInt n of
                    "0" ->
                        col.name

                    "-1" ->
                        col.raw

                    "-2" ->
                        col.raw

                    _ ->
                        row |> Dict.get col.key |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""
         , A.onMouseDown CellMouseDown
         , A.onMouseUp CellMouseUp
         , A.onMouseEnter (CellHover (xy i n))
         , iif (n == 0 && sheet.filterOpen == Just col.key) (S.zIndex "2") (A.classList [])
         , cellClasses sheet i n
         , iif (inSelection sheet.select i n) (A.attribute "aria-selected" "true") (A.classList [])
         , -- A `td` in a `role="grid"` table is a gridcell already, so only a header cell names its role.
           iif (n == 0) (A.attribute "role" "columnheader") (A.classList [])
         , (spec col.typ).align
         , colWidth sheet col
         , iif (Set.member col.key sheet.hidden) S.displayNone (A.classList [])
         ]
            ++ pinAttrs pins col
        )
    <|
        if sheet.write /= Nothing && sheet.select == rect i n i n then
            viewEditCell sheet col

        else
            case String.fromInt n of
                "-2" ->
                    viewStatCell col.typ (Dict.get col.key sheet.decimals) (Dict.get col.key sheet.formats) (Maybe.andThen (Array.get i) (Result.toMaybe stats))

                "-1" ->
                    [ H.p [ S.displayBlock, S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.fontSizeRem 0.75 ] [ text col.raw ] ]

                "0" ->
                    viewHeaderCell sheet col

                _ ->
                    let
                        value =
                            row
                                |> Dict.get col.key
                                |> Maybe.withDefault (E.string "")
                                |> D.decodeValue (cellDecoder col.typ (Dict.get col.key sheet.decimals) (Dict.get col.key sheet.formats) i n)
                                |> Result.map (Maybe.withDefault (text ""))
                                |> Result.mapError (D.errorToString >> text)
                                |> (\r ->
                                        case r of
                                            Ok x ->
                                                x

                                            Err x ->
                                                x
                                   )

                        -- This arm is the data rows; the header, the stats and
                        -- the totals are other arms, and go unshaded.
                        --
                        -- The cell is read a second time, and only for a column
                        -- somebody shaded: Elm is strict, so asking for the
                        -- number beside the two lookups would decode every cell
                        -- of every column on every keystroke. `positional` is
                        -- the guard `columnExtent` already puts on the ends of
                        -- the scale, asked again of the cell sitting on it, so
                        -- a JSON `1e400` -- Infinity, still typeof "number" --
                        -- draws no shade rather than a background written
                        -- "Infinity%".
                        shaded =
                            case ( Dict.get col.key sheet.shades, Dict.get col.key extents ) of
                                ( Just shading, Just extent ) ->
                                    row
                                        |> Dict.get col.key
                                        |> Maybe.andThen (D.decodeValue number >> Result.toMaybe)
                                        |> Maybe.andThen (\v -> iif (positional v) (Just v) Nothing)
                                        |> Maybe.map (shadeAttr shading extent)

                                _ ->
                                    Nothing
                    in
                    [ -- There is no row-number cell, so the first data cell carries
                      -- the row's handle. Only while the table is in document order:
                      -- `grab` is decided where the rows on screen are known.
                      iif (i == 0 && grab)
                        (H.span
                            [ A.class "grab"
                            , A.title "drag onto the row it should sit at"
                            , A.stopPropagationOn "mousedown" (D.succeed ( RowMoveStart n, True ))
                            ]
                            []
                        )
                        (text "")
                    , case shaded of
                        Just background ->
                            H.div [ A.class "shade", background ] [ value ]

                        Nothing ->
                            value
                    ]


viewTableRow : Sheet -> Doc -> Result String (Array Stat) -> Dict String Int -> Dict String ( Float, Float ) -> Bool -> Array Col -> Int -> Row -> Html Msg
viewTableRow sheet doc stats pins extents grab cols n row =
    H.tr
        [ A.classList [ ( "meta", n < 0 ) ]
        , case ( String.fromInt n, stats ) of
            ( "-2", Err _ ) ->
                S.displayNone

            _ ->
                S.displayTableRow
        ]
    <|
        List.indexedMap (\i col -> viewCell sheet stats pins extents grab i n col row) (Array.toList cols)
            ++ [ case doc of
                    Tab _ ->
                        H.th [ A.onClick (DocMsg SheetColumnPush), A.title "add column", S.widthRem 0.001, S.whiteSpaceNowrap ] [ text (iif (n == 0) "→" "") ]

                    _ ->
                        text ""
               ]


viewFilterBar : Sheet -> Int -> Int -> Html Msg
viewFilterBar sheet filteredCount totalCount =
    if Dict.isEmpty sheet.filters && Set.isEmpty sheet.hidden then
        text ""

    else
        H.div [ S.padding "0.25rem 0.5rem", S.backgroundColor "#fff8e0", S.borderBottom "1px solid #e0d8a0", S.fontSizeRem 0.875, S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter ]
            [ H.span [] [ text (filterBarLabel sheet filteredCount totalCount) ]
            , H.div [ S.displayFlex, S.gapRem 0.25 ]
                [ iif (Set.isEmpty sheet.hidden)
                    (text "")
                    (H.button [ A.onClick ColumnsShowAll, S.padding "0.125rem 0.5rem" ] [ text "Show all columns" ])
                , iif (Dict.isEmpty sheet.filters)
                    (text "")
                    (H.button [ A.onClick (FilterClear ""), S.padding "0.125rem 0.5rem" ] [ text "Clear all filters" ])
                ]
            ]


filterBarLabel : Sheet -> Int -> Int -> String
filterBarLabel sheet filteredCount totalCount =
    String.join ", " <|
        List.filterMap identity
            [ iif (Dict.isEmpty sheet.filters)
                Nothing
                (Just ("Showing " ++ String.fromInt filteredCount ++ " of " ++ String.fromInt totalCount ++ " rows"))
            , iif (Set.isEmpty sheet.hidden)
                Nothing
                (Just (String.fromInt (Set.size sheet.hidden) ++ " columns hidden"))
            ]


{-| Sum of a numeric column over the rows actually on screen. Deliberately not
`sheet.stats`, which is computed over every document row: a totals line that
ignored the active filter would contradict the rows above it.
-}
columnTotal : Array Row -> Col -> Maybe Float
columnTotal rows col =
    case col.typ of
        Number ->
            Just (sumColumn rows col.key)

        Usd ->
            Just (sumColumn rows col.key)

        _ ->
            Nothing


{-| The smallest and largest number a column holds over the rows actually on
screen, or nothing where it holds no number at all. Deliberately not
`sheet.stats`, for the reason the totals line beside it is not: stats are
computed over every document row, so a shade that ignored the active filter
would paint a scale nobody on screen can read -- and a query sheet has no stats
at all.

A blank cell and a cell that is not a number are passed over rather than
counted as zero: the extent is the range of the values there are.

A cell past what `positional` allows is passed over the same way -- JSON's own
`1e400` decodes to Infinity without ever failing `number`'s check, typeof
"number" in the host either way, and folding it in with `min`/`max` would
stretch the whole column's scale to an endpoint no finite cell holds.

-}
columnExtent : Array Row -> Col -> Maybe ( Float, Float )
columnExtent rows col =
    Array.foldl
        (\row acc ->
            case row |> Dict.get col.key |> Maybe.andThen (D.decodeValue number >> Result.toMaybe) |> Maybe.andThen (\v -> iif (positional v) (Just v) Nothing) of
                Just v ->
                    case acc of
                        Just ( lo, hi ) ->
                            Just ( min lo v, max hi v )

                        Nothing ->
                            Just ( v, v )

                Nothing ->
                    acc
        )
        Nothing
        rows


sumColumn : Array Row -> String -> Float
sumColumn rows key =
    Array.foldl
        (\row acc ->
            row
                |> Dict.get key
                |> Maybe.andThen (D.decodeValue number >> Result.toMaybe)
                |> Maybe.withDefault 0
                |> (+) acc
        )
        0
        rows



-- The library is one flat table, so the only thing telling a demo pipeline from
-- a reference table is its tags. This strip names the demos and turns every tag
-- into a filter, which is what makes an empty grid the second thing you see
-- rather than the first.


viewGallery : Model -> Html Msg
viewGallery model =
    let
        demos =
            model.library
                |> Dict.filter (\k v -> String.startsWith "query:" k && List.member "demo" v.tags && not v.trashed)
                |> Dict.toList
                |> List.sortBy (\( _, v ) -> v.name)

        tags =
            model.library
                |> Dict.values
                |> List.concatMap .tags
                |> List.filter (\t -> not (List.member t [ "query", "example" ]))
                |> Set.fromList
                |> Set.toList

        -- The strip is where a demo is opened from, so a sheet that has failed
        -- since its last good run says so here rather than only in the library
        -- column below. Marked by its own freshness, not by its type: the read
        -- covers feeds and alerts today and is where connection health is
        -- headed, and this rule needs no edit when it arrives.
        rotten id =
            model.freshness |> Dict.get id |> Maybe.map (\f -> f.failures > 0) |> Maybe.withDefault False

        trashed =
            model.library |> Dict.filter (\k v -> k /= "" && not v.scratch && v.trashed) |> Dict.size
    in
    H.div [ S.paddingRem 0.5, S.backgroundColor "#f6f6f6", S.borderBottom "1px solid #aaa", S.displayFlex, S.flexWrapWrap, S.gapRem 0.375, S.alignItemsCenter, S.fontSizeRem 0.8125 ]
        (H.strong [] [ text "start from a demo" ]
            :: List.map
                (\( k, v ) ->
                    H.a
                        [ A.class "chip"
                        , A.href ("/" ++ k)
                        , A.title k
                        , iif (rotten k) (S.color "#b00") (A.classList [])
                        ]
                        [ text (iif (rotten k) (v.name ++ " ⚠") v.name) ]
                )
                demos
            ++ H.span [ S.color "#666", S.marginLeftRem 0.5 ] [ text "filter" ]
            :: List.map (\t -> H.button [ A.class "chip", A.onClick (InputChange SheetSearch t) ] [ text t ]) tags
            -- Shown while the trash is open even when it has just been emptied,
            -- because it is the only way back to the library from in there.
            ++ iif (trashed == 0 && not model.trash)
                []
                [ H.button
                    [ A.class "chip"
                    , A.onClick TrashToggle
                    , A.title (iif model.trash "back to the library" "what this browser threw away")
                    , iif model.trash (S.background "#fff8e0") (A.classList [])
                    ]
                    [ text ("🗑 " ++ String.fromInt trashed) ]
                ]
            -- The box holds the argument and the chip runs the verb, so the
            -- tag is typed where the rows it lands on are selected.
            ++ [ H.input
                    [ A.value model.sheet.tag
                    , A.onInput TagInput
                    , onTagKeydown
                    , A.placeholder "tag"
                    , A.title "add this tag to every selected library row"
                    , S.width "6rem"
                    , S.fontSizeRem 0.8125
                    ]
                    []
               , H.button [ A.class "chip", A.onClick TagSelected ] [ text "tag selected" ]
               ]
        )


viewTableFooter : Bool -> Sheet -> Dict String Int -> Array Col -> Array Row -> Html Msg
viewTableFooter trash sheet pins cols rows =
    H.tfoot [] <|
        case sheet.doc of
            -- Nothing is offered to make while the trash is open: a sheet made
            -- there would land in the library and disappear from the view that
            -- made it.
            Ok Library ->
                iif trash [] <|
                    List.map
                        (\( label, msg ) ->
                            H.tr [ A.onClick msg, A.title ("new " ++ label) ] <|
                                H.td [] [ text label ]
                                    :: List.map (\typ -> H.td [] [ text typ ]) [ "text", "list text", "" ]
                        )
                        [ ( "table:...", DocNewTable )
                        , ( "query:...", DocNewQuery )
                        , ( "net-hook:...", DocNew <| E.object [ ( "type", E.string "net-hook" ), ( "data", E.list identity [] ) ] )
                        , ( "net-http:...", DocNew <| E.object [ ( "type", E.string "net-http" ), ( "data", E.list identity [ E.object [ ( "url", E.string "" ), ( "interval", E.int 3600 ) ] ] ) ] )
                        , ( "net-socket:...", DocNew <| E.object [ ( "type", E.string "net-socket" ), ( "data", E.list identity [ E.object [ ( "url", E.string "" ) ] ] ) ] )
                        , ( "alert:...", DocNew <| E.object [ ( "type", E.string "alert" ), ( "data", E.list identity [ E.object [ ( "code", E.string "" ), ( "to", E.string "" ), ( "interval", E.int 3600 ), ( "digest", E.bool False ) ] ] ) ] )
                        , ( "chart:...", DocNew <| E.object [ ( "type", E.string "chart" ), ( "data", E.list identity [ E.object [ ( "source", E.string "" ), ( "kind", E.string "line" ), ( "x", E.string "" ), ( "y", E.string "" ) ] ] ) ] )
                        , ( "dashboard:...", DocNew <| E.object [ ( "type", E.string "dashboard" ), ( "data", E.list identity [ E.object [ ( "tiles", E.list E.string [] ) ] ] ) ] )
                        ]
                        ++ [ H.tr [] <|
                                H.td []
                                    [ H.label []
                                        [ text "import csv..."
                                        , H.input [ A.type_ "file", A.accept ".csv,text/csv", A.on "change" (D.at [ "target", "files" ] (D.index 0 File.decoder) |> D.map CsvImportFile), S.display "none" ] []
                                        ]
                                    ]
                                    :: List.map (\typ -> H.td [] [ text typ ]) [ "text", "list text", "" ]
                           ]

            Ok (Tab _) ->
                [ H.tr [ A.class "totals", A.title "totals for the rows shown" ] <|
                    List.map
                        (\col ->
                            H.td ([ S.textAlignRight, S.fontWeight "600", iif (Set.member col.key sheet.hidden) S.displayNone (A.classList []) ] ++ pinAttrs pins col)
                                [ text (Maybe.withDefault "" (Maybe.map (formatNumber col.typ (Dict.get col.key sheet.decimals) (Dict.get col.key sheet.formats)) (columnTotal rows col))) ]
                        )
                        (Array.toList cols)
                        ++ [ H.th [ S.widthRem 0.001, S.whiteSpaceNowrap ] [] ]
                , H.tr [ A.onClick (DocMsg SheetRowPush), A.title "add row" ] <|
                    List.map (\col -> H.td (iif (Set.member col.key sheet.hidden) S.displayNone (A.classList []) :: pinAttrs pins col) [ text col.raw ]) (Array.toList cols)
                        ++ [ H.th [ S.widthRem 0.001, S.whiteSpaceNowrap ] [ text "↴" ] ]
                ]

            _ ->
                []


viewError : String -> Html Msg
viewError error =
    case error of
        "" ->
            text ""

        _ ->
            H.div [ A.class "mono", S.backgroundColor "#fee", S.border "1px solid #c88", S.borderRadius "4px", S.padding "0.75rem", S.margin "0.5rem", S.fontSizeRem 0.8125, S.whiteSpacePre, S.overflowXAuto ]
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.marginBottomRem 0.5 ]
                    [ H.strong [] [ text "Error" ]
                    , H.button [ A.class "x", A.attribute "aria-label" "dismiss this error", A.onClick (DocError "") ] [ text "×" ]
                    ]
                , text error
                ]


viewAuthForm : Auth -> Html Msg
viewAuthForm auth =
    case auth.state of
        LoggedIn _ ->
            text ""

        _ ->
            H.form [ A.id "account", A.onSubmit (AuthMsg AuthSubmit), S.displayGrid, S.gapRem 0.5, S.maxWidth "100vw", S.width "100%", S.gridTemplateColumns "1fr 1fr auto", S.paddingRem 0.5, S.borderTop "1px solid #aaa", S.backgroundColor "#f0f0f0", S.positionAbsolute, S.bottomPx 0, S.zIndex "10" ]
                [ H.input [ S.minWidthRem 2, A.placeholder "email", A.attribute "aria-label" "email", A.type_ "email", A.name "email", A.value auth.email, A.onInput (InputChange AuthEmail), A.disabled (auth.state == LoggingIn) ] []
                , H.input [ S.minWidthRem 2, A.placeholder "password", A.attribute "aria-label" "password", A.type_ "password", A.name "password", A.value auth.password, A.onInput (InputChange AuthPassword), A.disabled (auth.state == LoggingIn) ] []
                , H.button [ A.type_ "submit", A.disabled (auth.state == LoggingIn) ]
                    [ text (iif (auth.state == LoggingIn) "..." (iif (String.isEmpty auth.password) "signup" "login")) ]
                ]


viewToolbar : Model -> SheetInfo -> Html Msg
viewToolbar model info =
    let
        sheet =
            model.sheet
    in
    H.div [ S.displayFlex, S.flexDirectionRow, S.alignItemsCenter, S.whiteSpaceNowrap, S.gapRem 0.5, S.paddingRem 0.5, S.borderBottom "1px solid #aaa", S.background "#f0f0f0" ] <|
        List.concat
            [ [ H.a [ A.href "/", A.title "library", A.attribute "aria-label" "library", S.fontWeight "900", S.fontSizeRem 1.25, S.lineHeight "1" ] [ text "⊞" ]
              , H.a [ A.href "/", A.id "title", S.fontWeight "900", S.marginLeftRem -0.25 ] [ text "scrapsheets" ]
              , text "/"
              ]
            , case model.auth.state of
                LoggedIn { usrId } ->
                    [ H.span [] [ text ("user:" ++ usrId) ], text "/" ]

                _ ->
                    [ H.span [] [ text "anon" ], text "/" ]
            , iif (sheet.id == "")
                [ H.span [] [ text "library" ] ]
                [ H.a [ A.href "#settings", A.title "sheet settings", S.textDecoration "underline dotted" ] [ text (iif (String.trim info.name == "") "untitled" info.name) ] ]
            , case sheet.lineage of
                Just source ->
                    [ text "/", H.a [ A.href ("/" ++ source), A.title ("forked from " ++ source), S.color "#666" ] [ text "forked" ] ]

                Nothing ->
                    []
            , iif (sheet.id == "")
                []
                [ H.button [ A.class "chip", A.onClick DocFork, A.title "copy this sheet into one of your own", S.marginLeftAuto ] [ text "fork" ] ]
            , [ H.button [ A.class "chip", A.onClick (ShortcutsToggle True), iif (sheet.id == "") S.marginLeftAuto (A.classList []) ] [ text "keys" ] ]
            , case sheet.doc of
                Ok (Tab _) ->
                    -- A bundled sheet is a plain object in the page, with no automerge document to have a history.
                    iif info.system [] [ H.button [ A.class "chip", A.onClick (HistoryMsg HistoryOpen), A.title "read this sheet as it was" ] [ text "history" ] ]
                        ++ List.map
                            (\format -> H.a [ A.class "chip", A.href (model.api ++ "/export/" ++ sheet.id ++ "." ++ format), A.download (sheet.id ++ "." ++ format) ] [ text ("export " ++ format) ])
                            [ "csv", "xlsx" ]

                Ok (Chart _) ->
                    List.map
                        (\format -> H.button [ A.class "chip", A.onClick (ChartDownload format), A.title ("save the chart as " ++ format) ] [ text format ])
                        [ "svg", "png" ]

                _ ->
                    []
            , case model.auth.state of
                LoggedIn _ ->
                    [ H.button [ A.class "chip", A.onClick (AuthMsg AuthLogout) ] [ text "log out" ] ]

                _ ->
                    []
            ]


viewTutorial : Maybe Int -> Html Msg
viewTutorial tutorial =
    case tutorial of
        Nothing ->
            text ""

        Just step ->
            H.div [ A.class "panel", S.positionFixed, S.bottomRem 3, S.rightRem 1, S.paddingRem 1, S.zIndex "90", S.maxWidthRem 18 ]
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter, S.gapRem 1, S.marginBottomRem 0.5 ]
                    [ H.strong [] [ text "get started" ]
                    , H.button [ A.class "x", A.attribute "aria-label" "dismiss the tutorial", A.onClick TutorialDismiss ] [ text "×" ]
                    ]
                , H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ] <|
                    List.indexedMap
                        (\i ( label, hint ) ->
                            if i < step then
                                H.div [ S.color "#666" ] [ text ("✓ " ++ label) ]

                            else if i == step then
                                H.div []
                                    [ H.div [ S.fontWeight "700" ] [ text label ]
                                    , H.div [ S.fontSizeRem 0.75, S.color "#666" ] [ text hint ]
                                    ]

                            else
                                H.div [ S.color "#666" ] [ text label ]
                        )
                        [ ( "create a table", "click table:... below" )
                        , ( "edit a cell", "click a cell and type" )
                        , ( "create a query", "click query:... below" )
                        , ( "reference a sheet with @", "type @ in the editor and pick a table" )
                        , ( "see live results", "results update as you type" )
                        ]
                ]


viewNetWarning : Model -> Html Msg
viewNetWarning model =
    case model.auth.state of
        Anonymous ->
            H.div [ S.backgroundColor "#fff8e0", S.border "1px solid #e0d8a0", S.borderRadius "4px", S.padding "0.5rem", S.fontSizeRem 0.875 ]
                [ text "Net sheets need an account: log in so the server can store this sheet's data." ]

        _ ->
            text ""


viewNetHook : Model -> Html Msg
viewNetHook model =
    let
        url =
            model.api ++ "/net/" ++ model.sheet.id
    in
    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5, S.paddingRem 1, S.minWidth "25vw" ]
        [ viewNetWarning model
        , H.h3 [] [ text "webhook inbox" ]
        , H.div [ A.class "mono", S.fontSizeRem 0.8125, S.backgroundColor "#f0f0f0", S.padding "0.5rem", S.borderRadius "4px", S.overflowXAuto ]
            [ text url ]
        , H.button [ A.onClick (CopyText url) ] [ text "copy" ]
        , H.p [ S.fontSizeRem 0.875, S.color "#666" ]
            [ text "POST to this URL and rows appear in the table. Every delivery must be signed: without a scrapsheets-signature header it is refused, because otherwise anyone who learns this sheet's id can write to it." ]
        , case model.share.hook of
            Nothing ->
                H.button [ A.onClick ShareHook ] [ text "show signing secret" ]

            Just hook ->
                H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5 ]
                    [ H.label [ S.fontSizeRem 0.875 ] [ text "signing secret" ]
                    , H.div [ A.class "mono", S.fontSizeRem 0.8125, S.backgroundColor "#f0f0f0", S.padding "0.5rem", S.borderRadius "4px", S.overflowXAuto ]
                        [ text hook.secret ]
                    , H.button [ A.onClick (CopyText hook.secret) ] [ text "copy secret" ]
                    , H.label [ S.fontSizeRem 0.875 ] [ text "send one" ]
                    , H.pre [ A.class "mono", S.fontSizeRem 0.75, S.backgroundColor "#f0f0f0", S.padding "0.5rem", S.borderRadius "4px", S.overflowXAuto ]
                        [ text hook.repro ]
                    , H.button [ A.onClick (CopyText hook.repro) ] [ text "copy curl" ]
                    ]
        ]


viewNetHttp : Model -> { url : String, interval : Int, headers : String, method : String, body : String, pageBy : String, pageParam : String, pagePath : String, mode : String, key : String, rowsPath : String, paused : Bool, cron : String, timezone : String } -> Html Msg
viewNetHttp model cfg =
    let
        paging =
            pageForm cfg.pageBy

        storing =
            storeForm cfg.mode
    in
    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5, S.paddingRem 1, S.minWidth "25vw" ]
        [ viewNetWarning model
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "URL"
            , H.input [ A.type_ "text", A.value cfg.url, A.onInput (InputChange NetUrl) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "method"
            , H.select [ A.value cfg.method, A.onInput (InputChange NetMethod) ] <|
                List.map (\m -> H.option [ A.value m, A.selected (m == cfg.method) ] [ text m ]) netMethods
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "poll every (seconds)"
            , H.input [ A.type_ "number", A.value (String.fromInt cfg.interval), A.onInput (InputChange NetInterval) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "or on a cron schedule, which wins over the seconds"
            , H.input [ A.class "mono", A.type_ "text", A.value cfg.cron, A.placeholder "0 9 * * 1-5", A.attribute "aria-label" "cron schedule", A.title "minute hour day-of-month month day-of-week: the poller refuses anything else, and the run row says why", A.spellcheck False, A.onInput (InputChange NetCron) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "in the timezone"
            , H.input [ A.class "mono", A.type_ "text", A.value cfg.timezone, A.placeholder "UTC", A.attribute "aria-label" "cron timezone", A.title "an IANA zone such as America/Chicago; empty is UTC, and a zone needs a cron", A.spellcheck False, A.onInput (InputChange NetTimezone) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "headers"
            , H.textarea [ A.class "mono", A.value cfg.headers, A.placeholder "Name: value\none per line", A.onInput (InputChange NetHeaders) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "body, sent with a POST or a PUT"
            , H.textarea [ A.class "mono", A.value cfg.body, A.placeholder "{\"since\": \"{{cursor}}\", \"key\": \"{{secret:name}}\"}", A.spellcheck False, A.onInput (InputChange NetBody) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "read every page"
            , H.select [ A.value cfg.pageBy, A.onInput (InputChange NetPageBy) ] <|
                List.map (\m -> H.option [ A.value m, A.selected (m == cfg.pageBy) ] [ text (iif (m == "") "no, one request" m) ]) ("" :: pageBy)
            ]
        , iif paging.param
            (H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
                [ text "page parameter"
                , H.input [ A.class "mono", A.type_ "text", A.value cfg.pageParam, A.placeholder "page", A.title "letters, digits, _ . and -, up to 64 of them: the poller refuses anything else", A.onInput (InputChange NetPageParam) ] []
                ]
            )
            (text "")
        , iif paging.path
            (H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
                [ text "cursor path"
                , H.input [ A.class "mono", A.type_ "text", A.value cfg.pagePath, A.placeholder "meta.next", A.title "names joined by dots, at most eight of them: the poller refuses anything else", A.onInput (InputChange NetPagePath) ] []
                ]
            )
            (text "")
        , H.span [ S.fontSizeRem 0.8125, S.color "#666" ] [ text paging.hint ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "rows path"
            , H.input [ A.class "mono", A.type_ "text", A.value cfg.rowsPath, A.placeholder "data", A.title "names joined by dots, at most eight of them: where in the answer the rows sit, for a feed that hands back more than one array", A.onInput (InputChange NetRowsPath) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "keep"
            , H.select [ A.value cfg.mode, A.onInput (InputChange NetMode) ] <|
                List.map (\m -> H.option [ A.value m, A.selected (m == cfg.mode) ] [ text (iif (m == "") "append, every run" m) ]) ("" :: netModes)
            ]
        , iif storing.key
            (H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
                [ text "key"
                , H.input [ A.class "mono", A.type_ "text", A.value cfg.key, A.placeholder "id", A.title "names joined by dots, at most eight of them: the poller refuses anything else, and a row holding nothing there is a failed poll", A.onInput (InputChange NetKey) ] []
                ]
            )
            (text "")
        , H.span [ S.fontSizeRem 0.8125, S.color "#666" ] [ text storing.hint ]
        , H.label [ S.displayFlex, S.gapRem 0.5, S.alignItemsCenter, S.fontSizeRem 0.875 ]
            [ H.input [ A.id "paused", A.type_ "checkbox", A.checked cfg.paused, A.onCheck (\on -> InputChange NetPaused (iif on "1" "")) ] []
            , text "paused: the poller steps over this sheet and writes nothing"
            ]
        , H.div [ S.displayFlex, S.gapRem 0.5, S.alignItemsCenter ]
            [ H.button [ A.class "chip", A.onClick Preflight, A.title "fetch it once, now, and show what comes back" ] [ text "test the request" ]
            , H.button [ A.class "chip", A.onClick RunNow, A.title "poll it now: the row it writes lands in the rows beside this" ] [ text "run now" ]
            , viewNextRun cfg.paused model
            , H.span [ S.fontSizeRem 0.875, S.color "#666" ]
                [ text <|
                    case model.sheet.table of
                        Ok tbl ->
                            String.fromInt (Array.length tbl.rows) ++ " payloads"

                        Err _ ->
                            "no payloads yet"
                ]
            ]
        , viewPreflight model.sheet.preflight
        , viewRun model.sheet.run
        ]


{-| What the request answered: the status, the time and the size on one line,
then the start of the body. A failure is the poller's own sentence about it.
-}
viewPreflight : Maybe (Result String Preview) -> Html Msg
viewPreflight tested =
    case tested of
        Nothing ->
            text ""

        Just (Err error) ->
            H.pre [ A.class "mono preflight", S.color "#b00", S.whiteSpacePreWrap, S.fontSizeRem 0.75 ] [ text error ]

        Just (Ok p) ->
            H.pre [ A.class "mono preflight", S.whiteSpacePreWrap, S.fontSizeRem 0.75 ]
                [ text
                    (String.fromInt p.status
                        ++ " · "
                        ++ String.fromInt p.ms
                        ++ " ms · "
                        ++ String.fromInt p.bytes
                        ++ " bytes · "
                        ++ p.contentType
                        ++ "\n"
                        ++ p.body
                    )
                ]


{-| What the run just now landed, in one line off its own `net` row. A refusal
is the server's own sentence about it.
-}
viewRun : Maybe (Result String String) -> Html Msg
viewRun ran =
    case ran of
        Nothing ->
            text ""

        Just (Err error) ->
            H.pre [ A.class "mono run", S.color "#b00", S.whiteSpacePreWrap, S.fontSizeRem 0.75 ] [ text error ]

        Just (Ok line) ->
            H.pre [ A.class "mono run", S.whiteSpacePreWrap, S.fontSizeRem 0.75 ] [ text line ]


{-| When the poller takes this sheet next, off the freshness the library already
reads. A sheet the due map has not reached yet says nothing rather than naming a
time nobody has decided, and a paused sheet says that instead: its due entry is
left where it was and means nothing until it runs again.
-}
viewNextRun : Bool -> Model -> Html Msg
viewNextRun paused model =
    H.span [ A.class "next-run", S.fontSizeRem 0.875, S.color "#666" ]
        [ text <|
            if paused then
                "paused"

            else
                case model.freshness |> Dict.get model.sheet.id |> Maybe.andThen .nextRun of
                    Nothing ->
                        ""

                    Just at ->
                        "next run " ++ String.left 16 (String.replace "T" " " at)
        ]


viewAlert : Model -> { code : String, to : String, interval : Int, digest : Bool, when : When, paused : Bool, snoozedUntil : String, cron : String, timezone : String } -> Html Msg
viewAlert model cfg =
    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5, S.paddingRem 1, S.minWidth "25vw" ]
        [ H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "watches this query"
            , H.textarea [ A.id "code", A.class "mono", A.rows 8, A.value cfg.code, A.placeholder "select * from @query:budget-burn where burn_ratio > 1.1", A.spellcheck False, A.onInput (InputChange AlertCode) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "email, or a webhook url"
            , H.input [ A.value cfg.to, A.placeholder "you@example.com", A.onInput (InputChange AlertTo) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "check every (seconds)"
            , H.input [ A.type_ "number", A.value (String.fromInt cfg.interval), A.onInput (InputChange NetInterval) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "or on a cron schedule, which wins over the seconds"
            , H.input [ A.class "mono", A.type_ "text", A.value cfg.cron, A.placeholder "0 9 * * 1-5", A.attribute "aria-label" "cron schedule", A.title "minute hour day-of-month month day-of-week: the poller refuses anything else, and the run row says why", A.spellcheck False, A.onInput (InputChange NetCron) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "in the timezone"
            , H.input [ A.class "mono", A.type_ "text", A.value cfg.timezone, A.placeholder "UTC", A.attribute "aria-label" "cron timezone", A.title "an IANA zone such as America/Chicago; empty is UTC, and a zone needs a cron", A.spellcheck False, A.onInput (InputChange NetTimezone) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "fires when"
            , H.select [ A.value (whenSpec cfg.when).name, A.onInput (InputChange AlertWhen) ] <|
                List.map (\w -> H.option [ A.value (whenSpec w).name, A.selected (w == cfg.when) ] [ text (whenSpec w).label ]) whens
            ]
        , H.label [ S.displayFlex, S.gapRem 0.5, S.alignItemsCenter, S.fontSizeRem 0.875 ]
            [ H.input [ A.type_ "checkbox", A.checked cfg.digest, A.onCheck (\on -> InputChange AlertDigest (iif on "1" "")) ] []
            , text "fold into the daily digest, sent to the account email"
            ]
        , H.label [ S.displayFlex, S.gapRem 0.5, S.alignItemsCenter, S.fontSizeRem 0.875 ]
            [ H.input [ A.id "paused", A.type_ "checkbox", A.checked cfg.paused, A.onCheck (\on -> InputChange NetPaused (iif on "1" "")) ] []
            , text "paused: the poller steps over this sheet and writes nothing"
            ]
        , H.div [ S.displayFlex, S.gapRem 0.5, S.alignItemsCenter ]
            [ H.button [ A.class "chip", A.onClick RunNow, A.title "run it now: the row it writes lands in the rows beside this" ] [ text "run now" ]
            , viewNextRun cfg.paused model
            ]

        -- Silence is not pause: the runs go on, the verdicts go on being
        -- recorded, and only the delivery is held. A snooze whose moment has
        -- passed is over, so it is neither drawn nor offered a way out.
        --
        -- Compared as text, because both sides are the UTC ISO stamp the chip
        -- writes and ISO sorts the way the calendar does. A cell the poller
        -- refuses by name is drawn as whatever it says, with the unsnooze that
        -- clears it: the refusal is the check, and this is the way out of it.
        , if cfg.snoozedUntil > isoStamp model.now then
            H.div [ S.displayFlex, S.gapRem 0.5, S.alignItemsCenter, S.fontSizeRem 0.875 ]
                [ H.span [] [ text ("snoozed until " ++ String.left 16 (String.replace "T" " " cfg.snoozedUntil)) ]
                , H.button [ A.class "chip", A.onClick (InputChange AlertSnoozed ""), A.title "send again from the next run on" ] [ text "unsnooze" ]
                ]

          else
            H.button [ A.class "chip", A.onClick AlertSnooze, A.title "keep deciding, keep recording, send nothing for a day" ] [ text "snooze a day" ]
        , viewRun model.sheet.run
        , H.p [ S.fontSizeRem 0.875, S.color "#666" ]
            [ text <|
                case model.sheet.table of
                    Ok tbl ->
                        String.fromInt (Array.length tbl.rows) ++ " runs recorded"

                    Err _ ->
                        "no runs yet"
            ]
        , H.p [ S.fontSizeRem 0.875, S.color "#666" ]
            [ text "Only a run that changes the answer is sent, and every run lands in the rows beside this. A url is posted to rather than mailed: a Slack or Discord webhook gets the shape it reads, and any other url gets the rows." ]
        ]


{-| A tile is the sheet itself, embedded. Nothing here knows how to draw a
chart or a table, because the sheet it names already does -- which is the whole
reason a chart is a sheet.
-}
viewDashboard : List String -> Html Msg
viewDashboard tiles =
    if List.isEmpty tiles then
        H.p [ S.paddingRem 2, S.color "#666" ] [ text "List the sheets to show, one reference per line." ]

    else
        H.div [ S.displayGrid, S.gridTemplateColumns "repeat(auto-fit, minmax(22rem, 1fr))", S.gapRem 0.5, S.paddingRem 0.5 ]
            (tiles
                |> List.map
                    (\tile ->
                        let
                            id =
                                String.dropLeft 1 tile
                        in
                        H.div [ A.class "panel", S.backgroundColor "#fff", S.overflowHidden ]
                            [ H.a [ A.href ("/" ++ id), S.displayBlock, S.paddingRem 0.375, S.fontSizeRem 0.8125, S.borderBottom "1px solid #aaa" ] [ text tile ]

                            -- A dashboard of dashboards nests one embed inside
                            -- another until the browser gives up, and a dashboard
                            -- holding itself never stops. A tile is a sheet with
                            -- rows; this one is not.
                            , if String.startsWith "dashboard:" id then
                                H.p [ S.paddingRem 1, S.fontSizeRem 0.875, S.color "#666" ] [ text "A dashboard cannot be a tile on a dashboard." ]

                              else
                                H.iframe [ A.src ("/" ++ id ++ "?embed=1"), A.title tile, S.width "100%", S.height "20rem", S.border "none" ] []
                            ]
                    )
            )


viewDashboardSettings : List String -> Html Msg
viewDashboardSettings tiles =
    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5, S.paddingRem 1, S.minWidth "25vw" ]
        [ H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "tiles, one sheet reference per line"
            , H.textarea [ A.class "mono", A.rows 10, A.value (String.join "\n" tiles), A.placeholder "@chart:burn-by-department\n@query:budget-burn", A.spellcheck False, A.onInput (InputChange DashboardTiles) ] []
            ]
        , H.p [ S.fontSizeRem 0.875, S.color "#666" ]
            [ text "Each tile is that sheet, embedded, so it shows whatever the sheet shows." ]
        ]


chartSet : Id -> String -> String -> Cmd Msg
chartSet id field value =
    changeDoc { id = id, data = [ { action = "set", path = [ E.int 0, E.string field ], value = E.string value } ] }


viewChartSettings : Model -> Chart_ -> Html Msg
viewChartSettings model cfg =
    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5, S.paddingRem 1, S.minWidth "25vw" ]
        [ H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "reads"
            , H.input [ A.class "mono", A.value cfg.source, A.placeholder "@query:budget-burn", A.onInput (InputChange ChartSource) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "kind"
            , H.select [ A.value (kindSpec cfg.kind).name, A.onInput (InputChange ChartKind) ] <|
                List.map
                    (\k ->
                        H.option [ A.value (kindSpec k).name, A.selected (k == cfg.kind), A.title (kindSpec k).label ]
                            [ text (kindSpec k).name ]
                    )
                    chartKinds
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "across"
            , H.input [ A.class "mono", A.value cfg.x, A.placeholder "month", A.onInput (InputChange ChartX) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "up"
            , H.input [ A.class "mono", A.value cfg.y, A.placeholder "spent", A.onInput (InputChange ChartY) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "and up, on its own scale"
            , H.input [ A.class "mono", A.value cfg.y2, A.placeholder "margin_pct", A.title "a second column, drawn as a dashed line against the axis on the right", A.onInput (InputChange ChartY2) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "split by"
            , H.input [ A.class "mono", A.value cfg.series, A.placeholder "department", A.onInput (InputChange ChartSeries) ] []
            ]
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "mark these days"
            , H.textarea
                [ A.class "mono"
                , A.rows 3
                , A.value (cfg.annotations |> List.map (\( on, label ) -> String.trim (on ++ " " ++ label)) |> String.join "\n")
                , A.placeholder "2026-03-01 price change\n2026-06-14 storm"
                , A.title "one day and its label per line; marks are drawn only where the across column reads as a day"
                , A.spellcheck False
                , A.onInput (InputChange ChartAnnotations)
                ]
                []
            ]
        , H.p [ S.fontSizeRem 0.875, S.color "#666" ]
            [ text <|
                case model.sheet.table of
                    Ok tbl ->
                        let
                            drawn =
                                chartPoints "y" tbl

                            -- What a fold cost, counted rather than left for the
                            -- reader to notice: a chart quietly drawing averages
                            -- of its rows is a chart lying about its rows.
                            folded =
                                List.length (List.concatMap Tuple.second drawn)
                                    - List.length (List.concatMap Tuple.second (chartFold drawn))
                        in
                        String.fromInt (Array.length tbl.rows)
                            ++ " points"
                            ++ iif (folded == 0) "" (", " ++ String.fromInt folded ++ " folded into bucket averages")

                    Err _ ->
                        "no points yet"
            ]
        ]


{-| The colours a chart cycles through, one per series. The first is the colour
every chart drew in before a chart could draw more than one thing, so a chart
with nothing to split by is the picture it always was.
-}
chartColours : List String
chartColours =
    [ "#468", "#c64", "#4a7", "#96c", "#ca3", "#877" ]


{-| The plotted points of one column, grouped into series in the order they
arrive: the query orders by series and then by x, so each series arrives whole
and in the order it is drawn. A chart with no series column is one series with
no name.

The column is an argument because a chart may carry two of them: `"y"` is the
scale on the left and `"y2"` the one on the right, read off the same rows by the
same rule, so the two scales cannot disagree about which row is which.

A row whose y does not read as a number is dropped: a chart cannot draw "n/a",
and pretending it is zero would be a lie about the shape.

-}
chartPoints : String -> Table -> List ( String, List ( String, Float ) )
chartPoints up tbl =
    tbl.rows
        |> Array.toList
        |> List.filterMap
            (\row ->
                -- The lenient `string` decoder, not D.string: chartSql casts both
                -- columns as the sheet holds them, so an int id arrives as a JSON
                -- number. Read as a blank, every row of a numeric x column shares
                -- the one label and the chart stacks on a single point, and every
                -- value of a numeric series column folds into the one unnamed
                -- series.
                Maybe.map2
                    (\x y ->
                        ( Dict.get "series" row
                            |> Maybe.map (D.decodeValue string >> Result.withDefault "")
                            |> Maybe.withDefault ""
                        , ( x, y )
                        )
                    )
                    (Dict.get "x" row |> Maybe.map (D.decodeValue string >> Result.withDefault ""))
                    (Dict.get up row |> Maybe.andThen (D.decodeValue number >> Result.toMaybe))
            )
        |> List.foldl
            (\( name, point ) acc ->
                if List.any (\( n, _ ) -> n == name) acc then
                    List.map (\( n, ps ) -> iif (n == name) ( n, point :: ps ) ( n, ps )) acc

                else
                    acc ++ [ ( name, [ point ] ) ]
            )
            []
        |> List.map (\( name, ps ) -> ( name, List.reverse ps ))


{-| The five numbers a box is drawn from, per x, in the order the rows arrive.

`chartSql`'s box branch already grouped them: one row per x carrying `lo`, `q1`,
`med`, `q3` and `hi`, which is why this reads a row rather than a column of
rows. A row missing any of the five, or holding one that is not a number, is
dropped whole -- a box with no whisker is not a box, and drawing four of the five
would be a picture of a spread nobody computed.

-}
chartBoxes : Table -> List ( String, { lo : Float, q1 : Float, med : Float, q3 : Float, hi : Float } )
chartBoxes tbl =
    let
        at key row =
            Dict.get key row |> Maybe.andThen (D.decodeValue number >> Result.toMaybe)
    in
    tbl.rows
        |> Array.toList
        |> List.filterMap
            (\row ->
                Maybe.map2 Tuple.pair
                    -- The same lenient decoder the points read their label with:
                    -- a box over an int column arrives as a number.
                    (Dict.get "x" row |> Maybe.map (D.decodeValue string >> Result.withDefault ""))
                    (Maybe.map5 (\lo q1 med q3 hi -> { lo = lo, q1 = q1, med = med, q3 = q3, hi = hi })
                        (at "lo" row)
                        (at "q1" row)
                        (at "med" row)
                        (at "q3" row)
                        (at "hi" row)
                    )
            )


{-| The `@…` the cursor is sitting in, or nothing. A space or a newline after the
`@` is a ref that has ended, so there is nothing left to complete.
-}
completionTrigger : String -> Maybe String
completionTrigger textBeforeCursor =
    String.indices "@" textBeforeCursor
        |> List.reverse
        |> List.head
        |> Maybe.map (\at -> String.dropLeft at textBeforeCursor)
        |> Maybe.andThen (\t -> iif (String.contains " " t || String.contains "\n" t) Nothing (Just t))


{-| The sheet a trigger is asking about the columns of, which is what a dot after
the ref says: `@table:x.co` is `co` against the columns of `table:x`. No dot is
somebody still naming the sheet, and there is nothing to ask.

Only the two prefixes a query may reference at all, which is what the resolver
refuses anything else for: the answer is a `describe` run through the page's own
engine, and half a ref somebody is still typing is not a question worth asking
it.

-}
completionRef : String -> Maybe String
completionRef trigger =
    case String.split "." (String.dropLeft 1 trigger) of
        ref :: _ :: _ ->
            iif (String.startsWith "table:" ref || String.startsWith "query:" ref) (Just ref) Nothing

        _ ->
            Nothing


{-| What the editor offers for what has been typed: the columns of the named
sheet once there is a dot, and the sheets themselves before there is one.

It may answer nothing, and a `QueryAutocomplete` holding nothing draws nothing --
which is what lets the list be recomputed when a sheet's columns arrive, rather
than held back until the next keystroke rebuilds it.

-}
completionAt : Library -> Dict String (List String) -> String -> QueryAutocomplete
completionAt shelf known trigger =
    let
        matching typed =
            List.filter (String.toLower >> String.contains (String.toLower typed))
    in
    { trigger = trigger
    , selectedIndex = 0
    , suggestions =
        case String.split "." (String.dropLeft 1 trigger) of
            ref :: rest ->
                if List.isEmpty rest then
                    shelf
                        |> Dict.keys
                        |> List.filter (\k -> String.startsWith "table:" k || String.startsWith "query:" k)
                        |> matching ref
                        |> List.take 8

                else
                    Dict.get ref known
                        |> Maybe.withDefault []
                        |> matching (String.join "." rest)
                        |> List.take 8
                        |> List.map (\col -> ref ++ "." ++ col)

            [] ->
                []
    }


{-| One line of the annotations box: a day, whitespace, and whatever is left over
as the label. Nothing before the whitespace is no annotation, so a blank line and
a stray tab both fall out rather than marking day "".

The day itself is not checked here. `viewChart` places a mark only where
`parseDay` reads it, so a half-typed date is a mark nobody draws rather than a
line the editor refuses to keep -- the box writes the document on every keystroke,
the way the dashboard's tiles do.

-}
parseAnnotation : String -> Maybe ( String, String )
parseAnnotation line =
    case String.words line of
        -- A line of nothing but spaces answers one empty word rather than no
        -- words, so the day is checked and not the list: a blank line was
        -- marking the day "", which parseDay reads as no day and viewChart then
        -- draws nowhere -- a mark in the document that nothing on screen says.
        at :: rest ->
            iif (String.isEmpty at) Nothing (Just ( at, String.join " " rest ))

        [] ->
            Nothing


{-| The most points one series is drawn with. The plot is 720 units wide, so
past one point every other unit a line is drawing over itself and the browser is
holding elements nobody can see.
-}
chartPointsMax : Int
chartPointsMax =
    360


{-| A series past `chartPointsMax` points, averaged down to that many: the
points are cut into equal buckets in day order, and each bucket is the mean of
its y values at the earliest x it holds.

Only a chart whose every x is a day folds. An ordinal axis is one place per
distinct label, so a bucket's first label would be drawn where the labels it
swallowed are still drawn by the other series -- on a day axis a bucket's first
day is a place of its own.

-}
chartFold : List ( String, List ( String, Float ) ) -> List ( String, List ( String, Float ) )
chartFold series =
    if not (List.all (\( _, ps ) -> List.all (\( x, _ ) -> parseDay x /= Nothing) ps) series) then
        series

    else
        series
            |> List.map
                (\( name, ps ) ->
                    let
                        size =
                            (List.length ps + chartPointsMax - 1) // chartPointsMax
                    in
                    if List.length ps <= chartPointsMax then
                        ( name, ps )

                    else
                        ( name
                        , ps
                            -- Sorted by day rather than left in arrival order,
                            -- the way chartRuns places them: a bucket is a
                            -- stretch of time, and the query's `order by x` is a
                            -- string order, which a day column with inconsistent
                            -- zero-padding ("2024-1-5" before "2024-01-10")
                            -- still parses as days but does not sort as days.
                            -- The guard above read every x, so no point falls
                            -- back to day 0.
                            |> List.sortBy (\( x, _ ) -> Maybe.withDefault 0 (parseDay x))
                            |> List.indexedMap (\i point -> ( i // size, point ))
                            |> List.foldr (\( b, point ) acc -> Dict.insert b (point :: Maybe.withDefault [] (Dict.get b acc)) acc) Dict.empty
                            -- Dict.values is in key order, so the buckets come
                            -- back in the order the days did; foldr is what
                            -- leaves each one holding its own points that way
                            -- too, earliest day first.
                            |> Dict.values
                            |> List.map
                                (\held ->
                                    -- A bucket holds the point that made it, so
                                    -- the blank is never a label.
                                    ( held |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault ""
                                    , List.sum (List.map Tuple.second held) / toFloat (List.length held)
                                    )
                                )
                        )
                )


{-| The earliest and the latest day the chart holds, or `Nothing` when any x of
any series is not a day.

Lifted out of `chartRuns` because an annotation is placed against the same span
and by the same rule, and a mark half a pixel off the point it is about is a
picture that argues with itself. It is also the one question "is this axis time?"
is asked as.

-}
chartSpan : List ( String, List ( String, Float ) ) -> Maybe ( Int, Int )
chartSpan series =
    let
        points =
            List.concatMap Tuple.second series

        days =
            List.filterMap (Tuple.first >> parseDay) points
    in
    if List.isEmpty points || List.length days /= List.length points then
        Nothing

    else
        -- The minimum and the maximum of a list this branch already knows is not
        -- empty.
        Just ( Maybe.withDefault 0 (List.minimum days), Maybe.withDefault 0 (List.maximum days) )


{-| Where one day sits across the plot. One day, or many days all the same: there
is no span to sit in, so it sits where a lone point always sat.

A day outside the span is not clamped -- an annotation may name one, and a mark
dragged to the edge would say the release happened on a day it did not. The
caller is what decides whether to draw it.

-}
chartAt : ( Int, Int ) -> Int -> Float
chartAt ( first, last ) day =
    iif (last == first) 400 (60 + (toFloat (day - first) / toFloat (last - first)) * 720)


{-| Every series at its place across the plot, split into the runs that are
drawn as one unbroken line. `Nothing` when any x of any series is not a day:
the axis is then one place per distinct label, the way it always was.

On a day axis the ends are the earliest and the latest day any series holds, a
point sits at its own day between them, and a step wider than twice that
series' median step is a gap in the data rather than a stretch of line nobody
measured, so the run ends there and the next one starts after it.

-}
chartRuns : List ( String, List ( String, Float ) ) -> Maybe (List ( String, List (List ( Float, Float )) ))
chartRuns series =
    chartSpan series
        |> Maybe.map
            (\span ->
                let
                    plot =
                        chartAt span
                in
                List.map
                    (\( name, ps ) ->
                        let
                            -- Sorted by day and not left in arrival order: the query
                            -- orders by x, but that is a string order, and a day
                            -- column with inconsistent zero-padding ("2024-1-5"
                            -- before "2024-01-10") still parses as a day while
                            -- sorting wrong. Steps and runs mean the gaps between
                            -- neighbouring days, and a day axis has no other
                            -- neighbour to mean.
                            dayed =
                                List.filterMap (\( x, v ) -> Maybe.map (\day -> ( day, v )) (parseDay x)) ps
                                    |> List.sortBy Tuple.first

                            steps =
                                List.map2 (\( a, _ ) ( b, _ ) -> b - a) dayed (List.drop 1 dayed)

                            sorted =
                                List.sort steps

                            -- The two middle steps averaged, which is one step when
                            -- there is an odd number of them. No steps is a series
                            -- of one point, and nothing to break.
                            median =
                                case ( List.drop ((List.length sorted - 1) // 2) sorted, List.drop (List.length sorted // 2) sorted ) of
                                    ( lo :: _, hi :: _ ) ->
                                        toFloat (lo + hi) / 2

                                    _ ->
                                        0

                            ( _, open, closed ) =
                                List.foldl
                                    (\( day, v ) ( previous, current, done ) ->
                                        case previous of
                                            Just was ->
                                                if toFloat (day - was) > 2 * median then
                                                    ( Just day, [ ( plot day, v ) ], List.reverse current :: done )

                                                else
                                                    ( Just day, ( plot day, v ) :: current, done )

                                            Nothing ->
                                                ( Just day, [ ( plot day, v ) ], done )
                                    )
                                    ( Nothing, [], [] )
                                    dayed
                        in
                        ( name, (List.reverse open :: closed) |> List.filter (not << List.isEmpty) |> List.reverse )
                    )
                    series
            )


{-| The most series a legend names before it says how many more there are.
`chartColours` cycles at six, so past a dozen the swatches have stopped telling
the series apart and the list is a wall of text over the picture.
-}
legendMax : Int
legendMax =
    12


{-| Where each legend entry sits, and how many rows it took. Entries flow left to
right and wrap at the width of the plot, because a chart with eight series used
to spread them across the one viewBox until the labels sat on top of each other
and none of them could be read.

The width of an entry is estimated from its length rather than measured: SVG
cannot be asked how wide a string will be before it is drawn, and a legend laid
out a little generously reads correctly while a measured one costs a second
layout pass on every keystroke.

-}
legendLayout : List String -> ( List ( Float, Float ), Int )
legendLayout names =
    let
        step ( x, y, rows ) name =
            let
                w =
                    legendItemWidth name
            in
            -- Wrap before the entry rather than after it, so the entry that
            -- would have overhung starts the next row instead.
            if x > 60 && x + w > 790 then
                ( 60 + w, y + 14, rows + 1 )

            else
                ( x + w, y, rows )
    in
    names
        |> List.foldl
            (\name ( placed, at ) ->
                let
                    ( x, y, rows ) =
                        step at name

                    ( wasX, wasY, _ ) =
                        at
                in
                ( placed ++ [ iif (y == wasY) ( wasX, wasY ) ( 60, y ) ], ( x, y, rows ) )
            )
            ( [], ( 60, 8, 1 ) )
        |> (\( placed, ( _, _, rows ) ) -> ( placed, iif (List.isEmpty names) 0 rows ))


{-| What one entry of a two-scale legend is called: the column on its own when
there is nothing to split by, and the series and the column together when there
is.
-}
legendName : String -> String -> String
legendName series column =
    iif (series == "") column (series ++ " · " ++ column)


{-| How wide one legend entry is drawn: the swatch, the gap, and the label at
roughly the width of the 12px font's average glyph.
-}
legendItemWidth : String -> Float
legendItemWidth name =
    28 + 6.6 * toFloat (String.length name)


viewChart : Chart_ -> Table -> Html Msg
viewChart cfg tbl =
    let
        series =
            chartFold (chartPoints "y" tbl)

        points =
            List.concatMap Tuple.second series

        -- The second scale is read off the same rows by the same rule and kept
        -- apart from the first all the way down: its own extent, its own plotY
        -- and its own labels. A y2 folded into `series` would be averaged into
        -- the first scale's extent, which is the whole reason a chart has two.
        second =
            chartFold (chartPoints "y2" tbl)

        -- A box carries five numbers per x, which no point can hold, so it is
        -- read whole rather than through `chartPoints`. Only a box asks: every
        -- other kind's source has no such columns and reading them would be a
        -- table scan for nothing.
        boxes =
            iif (cfg.kind == Box) (chartBoxes tbl) []

        timed =
            chartRuns series

        -- The axis is the x labels and not the rows: two series are drawn
        -- against the same one, and a bar stacks what shares a label. A box
        -- names its own labels, one per group its query already made.
        xs =
            let
                arrived =
                    iif (cfg.kind == Box) (List.map Tuple.first boxes) (List.map Tuple.first points)
                        |> List.foldl
                            (\x ( seen, out ) ->
                                iif (Set.member x seen) ( seen, out ) ( Set.insert x seen, x :: out )
                            )
                            ( Set.empty, [] )
                        |> Tuple.second
                        |> List.reverse
            in
            case timed of
                -- A day axis is read in day order, its two end labels and its
                -- bars included: the query orders by the series first, so every
                -- day of a second series arrives after every day of the first,
                -- and the label that arrived last is not the one at the
                -- right-hand end. `Just` is chartRuns saying it read every x as
                -- a day, so nothing falls back to day 0.
                Just _ ->
                    List.sortBy (\x -> Maybe.withDefault 0 (parseDay x)) arrived

                Nothing ->
                    arrived

        xAt =
            xs |> List.indexedMap (\i x -> ( x, i )) |> Dict.fromList

        -- Every point at its place on that axis. The lookup cannot miss: xAt is
        -- built out of these same points.
        place ss =
            ss
                |> List.map
                    (\( name, ps ) ->
                        ( name
                        , ps |> List.filterMap (\( x, v ) -> Dict.get x xAt |> Maybe.map (\i -> ( i, v )))
                        )
                    )

        placed =
            place series

        -- A bar chart stacks its series, so its axis spans the sums on each
        -- label; every other kind draws each series over the others. A box
        -- spans its whiskers, which is the widest of the five and the only pair
        -- that must be inside the picture.
        stacked =
            points
                |> List.foldl (\( x, v ) acc -> Dict.insert x (v + Maybe.withDefault 0 (Dict.get x acc)) acc) Dict.empty
                |> Dict.values

        heights =
            case cfg.kind of
                Bar ->
                    stacked

                Box ->
                    List.concatMap (\( _, b ) -> [ b.lo, b.hi ]) boxes

                _ ->
                    List.map Tuple.second points

        -- The baseline is zero unless the data goes below it, because a bar
        -- chart that does not start at zero misstates every comparison on it.
        extent vs =
            let
                top_ =
                    Maybe.withDefault 1 (List.maximum vs) |> max 0

                bottom_ =
                    Maybe.withDefault 0 (List.minimum vs) |> min 0
            in
            ( top_, bottom_, iif (top_ - bottom_ == 0) 1 (top_ - bottom_) )

        ( top, bottom, span ) =
            extent heights

        ( top2, bottom2, span2 ) =
            extent (List.map Tuple.second (List.concatMap Tuple.second second))

        n =
            List.length xs

        -- The legend is drawn above the plot, so the plot starts under whatever
        -- the legend took. One row -- or none -- leaves the 240 units every
        -- chart has always been drawn in, unchanged.
        legendNames =
            if cfg.y2 == "" then
                List.map Tuple.first series

            else
                -- Two scales, so every entry names its column: with nothing to
                -- split by the two lines are the two column names, and with a
                -- series they are "north · margin" and "north · margin_pct" --
                -- the same series appears on both scales, and naming it twice
                -- and identically is a legend that says nothing about which
                -- swatch is which.
                List.map (\( name, _ ) -> legendName name cfg.y) series
                    ++ List.map (\( name, _ ) -> legendName name cfg.y2) second

        -- Bounded, and it says what it did not name. Nothing caps how many
        -- distinct values a series column holds, and every entry now takes a row
        -- of its own to wrap into -- so without this a sheet with a thousand
        -- series pushed the top of the plot below its baseline and drew the
        -- whole chart upside down. `chartColours` cycles at six anyway, so past
        -- a dozen the swatches have stopped telling the series apart.
        legendShown =
            iif (List.length legendNames > legendMax)
                (List.take legendMax legendNames
                    ++ [ "+" ++ String.fromInt (List.length legendNames - legendMax) ++ " more" ]
                )
                legendNames

        ( legendSpots, legendRows ) =
            iif (List.all String.isEmpty legendNames) ( [], 0 ) (legendLayout legendShown)

        -- Clamped as well as bounded: one name long enough to take a row of its
        -- own, a dozen times over, is still a plot and not an inverted one.
        plotTop =
            min 140 (20 + 14 * toFloat (max 0 (legendRows - 1)))

        plotY v =
            260 - ((v - bottom) / span) * (260 - plotTop)

        plotY2 v =
            260 - ((v - bottom2) / span2) * (260 - plotTop)

        plotX i =
            iif (n < 2) 400 (60 + (toFloat i / toFloat (n - 1)) * 720)

        -- Where a bar and a box sit: one slot per label, the slot's left edge
        -- and its width, which is the ordinal placement neither can share with
        -- plotX.
        slot i =
            ( 60 + (toFloat i / toFloat (max 1 n)) * 720, iif (n == 0) 10 (720 / toFloat n * 0.7) )

        num v =
            String.fromFloat (round2 v)

        at px v =
            String.fromFloat px ++ "," ++ String.fromFloat (plotY v)

        -- Where the line, the area and the dots are drawn: a run of points that
        -- belong on one unbroken line. A day axis says where each point sits and
        -- where the data has a hole; every other axis is one place per label, in
        -- the order the labels arrive, and a series is the one run it always was.
        runs =
            case timed of
                Just byDay ->
                    byDay

                Nothing ->
                    placed |> List.map (\( name, ps ) -> ( name, [ List.map (\( i, v ) -> ( plotX i, v )) ps ] ))

        -- The second scale's runs, placed the same way but never folded into the
        -- first: they are drawn through plotY2.
        runs2 =
            case chartRuns second of
                Just byDay ->
                    byDay

                Nothing ->
                    place second |> List.map (\( name, ps ) -> ( name, [ List.map (\( i, v ) -> ( plotX i, v )) ps ] ))

        -- chartColours is never empty, and modBy keeps the index inside it. The
        -- second scale carries on from where the first left off, so the legend
        -- index and the colour on screen are the same number.
        colourAt j =
            chartColours |> List.drop (modBy (List.length chartColours) j) |> List.head |> Maybe.withDefault "#468"

        path ps =
            ps |> List.map (\( px, v ) -> at px v) |> String.join " "

        line =
            runs
                |> List.indexedMap
                    (\j ( _, rs ) ->
                        rs
                            |> List.map
                                (\run ->
                                    Svg.polyline [ SvgA.fill "none", SvgA.stroke (colourAt j), SvgA.strokeWidth "2", SvgA.points (path run) ] []
                                )
                    )
                |> List.concat

        -- The second y, always as a line whatever the kind. One rule rather than
        -- one per kind: a bar chart with a second scale is the bars with the
        -- line everybody draws that as, and a reader never has to ask which of
        -- the two shapes on screen belongs to which axis.
        secondLine =
            runs2
                |> List.indexedMap
                    (\j ( _, rs ) ->
                        rs
                            |> List.map
                                (\run ->
                                    Svg.polyline
                                        [ SvgA.fill "none"
                                        , SvgA.stroke (colourAt (List.length series + j))
                                        , SvgA.strokeWidth "2"
                                        , SvgA.strokeDasharray "6 3"
                                        , SvgA.points (run |> List.map (\( px, v ) -> String.fromFloat px ++ "," ++ String.fromFloat (plotY2 v)) |> String.join " ")
                                        ]
                                        []
                                )
                    )
                |> List.concat

        -- Every fill is drawn first and the lines over them, so a value at the
        -- baseline still shows its stroke instead of being covered by its own
        -- shading, and one series' shading cannot cover the series under it.
        area =
            List.append
                (runs
                    |> List.indexedMap
                        (\j ( _, rs ) ->
                            rs
                                |> List.map
                                    (\run ->
                                        let
                                            -- A run holds at least one point:
                                            -- chartRuns drops the empty ones and
                                            -- chartPoints makes a series out of a
                                            -- point.
                                            first =
                                                run |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0

                                            last =
                                                run |> List.reverse |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0
                                        in
                                        Svg.polygon
                                            [ SvgA.fill (colourAt j)
                                            , SvgA.fillOpacity "0.25"
                                            , SvgA.stroke "none"
                                            , SvgA.points (String.join " " [ at first bottom, path run, at last bottom ])
                                            ]
                                            []
                                    )
                        )
                    |> List.concat
                )
                line

        -- One rect per point, the series stacked per label in the order the
        -- legend reads: each one starts where the ones before it left off, which
        -- is what makes the top of the stack the sum. Bars are ordinal even on a
        -- day axis, which every other kind reads as time: one bar per day across
        -- a sparse year is a picture of hairlines nobody can read or click.
        bars =
            placed
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( j, ( _, ps ) ) base ->
                        List.foldl
                            (\( i, v ) ( offsets, drawn ) ->
                                let
                                    from =
                                        Maybe.withDefault 0 (Dict.get i offsets)

                                    ( x, w ) =
                                        slot i
                                in
                                ( Dict.insert i (from + v) offsets
                                , Svg.rect
                                    [ SvgA.x (String.fromFloat x)
                                    , SvgA.y (String.fromFloat (min (plotY from) (plotY (from + v))))
                                    , SvgA.width (String.fromFloat w)
                                    , SvgA.height (String.fromFloat (abs (plotY (from + v) - plotY from)))
                                    , SvgA.fill (colourAt j)
                                    ]
                                    []
                                    :: drawn
                                )
                            )
                            base
                            ps
                    )
                    ( Dict.empty, [] )
                |> Tuple.second
                |> List.reverse

        -- A box and its whiskers per label: the quartile rect, the median across
        -- it, and a whisker to each extreme with a cap on it. Ordinal, the way
        -- bars are and for the same reason.
        boxMarks =
            boxes
                |> List.filterMap (\( label, b ) -> Dict.get label xAt |> Maybe.map (\i -> ( i, b )))
                |> List.map
                    (\( i, b ) ->
                        let
                            ( x, w ) =
                                slot i

                            mid =
                                x + w / 2

                            rule y1 y2 x1 x2 =
                                Svg.line
                                    [ SvgA.x1 (String.fromFloat x1)
                                    , SvgA.x2 (String.fromFloat x2)
                                    , SvgA.y1 (String.fromFloat y1)
                                    , SvgA.y2 (String.fromFloat y2)
                                    , SvgA.stroke (colourAt 0)
                                    , SvgA.strokeWidth "2"
                                    ]
                                    []
                        in
                        Svg.g []
                            [ rule (plotY b.hi) (plotY b.lo) mid mid
                            , rule (plotY b.hi) (plotY b.hi) (mid - w / 4) (mid + w / 4)
                            , rule (plotY b.lo) (plotY b.lo) (mid - w / 4) (mid + w / 4)
                            , Svg.rect
                                [ SvgA.x (String.fromFloat x)
                                , SvgA.y (String.fromFloat (min (plotY b.q3) (plotY b.q1)))
                                , SvgA.width (String.fromFloat w)
                                , SvgA.height (String.fromFloat (abs (plotY b.q1 - plotY b.q3)))
                                , SvgA.fill (colourAt 0)
                                , SvgA.fillOpacity "0.25"
                                , SvgA.stroke (colourAt 0)
                                ]
                                []
                            , rule (plotY b.med) (plotY b.med) x (x + w)
                            ]
                    )

        dots =
            runs
                |> List.indexedMap
                    (\j ( _, rs ) ->
                        List.concat rs
                            |> List.map
                                (\( px, v ) ->
                                    Svg.circle
                                        [ SvgA.cx (String.fromFloat px)
                                        , SvgA.cy (String.fromFloat (plotY v))
                                        , SvgA.r "4"
                                        , SvgA.fill (colourAt j)
                                        ]
                                        []
                                )
                    )
                |> List.concat

        -- A moment marked on the axis. Only where the axis is time, and only for
        -- an `at` that reads as a day: everything else is drawn nowhere rather
        -- than at the left edge, which is where a placement that fell back to
        -- zero would have put a release nobody dated. The placement is the
        -- span's and not a lookup, so a day the data itself does not hold still
        -- lands between the days that surround it.
        marks =
            case chartSpan series of
                Nothing ->
                    []

                Just span_ ->
                    cfg.annotations
                        |> List.filterMap (\( on, label ) -> parseDay on |> Maybe.map (\day -> ( chartAt span_ day, label )))
                        |> List.map
                            (\( px, label ) ->
                                Svg.g []
                                    [ Svg.line
                                        [ SvgA.x1 (String.fromFloat px)
                                        , SvgA.x2 (String.fromFloat px)
                                        , SvgA.y1 (String.fromFloat plotTop)
                                        , SvgA.y2 (String.fromFloat (plotY bottom))
                                        , SvgA.stroke "#999"
                                        , SvgA.strokeDasharray "4 3"
                                        ]
                                        []
                                    , Svg.text_
                                        [ SvgA.x (String.fromFloat (px + 3))
                                        , SvgA.y (String.fromFloat (plotTop + 10))
                                        , SvgA.fontSize "11"
                                        , SvgA.fill "#666"
                                        ]
                                        [ Svg.text label ]
                                    ]
                            )

        -- A chart that splits its rows says which colour is which, in the same
        -- viewBox, because downloadChart clones the one svg. A chart with
        -- nothing to split by and one scale has nothing to name and draws no
        -- legend at all.
        legend =
            List.map2
                (\( lx, ly ) ( j, name ) ->
                    Svg.g []
                        [ Svg.rect [ SvgA.x (String.fromFloat lx), SvgA.y (String.fromFloat (ly - 9)), SvgA.width "10", SvgA.height "10", SvgA.fill (colourAt j) ] []
                        , Svg.text_ [ SvgA.x (String.fromFloat (lx + 14)), SvgA.y (String.fromFloat ly), SvgA.fontSize "12", SvgA.fill "#666" ] [ Svg.text name ]
                        ]
                )
                legendSpots
                (List.indexedMap Tuple.pair legendShown)

        -- Every kind but the tile shares an axis, the two end labels and the
        -- legend, so the marks are the only thing each one of them decides. A
        -- second scale puts its own two labels at the right-hand end, which is
        -- the side the reader reads it from.
        plotted drawn =
            List.concat
                [ [ Svg.line [ SvgA.x1 "60", SvgA.y1 (String.fromFloat (plotY bottom)), SvgA.x2 "790", SvgA.y2 (String.fromFloat (plotY bottom)), SvgA.stroke "#aaa" ] []
                  , Svg.text_ [ SvgA.x "4", SvgA.y (String.fromFloat (plotTop + 4)), SvgA.fontSize "12", SvgA.fill "#666" ] [ Svg.text (num top) ]
                  , Svg.text_ [ SvgA.x "4", SvgA.y (String.fromFloat (plotY bottom)), SvgA.fontSize "12", SvgA.fill "#666" ] [ Svg.text (num bottom) ]
                  ]
                , iif (List.isEmpty second)
                    []
                    [ Svg.text_ [ SvgA.x "796", SvgA.y (String.fromFloat (plotTop + 4)), SvgA.fontSize "12", SvgA.fill "#666", SvgA.textAnchor "end" ] [ Svg.text (num top2) ]
                    , Svg.text_ [ SvgA.x "796", SvgA.y (String.fromFloat (plotY2 bottom2)), SvgA.fontSize "12", SvgA.fill "#666", SvgA.textAnchor "end" ] [ Svg.text (num bottom2) ]
                    ]
                , marks
                , drawn
                , secondLine
                , -- Only the ends are labelled: every tick would collide, and the
                  -- rows underneath are one click away in the source sheet.
                  [ Svg.text_ [ SvgA.x "60", SvgA.y "290", SvgA.fontSize "12", SvgA.fill "#666" ] [ Svg.text (xs |> List.head |> Maybe.withDefault "") ]
                  , Svg.text_ [ SvgA.x "790", SvgA.y "290", SvgA.fontSize "12", SvgA.fill "#666", SvgA.textAnchor "end" ] [ Svg.text (xs |> List.reverse |> List.head |> Maybe.withDefault "") ]
                  ]
                , legend
                ]

        -- The tile is the first series and says which one it is. The last point
        -- is the number, the one before it is what it is compared against, and
        -- that series is the sparkline beside them.
        tilePoints =
            series |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault []

        tileName =
            series |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault ""

        latest =
            tilePoints |> List.reverse |> List.head

        previous =
            tilePoints |> List.reverse |> List.drop 1 |> List.head

        -- Its own scale rather than plotX/plotY: the sparkline lives in the
        -- right third of the same viewBox, and the value has the rest.
        sparkAt i v =
            String.fromFloat (iif (List.length tilePoints < 2) 600 (430 + (toFloat i / toFloat (List.length tilePoints - 1)) * 350))
                ++ ","
                ++ String.fromFloat (230 - ((v - bottom) / span) * 160)

        tile =
            case latest of
                Nothing ->
                    []

                Just ( label, v ) ->
                    List.concat
                        [ [ Svg.text_ [ SvgA.x "40", SvgA.y "150", SvgA.fontSize "92", SvgA.fill "#1a1a1a" ] [ Svg.text (num v) ]
                          , Svg.text_ [ SvgA.x "40", SvgA.y "230", SvgA.fontSize "20", SvgA.fill "#666" ]
                                [ Svg.text (label ++ iif (tileName == "") "" (" · " ++ tileName)) ]
                          ]
                        , -- No arrow colour: whether a rise is good is the
                          -- reader's question, not this chart's, and a green
                          -- number would answer it for them.
                          case previous of
                            Just ( was, before ) ->
                                [ Svg.text_ [ SvgA.x "40", SvgA.y "200", SvgA.fontSize "26", SvgA.fill "#468" ]
                                    [ Svg.text (iif (v < before) "▼ " "▲ " ++ num (abs (v - before)) ++ " since " ++ was) ]
                                ]

                            Nothing ->
                                []
                        , [ Svg.polyline
                                [ SvgA.fill "none"
                                , SvgA.stroke "#468"
                                , SvgA.strokeWidth "2"
                                , SvgA.points (tilePoints |> List.indexedMap (\i ( _, v_ ) -> sparkAt i v_) |> String.join " ")
                                ]
                                []
                          ]
                        ]
    in
    if List.isEmpty points && List.isEmpty boxes then
        H.p [ S.paddingRem 2, S.color "#666" ]
            [ text (iif (String.isEmpty cfg.source) "Set what this chart reads, and the two columns to draw." "No points to draw: check that the y column holds numbers.") ]

    else
        H.div [ S.paddingRem 1, S.backgroundColor "#fff" ]
            [ -- One svg for every kind, the tile included: downloadChart in
              -- src/index.html clones `main svg` to make the PNG, so a kind that
              -- drew itself in HTML would be the one that cannot be exported.
              Svg.svg [ SvgA.viewBox "0 0 800 300", SvgA.width "100%", SvgA.height "300" ] <|
                case cfg.kind of
                    Line ->
                        plotted line

                    Bar ->
                        plotted bars

                    Area ->
                        plotted area

                    Scatter ->
                        plotted dots

                    Box ->
                        plotted boxMarks

                    Kpi ->
                        tile
            ]


viewNetSocket : Model -> { url : String } -> Html Msg
viewNetSocket model cfg =
    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.5, S.paddingRem 1, S.minWidth "25vw" ]
        [ viewNetWarning model
        , H.label [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25, S.fontSizeRem 0.875 ]
            [ text "URL"
            , H.input [ A.type_ "text", A.value cfg.url, A.onInput (InputChange NetUrl) ] []
            ]
        , H.p [ S.fontSizeRem 0.875, S.color "#666" ]
            [ text (Maybe.withDefault "—" model.sheet.netStatus) ]
        ]


viewQueryEditor : Model -> Query_ -> Html Msg
viewQueryEditor model query =
    let
        sheet =
            model.sheet

        lineCount =
            max 1 (List.length (String.lines (String.trim query.code)))

        sheetRefs =
            model.library |> Dict.keys |> List.filter (\k -> String.startsWith "table:" k || String.startsWith "query:" k) |> List.take 5
    in
    H.div [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%", S.minWidth "25vw" ]
        [ H.div [ S.displayFlex, S.flexGrow "1", S.minHeightRem 8, S.overflowAuto, S.backgroundColor "#f8f8f8" ]
            [ H.div [ A.class "lines" ]
                (List.range 1 lineCount |> List.map (\n -> H.div [] [ text (String.fromInt n) ]))
            , H.div [ S.positionRelative, S.flexGrow "1", S.height "100%" ]
                [ H.textarea [ A.id "code", A.onInput (InputChange QueryCode), A.spellcheck False, S.height "100%", S.width "100%" ]
                    [ text (String.trim query.code) ]
                , case sheet.queryAutocomplete of
                    Nothing ->
                        text ""

                    -- Nothing to suggest draws no dropdown: the list is held
                    -- open while a sheet's columns are on their way, and an
                    -- empty panel over the editor is worse than none. The div is
                    -- named, because src/index.html's keydown handler has to
                    -- find it to know whether the arrow keys belong to the
                    -- editor or to this list; it used to look for
                    -- [style*="z-index: 100"], which is also the column filter
                    -- panel.
                    Just ac ->
                        if List.isEmpty ac.suggestions then
                            text ""

                        else
                            H.div [ A.id "complete", A.class "panel mono", S.positionAbsolute, S.top "2rem", S.left "0.5rem", S.zIndex "100", S.maxHeightRem 12, S.overflowYAuto, S.minWidthRem 15, S.fontSizeRem 0.8125 ]
                                (ac.suggestions
                                    |> List.indexedMap
                                        (\i ref ->
                                            H.div [ A.onClick (AutocompleteSelect ref), S.padding "0.5rem 0.75rem", S.cursorPointer, S.backgroundColor (iif (i == ac.selectedIndex) "#dce7f7" "transparent"), S.borderBottom "1px solid #eee" ]
                                                [ H.span [ S.color "#666" ] [ text "@" ], text ref ]
                                        )
                                )
                ]
            ]
        , case model.error of
            "" ->
                text ""

            err ->
                H.div [ A.class "mono", S.backgroundColor "#fee", S.borderTop "2px solid #c66", S.padding "0.75rem", S.fontSizeRem 0.8125, S.whiteSpacePre, S.overflowXAuto, S.maxHeightRem 8, S.overflowYAuto ]
                    [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsStart ]
                        [ H.span [ S.color "#c00" ] [ text err ]
                        , H.button [ A.class "x", A.attribute "aria-label" "dismiss this error", A.onClick (DocError ""), S.color "#c00", S.marginLeft "0.5rem" ] [ text "×" ]
                        ]
                    ]
        , if List.isEmpty query.examples then
            text ""

          else
            H.div [ S.displayFlex, S.flexWrapWrap, S.gapRem 0.25, S.padding "0.5rem 0.75rem", S.backgroundColor "#f0f0f0", S.borderTop "1px solid #ddd" ]
                (List.map
                    (\example ->
                        H.button [ A.class "mono", A.onClick (InputChange QueryCode example), S.fontSizeRem 0.75, S.padding "0.125rem 0.375rem" ]
                            [ text (iif (String.length example > 40) (String.left 40 example ++ "…") example) ]
                    )
                    query.examples
                )
        , if List.isEmpty sheetRefs then
            text ""

          else
            H.div [ S.padding "0.5rem 0.75rem", S.backgroundColor "#f0f0f0", S.borderTop "1px solid #ddd", S.fontSizeRem 0.75, S.color "#666" ]
                [ H.span [ S.fontWeight "600" ] [ text "Sheet refs: " ]
                , text (String.join ", " (List.map (\s -> "@" ++ s) sheetRefs))
                , iif (Dict.size model.library > 5) (H.span [] [ text " ..." ]) (text "")
                ]
        ]


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    let
        info =
            model.library |> Dict.get sheet.id |> Maybe.withDefault { name = "", tags = [], scratch = False, system = False, thumb = E.null, seen = "", trashed = False, starred = False }

        stats =
            sheet.stats

        table =
            resolveTable model

        content =
            H.div [ S.overflowAuto, S.height "100%", S.backgroundColor "#eee" ]
                [ viewError model.error
                , case table of
                    Err "" ->
                        H.div [ S.textAlignCenter, S.paddingRem 2, S.color "#666" ] [ text "loading" ]

                    Err err ->
                        H.p [] [ text err ]

                    Ok { cols, rows } ->
                        let
                            sortedRows =
                                filterAndSort model.search sheet rows

                            doc =
                                sheet.doc |> Result.withDefault Library

                            pins =
                                pinLeft sheet cols

                            -- The ends of every shaded column, over the rows as
                            -- drawn: sorted, filtered and searched, the way the
                            -- totals line below them is.
                            extents =
                                cols
                                    |> Array.toList
                                    |> List.filterMap
                                        (\col ->
                                            -- `if` and not `iif`, which is
                                            -- strict and would fold every
                                            -- column's rows whether it is
                                            -- shaded or not. `numericColumn`
                                            -- because a shade word may arrive
                                            -- on a text column from a document
                                            -- this panel never wrote, and no
                                            -- extent is what stops it drawing.
                                            if Dict.member col.key sheet.shades && numericColumn col.typ then
                                                columnExtent sortedRows col |> Maybe.map (Tuple.pair col.key)

                                            else
                                                Nothing
                                        )
                                    |> Dict.fromList

                            -- The row handle, only where a drop has an honest target.
                            grab =
                                (case doc of
                                    Tab _ ->
                                        True

                                    _ ->
                                        False
                                )
                                    && inDocumentOrder model.search sheet rows
                        in
                        case doc of
                            Chart cfg ->
                                viewChart cfg { cols = cols, rows = rows }

                            Dashboard tiles ->
                                viewDashboard tiles

                            _ ->
                                H.div []
                                    [ iif (doc == Library) (viewGallery model) (text "")
                                    , viewFilterBar sheet (Array.length sortedRows) (Array.length rows)
                                    , H.table [ A.onMouseLeave (CellHover (xy -1 -1)), A.attribute "role" "grid", A.attribute "aria-multiselectable" "true", A.attribute "aria-label" (iif (doc == Library) "library" (iif (String.trim info.name == "") "untitled sheet" info.name)) ]
                                        [ H.tbody [] <|
                                            Array.toList <|
                                                Array.indexedMap (\n_ row -> viewTableRow sheet doc stats pins extents grab cols (n_ - 2) row) <|
                                                    Array.append (Array.repeat 3 Dict.empty) sortedRows
                                        , viewTableFooter model.trash sheet pins cols sortedRows
                                        ]
                                    ]
                ]

        aside =
            H.aside [ A.id "aside", S.displayFlex, S.flexDirectionColumn, S.height "100%", S.backgroundColor "#fff" ] <|
                case sheet.doc of
                    Ok (Query query) ->
                        [ viewQueryEditor model query ]

                    Ok NetHook ->
                        [ viewNetHook model ]

                    Ok (NetHttp cfg) ->
                        [ viewNetHttp model cfg ]

                    Ok (Alert cfg) ->
                        [ viewAlert model cfg ]

                    Ok (Chart cfg) ->
                        [ viewChartSettings model cfg ]

                    Ok (Dashboard tiles) ->
                        [ viewDashboardSettings tiles ]

                    Ok (NetSocket cfg) ->
                        [ viewNetSocket model cfg ]

                    _ ->
                        []
    in
    { title = "scrapsheets"
    , body =
        -- An embed is the sheet and nothing else: no toolbar, no search, no
        -- aside, no modals. What a viewer may see is whatever the sheet's own
        -- sharing already says, so an embed grants nothing a link would not.
        if model.embed then
            [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%", S.overflowXAuto ] [ content ] ]

        else
            [ viewAuthForm model.auth
            , viewFindReplace sheet.findReplace
            , viewDeleteConfirm model.deleteConfirm
            , viewPending model.pending
            , viewImport model.importing
            , viewSettings model.showSettings info model.share
            , viewShortcuts model.showShortcuts
            , viewPalette model
            , viewHistory model.error model.history
            , viewTutorial model.tutorial
            , H.div [ S.displayGrid, S.gapRem 0, S.userSelectNone, A.style "-webkit-user-select" "none", S.maxWidth "100vw", S.maxHeight "100vh", S.height "100%", S.width "100%" ]
                [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.width "100%", S.overflowXAuto, S.gapRem 0 ]
                    [ viewToolbar model info
                    , H.input [ A.value model.search, A.onInput (InputChange SheetSearch), A.placeholder "search", A.attribute "aria-label" "search the rows", S.width "100%", S.border "none", S.borderRadius "0", S.borderBottom "1px solid #aaa", S.padding "0.25rem 0.5rem", S.fontSizeRem 0.875, S.marginBottomPx -1, S.zIndex "2" ] []
                    , content
                    ]
                , aside
                ]
            ]
    }
