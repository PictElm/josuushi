port module Main exposing (main)

import BigInt exposing (BigInt)
import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Json
import Regex exposing (Regex)
import Task exposing (Task)

type alias AnInt = BigInt
anInt : Int -> AnInt
anInt = BigInt.fromInt
anIntFromString : String -> Maybe AnInt
anIntFromString = BigInt.fromIntString
anIntFromKnownString : String -> AnInt
anIntFromKnownString = BigInt.fromIntString >> Maybe.withDefault (anInt 0)
anIntToString : AnInt -> String
anIntToString = BigInt.toString
addAnInt : AnInt -> AnInt -> AnInt
addAnInt = BigInt.add
mulAnInt : AnInt -> AnInt -> AnInt
mulAnInt = BigInt.mul
ltAnInt : AnInt -> AnInt -> Bool
ltAnInt = BigInt.lt
divmodAnInt : AnInt -> AnInt -> Maybe (AnInt, AnInt)
divmodAnInt = BigInt.divmod

type alias Ruby =
  { text : String
  , floating : String
  }

type alias Exception = Ruby -- YYY: usl?

type alias Counter =
  { repr : {-List-} Ruby -- kanji and reading for the counter
  , tags : List String -- what it may be a counter for
  , cases : Dict Int Exception -- special cases / exceptions
  }

type alias CounterIndex =
  { byRef : Dict String (List String) -- ref -> tags
  , byTag : Dict String (List String) -- tag -> refs
  }

rubyCat : List Ruby -> Ruby
rubyCat l =
  List.foldl (\b -> \a ->
    Ruby
      (a.text ++ b.text)
      (String.split "/" a.floating
        |> List.concatMap (\p -> String.split "/" b.floating |> List.map ((++) p))
        |> String.join "/") -- YYY: gets dumb for larg numbers, but that will do
  ) (Ruby "" "") l

rubyFromList : List String -> Ruby
rubyFromList list =
  Ruby
    (List.head list |> Maybe.withDefault "ERROR")
    (List.drop 1 list |> List.head |> Maybe.withDefault "error")

rubyDecoder : Json.Decoder Ruby
rubyDecoder =
  Json.map rubyFromList (Json.list Json.string)

filterIntKeys : Dict String a -> Dict Int a
filterIntKeys dict =
  Dict.fromList (
    Dict.toList dict
      |> List.filterMap (\(k, v) -> String.toInt k |> Maybe.andThen (\kk -> Just (kk, v)))
  )

casesDecoder : Json.Decoder (Dict Int Exception)
casesDecoder =
  Json.map filterIntKeys (Json.dict rubyDecoder)

counterDecoder : Json.Decoder Counter
counterDecoder =
  Json.map3 Counter
    (Json.field "repr" rubyDecoder)
    (Json.field "tags" (Json.list Json.string))
    (Json.field "cases" casesDecoder)

insertByTags : (String, List String) -> Dict String (List String) -> Dict String (List String)
insertByTags (ref, tags) acc =
  List.foldr
    (\tag bcc ->
      Dict.insert
        tag
        (Dict.get tag bcc
          |> Maybe.withDefault []
          |> (::) ref)
        bcc
    )
    acc
    (List.concatMap String.words tags)

counterIndexDecoder : Json.Decoder (List (String, List String))
counterIndexDecoder =
  Json.list
    <| Json.map2 Tuple.pair
      (Json.index 0 Json.string)
      (Json.index 1 (Json.list Json.string))

counterIndex : String -> CounterIndex
counterIndex json =
  let
    pairs = Json.decodeString counterIndexDecoder json
      |> Result.withDefault []
  in
    { byRef = Dict.fromList pairs
    , byTag = List.foldr insertByTags Dict.empty pairs
    }

viewRuby : Ruby -> Html.Html Message
viewRuby rb =
  Html.span []
    <| (
      List.map
        (\it ->
          Html.ruby []
            [ Html.text rb.text
            , Html.text "​"
            , Html.rp [] [ Html.text "(" ]
            , Html.text "​"
            , Html.rt [] [ Html.text it ]
            , Html.text "​"
            , Html.rp [] [ Html.text ")" ]
            ] -- YYY: the seemingly empty texts above are 0-width spaces; for some reason it behaves better
        )
        (String.split "/" rb.floating)
        |> List.intersperse (Html.text "・")
    )

ifBelow : AnInt -> List (AnInt, AnInt -> a) -> (AnInt -> a) -> a
ifBelow an below above =
  case List.head below of
    Just it ->
      if ltAnInt an (Tuple.first it)
        then Tuple.second it <| an
        else ifBelow an
          (List.tail below |> Maybe.withDefault [])
          above
    Nothing ->
      above an

one_nine_ : Dict Int Ruby
one_nine_ =
  Dict.fromList
    --[ (0, Ruby "零" "れい") -- YYY: (hm...)
    [ (0, Ruby "" "")
    , (1, Ruby "一" "いち")
    , (2, Ruby "二" "に")
    , (3, Ruby "三" "さん")
    , (4, Ruby "四" "よん")
    , (5, Ruby "五" "ご")
    , (6, Ruby "六" "ろく")
    , (7, Ruby "七" "なな")
    , (8, Ruby "八" "はち")
    , (9, Ruby "九" "きゅう")
    ]
ten_ : Ruby
ten_ = Ruby "十" "じゅう"
hundred_ : Ruby
hundred_ = Ruby "百" "ひゃく"
three_bundred_ : Ruby
three_bundred_ = Ruby "三百" "さんびゃく"
six_pundred_ : Ruby
six_pundred_ = Ruby "六百" "ろっぴゃく"
eight_pundred_ : Ruby
eight_pundred_ = Ruby "八百" "はっぴゃく"
thousand_ : Ruby
thousand_ = Ruby "千" "せん"
three_zousand_ : Ruby
three_zousand_ = Ruby "三千" "さんぜん"
eight_thousand_ : Ruby
eight_thousand_ = Ruby "八千" "はっせん"
tenthousand_ : Ruby
tenthousand_ = Ruby "万" "まん"
one_tenthousand_ : Ruby
one_tenthousand_ = Ruby "一万" "いちまん"
hundredmillion_ : Ruby
hundredmillion_ = Ruby "億" "おく"
one_hundredmillion_ : Ruby
one_hundredmillion_ = Ruby "一億" "いちおく"
trillion_ : Ruby
trillion_ = Ruby "兆" "ちょう"
one_trillion_ : Ruby
one_trillion_ = Ruby "一兆" "いっちょう"
tenquadrillion_ : Ruby
tenquadrillion_ = Ruby "京" "けい"
one_tenquadrillion_ : Ruby
one_tenquadrillion_ = Ruby "一京" "いっけい"
hundredquintillion_ : Ruby
hundredquintillion_ = Ruby "垓" "がい"
one_hundredquintillion_ : Ruby
one_hundredquintillion_ = Ruby "一垓" "いちがい"
gatherCounterWithRuby : AnInt -> Dict Int Ruby -> Ruby -> Ruby -- into a list of
gatherCounterWithRuby number cases repr =
  let
    repr_ = Dict.get 0 cases |> Maybe.withDefault repr
    recu : AnInt -> Bool -> Ruby
    recu an may_tail = (
      let
        n = String.toInt (anIntToString an) |> Maybe.withDefault 0
        special = if may_tail then Dict.get n cases else Nothing
        tailing = (if may_tail then \w -> rubyCat [w, repr_] else identity) >> always
        rubyCatWithTailing = (\bn -> \k -> \l ->
            let o = String.toInt (anIntToString bn) |> Maybe.withDefault 0
            in if (anInt 0) == k && may_tail
              then
                let ex = Dict.get o cases |> Maybe.map List.singleton
                in rubyCat (case l of
                    [b]      -> ex |> Maybe.withDefault [b, repr_]
                    a :: [b] -> a :: (ex |> Maybe.withDefault [b, repr_])
                    _        -> [] -- unreachable
                  ) -- working with Lists in elm is...
              else rubyCat (l ++ [recu k may_tail])
          )
        recursing = (\anExactValue -> \aPowOfTen -> \itsRuby -> \itContainsItsDigit -> \u ->
            let (div, mod) = divmodAnInt u aPowOfTen |> Maybe.withDefault (anInt 0, anInt 0) -- YYY: that even reachable?
            in rubyCatWithTailing anExactValue mod (
              if itContainsItsDigit
                then [                itsRuby]
                else [recu div False, itsRuby]
              )
          )
      in case special of
        Just ex -> ex -- special cases already contains the counter's repr
        Nothing -> case Dict.get n one_nine_ of
          Just it -> tailing it n
          Nothing -> ifBelow an
            [ ( anInt 11,  tailing ten_ )
            , ( anInt 20,  recursing (anInt 10) (anInt 10) ten_ True  )
            , ( anInt 100, recursing (anInt 10) (anInt 10) ten_ False )
            --
            , ( anInt 101,  tailing hundred_ )
            , ( anInt 200,  recursing (anInt 100) (anInt 100)       hundred_ True  )
            , ( anInt 300,  recursing (anInt 100) (anInt 100)       hundred_ False )
            , ( anInt 400,  recursing (anInt 300) (anInt 100) three_bundred_ True  )
            , ( anInt 600,  recursing (anInt 100) (anInt 100)       hundred_ False )
            , ( anInt 700,  recursing (anInt 600) (anInt 100)   six_pundred_ True  )
            , ( anInt 800,  recursing (anInt 100) (anInt 100)       hundred_ False )
            , ( anInt 900,  recursing (anInt 800) (anInt 100) eight_pundred_ True  )
            , ( anInt 1000, recursing (anInt 100) (anInt 100)       hundred_ False )
            --
            , ( anInt 1001,  tailing thousand_ )
            , ( anInt 2000,  recursing (anInt 1000) (anInt 1000)       thousand_ True  )
            , ( anInt 3000,  recursing (anInt 1000) (anInt 1000)       thousand_ False )
            , ( anInt 4000,  recursing (anInt 3000) (anInt 1000)  three_zousand_ True  )
            , ( anInt 8000,  recursing (anInt 1000) (anInt 1000)       thousand_ False )
            , ( anInt 9000,  recursing (anInt 8000) (anInt 1000) eight_thousand_ True  )
            , ( anInt 10000, recursing (anInt 1000) (anInt 1000)       thousand_ False )
            --
            , ( anInt 10001,     tailing one_tenthousand_ ) -- XXX: (is-)sen man [?]
            , ( anInt 100000000, recursing (anInt 10000) (anInt 10000) tenthousand_ False )
            --
            , ( anInt 100000001,     tailing one_hundredmillion_ ) -- XXX: (is-)sen oku [?]
            , ( anInt 1000000000000, recursing (anInt 100000000) (anInt 100000000) hundredmillion_ False )
            --
            , ( anInt 1000000000001,     tailing one_trillion_ )
            , ( anIntFromKnownString "10000000000000000", recursing (anInt 1000000000000) (anInt 1000000000000) trillion_ False )
            --
            , ( anIntFromKnownString "10000000000000001",     tailing one_tenquadrillion_ )
            , ( anIntFromKnownString "100000000000000000000", recursing (anIntFromKnownString "10000000000000000") (anIntFromKnownString "10000000000000000") tenquadrillion_ False )
            --
            , ( anIntFromKnownString "100000000000000000001",     tailing one_hundredquintillion_ )
            , ( anIntFromKnownString "1000000000000000000000000", recursing (anIntFromKnownString "100000000000000000000") (anIntFromKnownString "100000000000000000000") hundredquintillion_ False )
            --
            ] (\_ -> Ruby "全部の" "ぜんぶの") -- YYY: or 沢山 or idk (hm...)
      )
  in if anInt 0 == number
    then Ruby "無い" "ない" -- YYY: or idk (hm...)
    else recu number True

counterToRubyException : Exception -> Ruby
counterToRubyException special =
  special

counterToRubyRegular : Counter -> AnInt -> Ruby
counterToRubyRegular counter number =
  gatherCounterWithRuby number counter.cases counter.repr

counterToRuby : Counter -> AnInt -> (Ruby, Bool)
counterToRuby counter an =
  let n = String.toInt (anIntToString an) |> Maybe.withDefault 0
  in case Dict.get -n counter.cases of
    Just exact_ex -> (counterToRubyException exact_ex, True)
    Nothing ->
      case Dict.get n counter.cases of
        Just generic_ex -> (counterToRubyException generic_ex, True)
        Nothing -> (counterToRubyRegular counter an, False)

kanji_digits_ : Dict Char AnInt
kanji_digits_ =
  Dict.fromList
    [ ('一', anInt 1)
    , ('二', anInt 2)
    , ('三', anInt 3)
    , ('四', anInt 4)
    , ('五', anInt 5)
    , ('六', anInt 6)
    , ('七', anInt 7)
    , ('八', anInt 8)
    , ('九', anInt 9)
    ]
kanji_tens_ : Dict Char AnInt
kanji_tens_ =
  Dict.fromList
    [ ('十', anInt 10)
    , ('百', anInt 100)
    , ('千', anInt 1000)
    ]
kanji_tenthousands_ : Dict Char AnInt
kanji_tenthousands_ =
  Dict.fromList
    [ ('万', anInt 10000)
    , ('億', anInt 100000000)
    , ('兆', anInt 1000000000000)
    , ('京', anIntFromKnownString "10000000000000000")
    , ('垓', anIntFromKnownString "100000000000000000000")
    ]
kanji_numerals_ : String
kanji_numerals_ =
  String.fromList
    (  Dict.keys kanji_digits_
    ++ Dict.keys kanji_tens_
    ++ Dict.keys kanji_tenthousands_
    )

nextDigitValue : List Char -> Maybe (AnInt, List Char)
nextDigitValue l =
  case l of
    h :: t -> case Dict.get h kanji_digits_ of
      Just v  -> Just (v, t)
      Nothing -> Just (anInt 1, l)
    []     -> Just (anInt 1, [])

nextTensValue : List Char -> Maybe (AnInt, List Char)
nextTensValue l =
  let multByAndRecurse = \x -> \(a, l2) -> nextTensValue l2 |> Maybe.map (Tuple.mapFirst (addAnInt (mulAnInt a x)))
  in case l of
    h :: t -> case Dict.get h kanji_tens_ of
      Just v  -> nextDigitValue t |> Maybe.andThen (multByAndRecurse v)
      Nothing -> if Dict.member h kanji_tenthousands_
        then Just (anInt 0, l)
        else nextDigitValue l |> Maybe.andThen (multByAndRecurse (anInt 1))
    []     -> Just (anInt 0, [])

nextTenthousandsValue : List Char -> Maybe (AnInt, List Char)
nextTenthousandsValue l =
  let multByAndRecurse = \x -> \(a, l2) -> nextTenthousandsValue l2 |> Maybe.map (Tuple.mapFirst (addAnInt (mulAnInt a x)))
  in case l of
    h :: t -> case Dict.get h kanji_tenthousands_ of
      Just v  -> nextTensValue t |> Maybe.andThen (multByAndRecurse v)
      Nothing -> nextTensValue l |> Maybe.andThen (multByAndRecurse (anInt 1))
    []     -> Just (anInt 0, [])

kanjiToInt : String -> Maybe AnInt
kanjiToInt c = -- YYY: does not catch incorrect input, sad (garbage in, garbage out)
  String.toList c
    |> List.reverse
    |> nextTenthousandsValue
    |> Maybe.map Tuple.first

parseInt : String -> Maybe AnInt
parseInt c =
  let f = String.left 1 c
  in if String.contains f kanji_numerals_
    then kanjiToInt c
    else anIntFromString (
        let cc = String.filter (\k -> not <| List.member k [' ', ',', '_', '　', '，']) c
        in if String.contains f "0123456789 ,_"
          then cc
          else cc |> String.map (\k -> Char.toCode k |> (+) -65248 |> Char.fromCode) -- ord('０')-ord('0')
      )

rx_number_ : Regex
rx_number_ = Regex.fromString ("^.*?[0-9 ,_]+|[" ++ kanji_numerals_ ++ "]+|[０-９　，]+") |> Maybe.withDefault Regex.never
rx_tag_ : Regex
rx_tag_ = Regex.fromString "[A-Za-z]+|[一-龯]|[ぁ-んァ-ン]+" |> Maybe.withDefault Regex.never
parseQuery : String -> (Maybe (String, AnInt), List String)
parseQuery qq =
  let
    (str, num) = case Regex.findAtMost 1 rx_number_ qq of
      head :: _ -> (head.match, parseInt head.match)
      []        -> ("", Nothing)
    qqq = String.dropLeft (String.length str) qq
  in
    ( num |> Maybe.map (Tuple.pair str)
    , Regex.find rx_tag_ qqq |> List.map .match
    )

pluralIrregulars_ : Dict String String
pluralIrregulars_ =
  Dict.fromList
    [ ("child", "children")
    , ("goose", "geese")
    , ("man", "men")
    , ("woman", "women")
    , ("tooth", "teeth")
    , ("foot", "feet")
    , ("mouse", "mice")
    , ("person", "people")
    ]
potentialPlurals : String -> List String
potentialPlurals c =
  let
    rev = String.toList c |> List.reverse
    (last, onetolast) = case rev of
      z::y::_ -> (z, y)
      _ -> (' ', ' ')
  in case Dict.get c pluralIrregulars_ of
    Just it -> [it]
    Nothing -> List.concat
      [ case rev of
        'y'::t      -> ['s'::'e'::'i'::t |> List.reverse |> String.fromList]
        'f'::t      -> ['s'::'e'::'v'::t |> List.reverse |> String.fromList]
        'e'::'f'::t -> ['s'::'e'::'v'::t |> List.reverse |> String.fromList]
        's'::'i'::t -> ['s'::'e'::t      |> List.reverse |> String.fromList]
        's'::'u'::t -> ['i'::t           |> List.reverse |> String.fromList]
        'n'::'o'::t -> ['a'::t           |> List.reverse |> String.fromList]
        _ -> []
      , if String.contains (String.fromChar last) "shxzo"
        then (c ++ "es") :: (if 's' == onetolast || 'z' == onetolast
          then [c ++ String.cons onetolast "es"]
          else [])
        else [c ++ "s"]
      ]

hiraganaMap1_ : Dict String String
hiraganaMap1_ =
  Dict.fromList
    [ ("n", "ん")
    , ("a", "あ")
    , ("i", "い")
    , ("u", "う")
    , ("e", "え")
    , ("o", "お")
    ]
hiraganaMap2_ : Dict String String
hiraganaMap2_ =
  Dict.fromList
    [ ("nn", "ん")
    , ("ka", "か")
    , ("ki", "き")
    , ("ku", "く")
    , ("ke", "け")
    , ("ko", "こ")
    , ("ga", "が")
    , ("gi", "ぎ")
    , ("gu", "ぐ")
    , ("ge", "げ")
    , ("go", "ご")
    , ("sa", "さ")
    , ("si", "し")
    , ("su", "す")
    , ("se", "せ")
    , ("so", "そ")
    , ("za", "ざ")
    , ("zi", "じ")
    , ("zu", "ず")
    , ("ze", "ぜ")
    , ("zo", "ぞ")
    , ("ja", "じゃ")
    , ("ji", "じ")
    , ("ju", "じゅ")
    , ("jo", "じょ")
    , ("ta", "た")
    , ("ti", "ち")
    , ("tu", "つ")
    , ("te", "て")
    , ("to", "と")
    , ("da", "だ")
    , ("di", "ぢ")
    , ("du", "づ")
    , ("de", "で")
    , ("do", "ど")
    , ("na", "な")
    , ("ni", "に")
    , ("nu", "ぬ")
    , ("ne", "ね")
    , ("no", "の")
    , ("ha", "は")
    , ("hi", "ひ")
    , ("hu", "ふ")
    , ("fu", "ふ")
    , ("he", "へ")
    , ("ho", "ほ")
    , ("ba", "ば")
    , ("bi", "び")
    , ("bu", "ぶ")
    , ("be", "べ")
    , ("bo", "ぼ")
    , ("pa", "ぱ")
    , ("pi", "ぴ")
    , ("pu", "ぷ")
    , ("pe", "ぺ")
    , ("po", "ぽ")
    , ("ma", "ま")
    , ("mi", "み")
    , ("mu", "む")
    , ("me", "め")
    , ("mo", "も")
    , ("ya", "や")
    , ("yu", "ゆ")
    , ("yo", "よ")
    , ("ra", "ら")
    , ("ri", "り")
    , ("ru", "る")
    , ("re", "れ")
    , ("ro", "ろ")
    , ("wa", "わ")
    , ("wo", "を")
    ]
hiraganaMap3_ : Dict String String
hiraganaMap3_ =
  Dict.fromList
    [ ("kya", "きゃ")
    , ("kyu", "きゅ")
    , ("kyo", "きょ")
    , ("gya", "ぎゃ")
    , ("gyu", "ぎゅ")
    , ("gyo", "ぎょ")
    , ("sya", "しゃ")
    , ("syu", "しゅ")
    , ("syo", "しょ")
    , ("sha", "しゃ")
    , ("shi", "し")
    , ("shu", "しゅ")
    , ("sho", "しょ")
    , ("zya", "じゃ")
    , ("zyu", "じゅ")
    , ("zyo", "じょ")
    , ("tya", "ちゃ")
    , ("tyu", "ちゅ")
    , ("tyo", "ちょ")
    , ("cha", "ちゃ")
    , ("chi", "ち")
    , ("chu", "ちゅ")
    , ("cho", "ちょ")
    , ("dya", "ぢゃ")
    , ("dyu", "ぢゅ")
    , ("dyo", "ぢょ")
    , ("nya", "にゃ")
    , ("nyu", "にゅ")
    , ("nyo", "にょ")
    , ("hya", "ひゃ")
    , ("hyu", "ひゅ")
    , ("hyo", "ひょ")
    , ("bya", "びゃ")
    , ("byu", "びゅ")
    , ("byo", "びょ")
    , ("pya", "ぴゃ")
    , ("pyu", "ぴゅ")
    , ("pyo", "ぴょ")
    , ("mya", "みゃ")
    , ("myu", "みゅ")
    , ("myo", "みょ")
    , ("rya", "りゃ")
    , ("ryu", "りゅ")
    , ("ryo", "りょ")
    , ("jya", "じゃ")
    , ("jyu", "じゅ")
    , ("jyo", "じょ")
    , ("tsu", "つ")
    ]
potentialKana : String -> List String
potentialKana c =
  let
    recu : List Char -> Maybe String
    recu cc =
      case cc of
        ' '::_ -> Just ""
        a3::b3::c3::t3 -> case Dict.get (String.fromList [a3, b3, c3]) hiraganaMap3_ of
          Just r3 -> recu t3 |> Maybe.map ((++) r3)
          Nothing ->
            case cc of
              a2::b2::t2 -> case Dict.get (String.fromList [a2, b2]) hiraganaMap2_ of
                Just r2 -> recu t2 |> Maybe.map ((++) r2)
                Nothing ->
                  if a2 == b2 && String.contains (String.fromChar a2) "kgszjtdhfbpmyr"
                    then recu (b2::t2) |> Maybe.map ((++) "っ")
                    else case cc of
                      a1::t1 -> case Dict.get (String.fromChar a1) hiraganaMap1_ of
                        Just r1 -> recu t1 |> Maybe.map ((++) r1)
                        Nothing -> Nothing
                      _ -> Nothing
              _ -> Nothing
        _ -> Nothing
  in case recu (String.toList c ++ [' ', ' ']) of
    Just it -> [it] -- YYY: also katakana
    Nothing -> []

processQuery : CounterIndex -> String -> (Maybe (String, AnInt), List String)
processQuery index query =
  let
    (num_pair, tags) = parseQuery query
    moreTags
      =  tags
      ++ List.concat (List.map potentialPlurals tags)
      ++ List.concat (List.map potentialKana tags)
  in
    ( num_pair
    , if List.isEmpty tags
      then Dict.get "things" index.byTag |> Maybe.withDefault [] --[ "_07b87" ] --[ "tu" ] -- YYY: default when no counter (hm...)
      else List.foldr
        (\tag -> \dct ->
          Dict.get tag index.byTag
            |> Maybe.withDefault []
            |> List.foldl (\it -> Dict.update it (Maybe.withDefault 0 >> (+) 1 >> Just)) dct
        ) Dict.empty moreTags
          |> Dict.toList
          |> List.sortBy Tuple.second
          |> List.map Tuple.first
          |> List.reverse
    )

responseToDecodedCounter : Http.Response String -> Result Http.Error Counter
responseToDecodedCounter response =
  case response of
    Http.BadUrl_ url           -> Err (Http.BadUrl url)
    Http.Timeout_              -> Err Http.Timeout
    Http.NetworkError_         -> Err Http.NetworkError
    Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
    Http.GoodStatus_ _ body ->
      case Json.decodeString counterDecoder body of
        Ok value -> Ok value
        Err err  -> Err (Http.BadBody (Json.errorToString err))

refToRequestTask : String -> Task Http.Error Counter
refToRequestTask ref =
  Http.task
    { method = "GET"
    , headers = []
    , url = "data/" ++ ref ++ ".json"
    , body = Http.emptyBody
    , resolver = Http.stringResolver responseToDecodedCounter
    , timeout = Nothing
    }

viewResult : Maybe AnInt -> Counter -> Html.Html Message
viewResult num counter =
  Html.div
    [ Attr.class "result" ]
    [ Html.div
      [ Attr.class "repr" ]
      [ viewRuby counter.repr ]
    , Html.div
      [ Attr.class "info" ]
      [ Html.div
        [ Attr.class "overview" ]
        [ Html.span
          [ Attr.class "tags" ]
          [ Html.text (String.join ", " counter.tags) ] -- YYY: highlight matches from query?
        , Html.br [] []
        , Html.a
          [ Attr.href ("https://jisho.org/word/" ++ counter.repr.text) ]
          [ Html.text "Jisho.org" ]
        ] -- .overview
      , Html.span
        [ Attr.class "computed"
        , Attr.class (if Nothing == num then "empty" else "filled")
        ]
        [ case num of
          Just n  -> viewRuby <| Tuple.first <| counterToRuby counter n
          Nothing -> Html.text "enter a number"
        ] -- .computed
      , Html.details
        []
        [ Html.summary
          []
          []
        , Html.div
          []
          [ Html.table
            []
            [ Html.thead
              []
              [ Html.tr
                []
                [ Html.th [] [ Html.text "number" ]
                , Html.th [] [ Html.text "writing" ]
                , Html.th [] [ Html.text "reading" ]
                ]
              ]
            , Html.tbody
              []
              <| List.map
                (\n ->
                  let (ruby, is_ex) = counterToRuby counter (anInt n)
                  in Html.tr
                    [ Attr.class (if is_ex then "exception" else "regular") ]
                    [ Html.td [] [ Html.text (String.fromInt n) ]
                    , Html.td [] [ Html.text ruby.text ]
                    , Html.td [] [ Html.text (String.split "/" ruby.floating |> String.join "・") ]
                    ]
                )
                (List.range 1 10 ++ List.filter (\n -> 10 < n) (Dict.keys counter.cases))
            ] -- table
          ] -- summary>div
        ] -- summary
      ] -- .info
    ] -- .repr

viewHomePage : Model -> Html.Html Message
viewHomePage model =
  Html.div
    [ Attr.class "page-home" ]
    [ Html.p
      []
      [ Html.text "Hello! Use the search bar to query a counter for something, for example:"
      , Html.ul
        []
        (List.map
          (\(t, q) ->
            Html.li
              []
              [ Html.text (t ++ ": ")
              , Html.a [ Attr.href ("?q=" ++ q) ] [ Html.text q ]
              ])
          [ ("counter for cats", "cats")
          , ("counter for round objects", "round objects")
          , ("exactly 7 days", "7 days")
          , ("using a kanji", "枚")
          , ("or its reading in kana or romaji", "まい")
          , ("reading of a number (will use the 〜つ)", "262816")
          , ("if 'peacocks' does not find anything, try with a more general", "animals")
          ])
      , Html.br [] []
      , Html.text (String.fromInt (Dict.size model.counters.byRef) ++ " counters in database.")
      ]
    ]

viewLoading : Model -> Html.Html Message
viewLoading model =
  Html.div
    [ Attr.class "page-loading" ]
    [ Html.p
      []
      [ Html.text ("fetching counter data for " ++ String.join ", " model.parsedTags) ]
    ]

viewNotFound : Model -> Http.Error -> Html.Html Message
viewNotFound model err =
  Html.div
    [ Attr.class "page-not-found" ]
    [ Html.p
      []
      <| List.concat
        [ [ Html.text "Oops, an error occurred when trying to get counter data."
          , Html.br [] []
          , Html.text "Dev hint: trying to fetch "
          ]
        , List.map
          (\t -> Html.code [] [ Html.text t ])
          model.parsedTags
          |> List.intersperse (Html.text ", ")
        , [ Html.text "."
          , Html.br [] []
          , Html.text "HTTP error: "
          , Html.code
            []
            [ Html.text
              (case err of
                Http.BadUrl str    -> ("bad URL: " ++ str)
                Http.Timeout       -> "timeout"
                Http.NetworkError  -> "network error"
                Http.BadStatus int -> ("bad status: " ++ String.fromInt int)
                Http.BadBody str   -> ("bad body: " ++ str)
              )
            ]
          ]
        ]
    ]

viewFound : Model -> List Counter -> Html.Html Message
viewFound model list =
  Html.div
    [ Attr.class "page-found" ]
    (if 0 == List.length list
      then
        [ Html.p
          []
          [ Html.text "Could not find a suitable counter for the given query... try using more general terms, for example "
          , Html.strong [] [ Html.text "papers" ]
          , Html.text " or "
          , Html.strong [] [ Html.text "flat" ]
          , Html.text " instead of "
          , Html.strong [] [ Html.text "flyers" ]
          , Html.text "."
          ]
        ]
      else
        [ Html.p
          []
          [ Html.text (String.fromInt (List.length list) ++ " result" ++ (if 1 == List.length list then "" else "s")) ]
        , Html.input
          [ Attr.id "number-input"
          , Attr.placeholder "number"
          , Attr.value model.numberInput
          , Event.onInput UpdateNumber
          ]
          []
        , Html.ol
            []
            (List.map
              (\counter ->
                Html.li
                  []
                  [ viewResult model.number counter ]
              )
              list)
        ]) -- #number-input

--- application
type Page
  = HomePage
  | Loading
  | NotFound Http.Error
  | Found (List Counter)

type alias Model =
  { query : String
  , numberInput : String
  , number : Maybe AnInt
  , current : Page
  , parsedTags : List String
  , counters : CounterIndex
  , theme : String
  }

type Message
  = UpdateQuery String
  | UpdateNumber String
  | SearchCounter
  | GotResult (Result Http.Error (List Counter))
  | ChangeTheme

port setStorage : String -> Cmd msg

init : (String, String, String) -> (Model, Cmd Message)
init (jsonCounterIndex, urlQuery, theme) =
  ( { query = urlQuery
    , numberInput = ""
    , number = Nothing
    , current = HomePage
    , parsedTags = ["(wait, how did you..?)"]
    , counters = counterIndex jsonCounterIndex -- TODO: if this double-parsing becomes a problem, only do in js-side (only really use the `byTags` anyways)
    , theme = theme
    }
  , if "" == urlQuery
    then Cmd.none
    else Task.perform (always SearchCounter) (Task.succeed 0)
  )

subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case msg of
    UpdateQuery niw ->
      ( { model
        | query = niw
        }
      , Cmd.none
      )
    UpdateNumber niw ->
      ( { model
        | numberInput = niw
        , number = parseQuery niw
          |> Tuple.first
          |> Maybe.map Tuple.second
        }
      , Cmd.none
      )
    SearchCounter ->
      let
        (number, list) = processQuery model.counters model.query
        model_wnum = case number of
          Just (str, num) ->
            { model
            | numberInput = str
            , number = Just num
            }
          Nothing -> model
      in
        ( { model_wnum
          | current = Loading
          , parsedTags = list
          }
        , list
          |> List.map refToRequestTask -- List (Task Http.Error Counter)
          |> Task.sequence -- Task Http.Error (List Counter)
          |> Task.attempt GotResult -- Cmd Message
        )
    GotResult res ->
      case res of
        Ok list ->
          ( { model
            | current = Found list
            }
          , Cmd.none
          )
        Err err ->
          ( { model
            | current = NotFound err
            }
          , Cmd.none
          )
    ChangeTheme ->
      let
        niwTheme = case model.theme of
          "dark"  -> "light"
          "light" -> "dark"
          _       -> ""
      in
        ( { model
          | theme = niwTheme
          }
        , setStorage niwTheme
        )

view : Model -> Html.Html Message
view model =
  Html.div
    [ Attr.id "root"
    , Attr.class model.theme
    ]
    [ Html.header
      []
      [ Html.a
        [ Attr.href "/" ]
        [ Html.h1
          []
          [ viewRuby (Ruby "助数詞" "じょすうし")
          , Html.text " — counters"
          ]
        ]
      , Html.button
        [ Attr.id "theme-change"
        , Event.onClick ChangeTheme
        ]
        [ Html.text "change theme" ]
      ] -- header
    , Html.hr [] []
    , Html.form
      [ Attr.id "search-bar"
      , Event.onInput UpdateQuery
      , Event.onSubmit SearchCounter
      ]
      [ Html.input
        [ Attr.placeholder "counter/reading for..."
        , Attr.value model.query
        , Attr.autofocus True
        ]
        []
      , Html.button
        []
        [ Html.text "find" ]
      ] -- #search-bar
    , Html.div
      [ Attr.id "page-content" ]
      [ case model.current of
        HomePage   -> viewHomePage model
        Loading    -> viewLoading model
        NotFound e -> viewNotFound model e
        Found list -> viewFound model list
      ] -- #page-content
    , Html.hr [] []
    , Html.footer
      []
      [ Html.p
        []
        [ Html.text
            """
              This page is made available with the hope of being helpful at most; it should not be taken as always accurate.
              If you believe you found an error, it could very much be one. In that case, you may report it.
              Because it is still in quite early development, not many counters have been added to the database.
              It will be filled slowly. Thank you for your understanding.
            """
        ]
      , Html.ul
        []
        (List.map (\(text, href) -> Html.li [] [ Html.a [ Attr.href href, Attr.target "_blank", Attr.rel "noopener" ] [ Html.text text ] ])
          [ ("explore the sources", "https://github.com/naclsn/josuushi")
          , ("report an error",     "https://github.com/naclsn/josuushi/issues/new?labels=error&template=error-report.md")
          , ("report a bug",        "https://github.com/naclsn/josuushi/issues/new?labels=bug&template=bug-report.md")
          , ("request for counter", "https://github.com/naclsn/josuushi/issues/new?labels=rfc&template=counter-request.md")
          ])
      ] -- footer
    ] -- #root

--- entry point
main : Program (String, String, String) Model Message
main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
