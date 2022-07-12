module Main exposing (main)

import Array
import Dict exposing (Dict)
import Browser
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as Json

import Embedded as Emb

type alias Ruby =
  { text : String
  , floating : String
  }

type alias Exception = Ruby

type alias Counter =
  { repr : {-List-} Ruby -- kanji and reading for the counter
  , tags : List String -- what it may be a counter for
  , cases : Dict Int Exception -- special cases / exceptions
  }

type alias CounterStore =
  { byRef : Dict String Counter
  , byTag : Dict String (List Counter)
  }

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

loadCounter : (String, String) -> Maybe (String, Counter)
loadCounter (ref, json) =
  case Json.decodeString counterDecoder json of
    Result.Ok it -> Just (ref, it)
    Result.Err e -> -- YYY: temp hack to get notified in console on fail
      (Just (Debug.log "loadCounter failed" (ref, e)))
        |> Maybe.andThen (\_ -> Nothing)

insertByTags : (String, Counter) -> Dict String (List Counter) -> Dict String (List Counter)
insertByTags (ref, counter) acc =
  List.foldr
    (\tag bcc ->
      Dict.insert
        tag
        (Dict.get tag bcc
          |> Maybe.withDefault []
          |> (::) counter)
        bcc
    )
    acc
    counter.tags

loadEveryCounters : CounterStore
loadEveryCounters =
  let
    all = Dict.toList Emb.all
      |> List.filterMap loadCounter
  in
    { byRef = Dict.fromList all
    , byTag = List.foldr insertByTags Dict.empty all
    }

--- kanji
viewRuby : Ruby -> Html.Html Message
viewRuby rb =
  Html.ruby []
    [ Html.text rb.text
    , Html.rp [] [ Html.text "(" ]
    , Html.rt [] [ Html.text rb.floating ]
    , Html.rp [] [ Html.text ")" ]
    ]


ifBelow : Int -> List (Int, Int -> a) -> (Int -> a) -> a
ifBelow u below above =
  case List.head below of
    Just it ->
      if u < Tuple.first it
        then Tuple.second it <| u
        else ifBelow u
          (List.tail below |> Maybe.withDefault [])
          above
    Nothing ->
      above u

hundred_ = Ruby "百" "ひゃく"
three_bundred_ = Ruby "三百" "さんびゃく"
six_pundred_ = Ruby "六百" "ろっぴゃく"
eight_pundred_ = Ruby "八百" "はっぴゃく"
ten_ = Ruby "十" "じゅう"
one_ten_ =
  Array.fromList
    [ Ruby "一" "いち"
    , Ruby "二" "に"
    , Ruby "三" "さん"
    , Ruby "四" "よん"
    , Ruby "五" "ご"
    , Ruby "六" "ろく"
    , Ruby "七" "なな"
    , Ruby "八" "はち"
    , Ruby "九" "きゅう"
    , ten_
    ]
viewNumberWithRuby : Int -> Html.Html Message
viewNumberWithRuby number =
  ifBelow number
    [ (11,   \u -> Array.get (u - 1) one_ten_ |> Maybe.map viewRuby |> Maybe.withDefault (Html.div [] [ Html.text "unreachable" ]))
    , (20,   \u -> Html.span [] [ viewRuby ten_, viewNumberWithRuby (modBy 10 u) ])
    , (100,  \u -> Html.span [] [ viewNumberWithRuby (u // 10), viewNumberWithRuby (10 + modBy 10 u) ])
    , (101,  \u -> Html.span [] [ viewRuby hundred_ ])
    , (200,  \u -> Html.span [] [ viewRuby hundred_, viewNumberWithRuby (modBy 100 u) ])
    , (300,  \u -> Html.span [] [ viewNumberWithRuby (u // 100), viewNumberWithRuby (100 + modBy 100 u) ])
    , (400,  \u -> Html.span [] [ viewRuby three_bundred_, viewNumberWithRuby (100 + modBy 100 u) ])
    , (600,  \u -> Html.span [] [ viewNumberWithRuby (u // 100), viewNumberWithRuby (100 + modBy 100 u) ])
    , (700,  \u -> Html.span [] [ viewRuby six_pundred_, viewNumberWithRuby (100 + modBy 100 u) ])
    , (800,  \u -> Html.span [] [ viewNumberWithRuby (u // 100), viewNumberWithRuby (100 + modBy 100 u) ])
    , (900,  \u -> Html.span [] [ viewRuby eight_pundred_, viewNumberWithRuby (100 + modBy 100 u) ])
    , (1000, \u -> Html.span [] [ viewNumberWithRuby (u // 100), viewNumberWithRuby (100 + modBy 100 u) ])
    ] (      \_ -> Html.span [] [ Html.text "idk how to count to that" ])

viewCounterRegular : Ruby -> Int -> Html.Html Message
viewCounterRegular repr number =
  Html.div []
    [ viewNumberWithRuby number
    , viewRuby repr
    ]

viewCounterException : Exception -> Ruby -> Int -> Html.Html Message
viewCounterException special repr number =
  Html.div []
    [ viewRuby special ]

--- counters
findCounterByRef : CounterStore -> String -> SearchResult
findCounterByRef store ref =
  case Dict.get ref store.byRef of
    Just it -> Found [ it ]
    Nothing -> NotFound

findCounterByTag : CounterStore -> String -> SearchResult
findCounterByTag store tag =
  case Dict.get tag store.byTag of
    Just it -> Found it
    Nothing -> NotFound

findCounter = findCounterByTag

viewCounter : Counter -> Html.Html Message
viewCounter counter =
  Html.ol [] (
    List.map
      (\n -> Html.li [] [ (
        case Dict.get n counter.cases of
          Just ex -> viewCounterException ex counter.repr n
          Nothing -> viewCounterRegular counter.repr n
      ) ])
      (List.range 1 237)
  )

--- application
type SearchResult
  = WaitingUser
  | NotFound
  | Found (List Counter)

type alias Model =
  { query : String
  , prevQuery : String
  , current : SearchResult
  , counters : CounterStore
  }

type Message
  = UpdateQuery String
  | FindCounter

init : Model
init =
  { query = ""
  , prevQuery = ""
  , current = WaitingUser
  , counters = loadEveryCounters
  }

update : Message -> Model -> Model
update msg model =
  case msg of
    UpdateQuery niw ->
      { model | query = niw }
    FindCounter ->
      { model | current = findCounter model.counters model.query
              , prevQuery = model.query }

view : Model -> Html.Html Message
view model =
  Html.div []
    [ Html.form
        [ Event.onInput UpdateQuery
        , Event.onSubmit FindCounter
        ]
        [ Html.input
            [ Attr.placeholder "enter a query here"
            , Attr.value model.query
            ]
            []
        , Html.button
            []
            [ Html.text "find" ]
        ]
    , Html.div [] (
        case model.current of
          WaitingUser ->
            []
          NotFound ->
            [ Html.hr [] []
            , Html.div [] [ Html.text ("no counter found for " ++ model.prevQuery) ]
            ]
          Found ls ->
            [ Html.hr [] []
            , Html.div [] (List.map viewCounter ls)
            ]
      )
    ]

--- entry point
main : Program () Model Message
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
