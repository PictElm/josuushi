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

loadEveryCounters : CounterStore
loadEveryCounters =
  let
    all = Dict.toList Emb.all
      |> List.filterMap loadCounter
  in
    { byRef = Dict.fromList all
    , byTag = Dict.empty
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

hundred_ : Ruby
hundred_ = Ruby "百" "ひゃく" -- XXX/FIXME: some **will** be straight up wrong (eg. 300, 600, 800)
ten_ : Ruby
ten_ = Ruby "十" "じゅう"
one_nine_ : Array.Array Ruby
one_nine_ =
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
-- YYY: maybe could do better
viewNumberWithRuby : Int -> Html.Html Message
viewNumberWithRuby number =
  if number < 11
    then case Array.get (number - 1) one_nine_ of
      Just it -> viewRuby it
      Nothing -> Html.span [] [ Html.text "unreachable" ]
    else if number < 20
      then Html.span [] [ viewRuby ten_, viewNumberWithRuby (modBy 10 number) ]
      else if number < 100
        then Html.span [] [ viewNumberWithRuby (number // 10), viewNumberWithRuby (10 + modBy 10 number) ]
        else if 100 == number
          then viewRuby hundred_
          else if number < 200
            then Html.span [] [ viewRuby hundred_, viewNumberWithRuby (modBy 100 number) ]
            else if number < 1000
              then Html.span [] [ viewNumberWithRuby (number // 100), viewNumberWithRuby (100 + modBy 100 number) ]
              else Html.span [] [ Html.text "idk how to count to that" ]

viewCounterRegular : Ruby -> Int -> Html.Html Message
viewCounterRegular repr number =
  Html.div []
    [ viewNumberWithRuby number
    , viewRuby repr
    ]

-- TODO
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
findCounterByTag store ref =
  NotFound

findCounter = findCounterByRef

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
  , current : SearchResult
  , counters : CounterStore
  }

init : Model
init =
  { query = ""
  , current = WaitingUser
  , counters = loadEveryCounters
  }

type Message
  = UpdateQuery String
  | FindCounter

update : Message -> Model -> Model
update msg model =
  case msg of
    UpdateQuery niw ->
      { model | query = niw }
    FindCounter ->
      { model | current = findCounter model.counters model.query }

viewInput : String -> String -> (String -> Message) -> Html.Html Message
viewInput ph val msg =
  Html.input
        [ Attr.placeholder ph
        , Attr.value val
        , Event.onInput msg
        ]
        []

view : Model -> Html.Html Message
view model =
  Html.div []
    [ Html.p [] [ Html.text ("embedded: " ++ Debug.toString model.counters) ]
    , viewInput "enter a query here" model.query UpdateQuery
    , Html.button
        [ Event.onMouseDown FindCounter
        , Event.on "keydown" (Json.succeed FindCounter) -- FIXME: only trigger for " " and "\r" or something
        ]
        [ Html.text "find" ]
    , Html.div [] (
        case model.current of
          WaitingUser ->
            []
          NotFound ->
            [ Html.hr [] [] -- TODO: text should not update with input, needs to use a `model.previousQuery`
            , Html.div [] [ Html.text ("no counter found for " ++ model.query) ]
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
