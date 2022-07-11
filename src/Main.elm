module Main exposing (..)

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

rubyFromList : List String -> Ruby
rubyFromList list =
  Ruby
    (Maybe.withDefault "ERROR" (List.head list))
    (Maybe.withDefault "error" (List.head (List.drop 1 list)))

rubyDecoder : Json.Decoder Ruby
rubyDecoder =
  Json.map rubyFromList (Json.list Json.string)

filterIntKeys : Dict String a -> Dict Int a
filterIntKeys dict =
  Dict.fromList (
    List.filterMap
      (\(k, v) ->
        case String.toInt k of
          Just kk -> Just (kk, v)
          Nothing -> Nothing
      )
      (Dict.toList dict)
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

loadCounter : String -> Counter
loadCounter ref =
  Counter (Ruby "" "") [] Dict.empty

--- kanji
viewRuby : Ruby -> Html.Html Message
viewRuby rb =
  Html.ruby []
    [ Html.text rb.text
    , Html.rp [] [ Html.text "(" ]
    , Html.rt [] [ Html.text rb.floating ]
    , Html.rp [] [ Html.text ")" ]
    ]

qhundo = Ruby "百" "ひゃく" -- XXX/FIXME: some **will** be straight up wrong (eg. 300, 600, 800)
qten = Ruby "十" "じゅう"
qonetonine =
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
    , qten
    ]
viewNumberWithRuby : Int -> Html.Html Message
viewNumberWithRuby number =
  if number < 11
    then case Array.get (number-1) qonetonine of
      Just it -> viewRuby it
      Nothing -> Html.span [] [ Html.text "unreachable" ]
    else if number < 20
      then Html.span [] [ viewRuby qten, viewNumberWithRuby (modBy 10 number) ]
      else if number < 100
        then Html.span [] [ viewNumberWithRuby (number // 10), viewNumberWithRuby (10 + modBy 10 number) ]
        else if 100 == number
          then viewRuby qhundo
          else if number < 200
            then Html.span [] [ viewRuby qhundo, viewNumberWithRuby (modBy 100 number) ]
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
    [ Html.text "special cases not implemented yet" ]

--- counters
-- PLHO
kaka : Dict Int Exception
kaka = Dict.fromList [ (3, Ruby "XHA" "nyom") ]
-- TODO
findCounter : String -> Counter
findCounter query =
  Counter
    (Ruby "CHA" "nyan")
    [ "x", "y", "z" ]
    kaka

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
type alias Model =
  { query : String
  , current : Maybe Counter
  }

init : Model
init =
  { query = ""
  , current = Nothing
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
      { model | current = Just (findCounter model.query) }

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
    [ Html.p [] [ Html.text ("embedded: " ++ Debug.toString Emb.all) ]
    , viewInput "enter a query here" model.query UpdateQuery
    , Html.button
        [ Event.onMouseDown FindCounter
        , Event.on "keydown" (Json.succeed FindCounter) -- FIXME: only trigger for " " and "\r" or something
        ]
        [ Html.text "find" ]
    , Html.div [] (
        case model.current of
          Just it -> [ Html.hr [] [], viewCounter it ]
          Nothing -> []
      )
    ]

--- entry point
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
