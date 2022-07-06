module Main exposing (..)

import Browser
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as Json

--- kanji
type alias Ruby =
  { text : String
  , floating : String
  }

viewRuby : Ruby -> Html.Html Message
viewRuby rb =
  Html.ruby []
    [ Html.text rb.text
    , Html.rp [] [ Html.text "(" ]
    , Html.rt [] [ Html.text rb.floating ]
    , Html.rp [] [ Html.text ")" ]
    ]

numberToKanjiRuby : Int -> Ruby
numberToKanjiRuby number = Ruby "NUM" (String.fromInt number)

viewCounterRegular : Ruby -> Int -> Html.Html Message
viewCounterRegular repr number =
  Html.div []
    [ viewRuby repr
    , viewRuby (numberToKanjiRuby number)
    ]

type alias Exception =
  { number : Int
  , result : Ruby
  }

viewCounterException : Exception -> Ruby -> Int -> Html.Html Message
viewCounterException special repr nummber =
  Html.div []
    [ Html.text "TODO: special cases not implemented yet" ]

--- counters
type alias Counter =
  { repr : {-List-} Ruby -- kanji and reading for the counter
  , tags : List String -- what it may be a counter for
  , cases : List Exception -- special cases / exceptions
  }

findCounter : String -> Counter
findCounter query =
  Counter
    (Ruby "CHA" "nyan")
    [ "x", "y", "z" ]
    []

viewCounter : Counter -> Html.Html Message
viewCounter counter =
  Html.ol []
    [ Html.li [] [ viewCounterRegular counter.repr 1 ]
    , Html.li [] [ viewCounterRegular counter.repr 2 ]
    , Html.li [] [ viewCounterRegular counter.repr 3 ]
    , Html.li [] [ viewCounterRegular counter.repr 4 ]
    , Html.li [] [ viewCounterRegular counter.repr 5 ]
    , Html.li [] [ viewCounterRegular counter.repr 6 ]
    , Html.li [] [ viewCounterRegular counter.repr 7 ]
    , Html.li [] [ viewCounterRegular counter.repr 8 ]
    , Html.li [] [ viewCounterRegular counter.repr 9 ]
    ]

--- application
type alias Model =
  { query : String
  , count : Int -- TODO: Maybe Int
  , countInput : String
  , current : Maybe Counter
  }

init : Model
init =
  { query = ""
  , count = 1
  , countInput = "1"
  , current = Nothing
  }

type Message
  = UpdateQuery String
  | UpdateCount String
  | FindCounter

update : Message -> Model -> Model
update msg model =
  case msg of
    UpdateQuery niw ->
      { model | query = niw }
    UpdateCount may ->
      { model
        | count =
          case String.toInt may of
            Just num -> num
            Nothing  -> -1
        , countInput = may
      }
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
    [ viewInput "enter a query here" model.query UpdateQuery
    , viewInput "coun" model.countInput UpdateCount
    , Html.text (
        if model.count < 0
          then "invalid count"
          else ""
      )
    , Html.button
        [ Event.onMouseDown FindCounter
        , Event.on "keydown" (Json.succeed FindCounter)
        ]
        [ Html.text "find" ]
    , Html.div [] (
        case model.current of
          Just it -> [ Html.hr [] [], viewCounter it ]
          Nothing -> [ Html.text "enter a query above" ]
      )
    ]

--- entry point
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
