module Main exposing (..)

import Array
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

type alias Exception = Ruby

viewCounterException : Exception -> Ruby -> Int -> Html.Html Message
viewCounterException special repr number =
  Html.div []
    [ Html.text "TODO: special cases not implemented yet" ]

--- counters
type alias Counter =
  { repr : {-List-} Ruby -- kanji and reading for the counter
  , tags : List String -- what it may be a counter for
  , cases : List (Maybe Exception) -- special cases / exceptions
  }

findCounter : String -> Counter
findCounter query =
  Counter
    (Ruby "CHA" "nyan")
    [ "x", "y", "z" ]
    [ Just (Ruby "XHA" "nyom")
    ]

viewCounter : Counter -> Html.Html Message
viewCounter counter =
  let asArray = Array.fromList counter.cases in -- TODO: not use array[?]
  Html.ol [] (
    List.map
      (\n -> Html.li [] [ (
        case Array.get (n-1) asArray of
          Just (Just ex) -> viewCounterException ex counter.repr n
          Just Nothing -> viewCounterRegular counter.repr n
          Nothing -> viewCounterRegular counter.repr n
      ) ])
      [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
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
    [ viewInput "enter a query here" model.query UpdateQuery
    , Html.button
        [ Event.onMouseDown FindCounter
        , Event.on "keydown" (Json.succeed FindCounter)
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
