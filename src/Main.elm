module Main exposing (..)

import Browser
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as Json

--- counters
type alias Case =
  { for : Int
  }
type alias Counter =
  { char : String
  , tags : List String
  , cases : List Case
  }

-- highly temporary obvs
initCounter : String -> List String -> Counter
initCounter char tags =
  { char = char
  , tags = tags
  , cases = []
  }

findCounter q =
  initCounter "A" [ "a", "1", q ]

viewCounter it =
  Html.text it.char

--- application
type alias Model =
  { query : String
  , count : Int
  , countInput : String
  , current : Maybe Counter
  }
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

viewInput ph val msg =
  Html.input
        [ Attr.placeholder ph
        , Attr.value val
        , Event.onInput msg
        ]
        []

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
