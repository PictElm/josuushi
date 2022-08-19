module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Json
import Task exposing (Task)

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

type alias CounterIndex =
  { byRef : Dict String (List String) -- ref -> tags
  , byTag : Dict String (List String) -- tag -> refs
  }

rubyCat : Ruby -> Ruby -> Ruby
rubyCat a b =
  Ruby
    (a.text ++ b.text)
    (a.floating ++ b.floating)

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
    tags

counterIndex : CounterIndex
counterIndex =
  let
    pairs =
      [ ("nichi", ["day", "days", "time"])
      , ("ko", ["place holder", "plho", "da"])
      ]
  in
    { byRef = Dict.fromList pairs
    , byTag = List.foldr insertByTags Dict.empty pairs
    }

viewRuby : Ruby -> Html.Html Message
viewRuby rb =
  Html.span []
    <| List.map
      (\it ->
        Html.ruby []
          [ Html.text rb.text
          , Html.rp [] [ Html.text "(" ]
          , Html.rt [] [ Html.text it ]
          , Html.rp [] [ Html.text ")" ]
          ]
      )
      (String.split "/" rb.floating)

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

hundred_ : Ruby
hundred_ = Ruby "百" "ひゃく"
three_bundred_ : Ruby
three_bundred_ = Ruby "三百" "さんびゃく"
six_pundred_ : Ruby
six_pundred_ = Ruby "六百" "ろっぴゃく"
eight_pundred_ : Ruby
eight_pundred_ = Ruby "八百" "はっぴゃく"
ten_ : Ruby
ten_ = Ruby "十" "じゅう"
one_ten_ : Array Ruby
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
gatherNumberWithRuby : Int -> Ruby -> Ruby -- into a list of
gatherNumberWithRuby number acc =
  ifBelow number
    [ ( 11,   \u -> Array.get (u - 1) one_ten_ |> Maybe.withDefault (Ruby "error" "unreachable") )
    , ( 20,   \u -> rubyCat ten_                                  (gatherNumberWithRuby (modBy 10 u) acc) )
    , ( 100,  \u -> rubyCat (gatherNumberWithRuby (u // 10) acc)  (gatherNumberWithRuby (10 + modBy 10 u) acc) )
    , ( 101,  \_ -> hundred_ )
    , ( 200,  \u -> rubyCat hundred_                              (gatherNumberWithRuby (modBy 100 u) acc) )
    , ( 300,  \u -> rubyCat (gatherNumberWithRuby (u // 100) acc) (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    , ( 400,  \u -> rubyCat three_bundred_                        (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    , ( 600,  \u -> rubyCat (gatherNumberWithRuby (u // 100) acc) (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    , ( 700,  \u -> rubyCat six_pundred_                          (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    , ( 800,  \u -> rubyCat (gatherNumberWithRuby (u // 100) acc) (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    , ( 900,  \u -> rubyCat eight_pundred_                        (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    , ( 1000, \u -> rubyCat (gatherNumberWithRuby (u // 100) acc) (gatherNumberWithRuby (100 + modBy 100 u) acc) )
    ] (       \_ -> Ruby "error" "number too big")

validateNumber : String -> Maybe Int
validateNumber = String.toInt

counterToRubyException : Exception -> Ruby
counterToRubyException special =
  special

counterToRubyRegular : Ruby -> Int -> Ruby
counterToRubyRegular repr number =
  (rubyCat (gatherNumberWithRuby number (Ruby "" "")) repr)

counterToRuby : Counter -> Int -> Ruby
counterToRuby counter n =
  case Dict.get n counter.cases of
    Just ex -> counterToRubyException ex
    Nothing -> counterToRubyRegular counter.repr n

processQuery : CounterIndex -> String -> (Maybe (String, Int), Maybe (List String))
processQuery index query =
  let
    num_pair = Nothing -- TODO: distinguish some kind of number in the query
    tag = query -- TODO: potentially multiple tags, AND? OR?
  in
    ( num_pair
    , Dict.get tag index.byTag
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

viewResult : Maybe Int -> Counter -> Html.Html Message
viewResult num counter =
  Html.div
    [ Attr.class "result" ]
    [ Html.div
      [ Attr.class "repr" ]
      [ viewRuby counter.repr ]
    , Html.div
      [ Attr.class "info" ]
      [ Html.div
        []
        [ Html.span
          [ Attr.class "tags" ]
          [ Html.text (String.join ", " counter.tags) ]
        , Html.span
          [ Attr.class "computed" ]
          [ case num of
            Just n  -> viewRuby <| counterToRuby counter n
            Nothing -> Html.text "enter a number"
          ]
        ]
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
                  let
                    (class, ruby) = case Dict.get n counter.cases of
                      Just ex -> ("exception", counterToRubyException ex)
                      Nothing -> ("regular", counterToRubyRegular counter.repr n)
                  in
                    Html.tr
                      [ Attr.class class ]
                      [ Html.td [] [ Html.text (String.fromInt n) ]
                      , Html.td [] [ Html.text ruby.text ]
                      , Html.td [] [ Html.text ruby.floating ]
                      ]
                )
                (List.range 1 10 ++ List.filter (\n -> 10 < n) (Dict.keys counter.cases))
            ] -- table
          ] -- summary>div
        ] -- summary
      ] -- .info
    ] -- .repr

viewHomePage : Html.Html Message
viewHomePage =
  Html.div
    [ Attr.class "page-home" ]
    [ Html.text "HomePage" ]

viewLoading : Html.Html Message
viewLoading =
  Html.div
    [ Attr.class "page-loading" ]
    [ Html.text "Loading" ]

viewNotFound : Html.Html Message
viewNotFound =
  Html.div
    [ Attr.class "page-not-found" ]
    [ Html.text "NotFound" ]

viewFound : Model -> List Counter -> Html.Html Message
viewFound model list =
  Html.div
    [ Attr.class "page-found" ]
    [ Html.input
      [ Attr.class "number-input"
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
    ] -- .number-input

--- application
type Page
  = HomePage
  | Loading
  | NotFound
  | Found (List Counter)

type alias Model =
  { query : String
  , numberInput : String
  , number : Maybe Int
  , current : Page
  , counters : CounterIndex
  }

type Message
  = UpdateQuery String
  | UpdateNumber String
  | SearchCounter
  | GotResult (Result Http.Error (List Counter))

init : () -> (Model, Cmd Message)
init _ =
  ( { query = ""
    , numberInput = ""
    , number = Nothing
    , current = HomePage
    , counters = counterIndex
    }
  , Cmd.none
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
        , number = validateNumber niw
        }
      , Cmd.none
      )
    SearchCounter ->
      let
        (number, match) = processQuery model.counters model.query
        model_wnum = case number of
          Just (str, num) ->
            { model
            | numberInput = str
            , number = Just num
            }
          Nothing -> model
      in case match of
        Just list ->
          ( { model_wnum
            | current = Loading
            }
          , list
            |> List.map refToRequestTask -- List (Task Http.Error Counter)
            |> Task.sequence -- Task Http.Error (List Counter)
            |> Task.attempt GotResult -- Cmd Message
          )
        Nothing ->
          ( { model
            | current = NotFound
            }
          , Cmd.none
          )
    GotResult res ->
      case res of
        Ok list ->
          ( { model
            | current = Found list
            }
          , Cmd.none
          )
        Err _ ->
          ( { model
            | current = NotFound
            }
          , Cmd.none
          )

view : Model -> Html.Html Message
view model =
  Html.div
    [ Attr.id "#root" ]
    [ Html.header
      []
      [ Html.text "head" ]
    , Html.form
      [ Attr.id "search-bar"
      , Attr.autofocus True
      , Event.onInput UpdateQuery
      , Event.onSubmit SearchCounter
      ]
      [ Html.input
        [ Attr.placeholder "counter for..."
        , Attr.value model.query
        ]
        []
      , Html.button
        []
        [ Html.text "find" ]
      ] -- #search-bar
    , Html.div
      [ Attr.id "page-content" ]
      [ case model.current of
        HomePage   -> viewHomePage
        Loading    -> viewLoading
        NotFound   -> viewNotFound
        Found list -> viewFound model list
      ] -- #page-content
    , Html.footer
      []
      [ Html.text "foot" ]
    ] -- #root

--- entry point
main : Program () Model Message
main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
