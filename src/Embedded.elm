module Embedded exposing (all)

import Dict exposing (Dict)

all : Dict String String
all =
  Dict.fromList
--#region this part is edited at build time (TODO: better would be better)
    [ ("nichi", "data/nichi.json")
    , ("ko", "data/ko.json")
    ]
--#endregion
