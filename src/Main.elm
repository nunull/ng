module NewsAggregator exposing (..)

import Html.App exposing (program)
import ListView exposing (..)

main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


