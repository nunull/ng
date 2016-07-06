module Models exposing (..)

import Date exposing(..)
import String exposing (..)
import Http

type alias Item =
  { name : String
  , url : String
  , commentsUrl : String
  , date : Date
  , source : String
  , channel : String
  }

type alias Model =
  { items : List Item
  , message : String
  , loading : Bool
  }

type Msg
  = FetchSucceed (List Item)
  | FetchFail Http.Error
