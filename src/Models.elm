module Models exposing (..)

import Html exposing (..)
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
  { sources : List Source
  , items : List Item
  , message : String
  , loading : Bool
  , sourceInput : String
  , sourceInputChannel : String
  , now : Maybe Date
  , dialogs : List Dialog
  }

type alias Dialog =
  { title : String
  , content : Html Msg
  }

type Source
  = Reddit String

type Msg
  = FetchSucceed (List Item)
  | FetchFail Http.Error
  | DateSucceed (Date)
  | ShowManage
  | ShowAbout
  | HideDialog
  | SourceInputChange String
  | SourceInputChangeChannel String
  | SourceAdd
  | SourceRemove Source
