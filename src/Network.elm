module Network exposing (..)

import String exposing (..)
import List exposing (..)
import Json.Decode exposing (..)
import Date exposing (fromTime)
import Http exposing (Error(UnexpectedPayload), get)
import Task exposing (perform)
import Platform.Cmd exposing (batch)

import Models exposing (..)

fetch : String -> Decoder (List Item) -> Cmd Msg
fetch url decode = perform FetchFail FetchSucceed <| get decode url

fetchAll : Cmd Msg
fetchAll = batch
  [ fetchReddit "haskell"
  , fetchReddit "elm"
  ]

fetchReddit : String -> Cmd Msg
fetchReddit sub = fetch ("https://www.reddit.com/r/" ++ sub ++ ".json")
  <| at ["data", "children"]
  <| list
  <| at ["data"]
  <| object4 (\title url permalink created ->
       { name = title
       , url = url
       , commentsUrl = "https://www.reddit.com" ++ permalink
       , date = fromTime <| created * 1000
       , source = "r"
       , channel = sub
       }
     )
     ("title"     := string)
     ("url"       := string)
     ("permalink" := string)
     ("created"   := float)

getErrorMessage : Error -> String
getErrorMessage error = case error of
    UnexpectedPayload text -> text
    otherwise -> "Uknown error"
