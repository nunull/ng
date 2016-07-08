module ListView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)
import String exposing (..)
import List exposing (..)
import Date exposing (..)
import Date.Format
import Time
import Task
import Array

import Network exposing (..)
import Models exposing (..)

init : (Model, Cmd Msg)
init =
  ( { items = [], message = "", loading = True, now = Nothing }
  , fetchAll
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  FetchSucceed items ->
    ( { model
        | items = List.append model.items items
        , message = "", loading = False
      }
    , Cmd.none
    )
  FetchFail error ->
    ( { model | message = getErrorMessage error, loading = False }
    , Cmd.none
    )
  DateSucceed date ->
    ( { model | now = Just date }
    , Cmd.none
    )

view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ header []
    [ h1 [ class "logo" ] [ text "N" ]
    , button [ class "primary" ] [ text "Manage" ]
    , button [] [ text "About" ]
    ]
  , div [ class "loading-indicator", hidden <| not model.loading ] []
  , span [] [ text model.message ]
  , ul []
    <| List.map (viewItem model)
    <| List.reverse
    <| sortBy (\item ->
         Time.inMilliseconds
         <| Date.toTime item.date
       ) model.items
  ]

viewItem : Model -> Item -> Html Msg
viewItem model item =
  li []
    [ h2 [ ] [ a [ href item.url, target "_blank" ] [ text item.name ] ]
    , span
      [ class "source-channel" ]
      [ text <| item.source ++ "/" ++ item.channel]
    , space
    , a [ href item.commentsUrl, target "_blank" ] [ text "Comments" ]
    , space
    , a [ href item.url, target "_blank" ] [ text <| displayUrl item.url ]
    , space
    , time [] [ text <| displayDate model.now item.date ]
    ]

displayUrl : String -> String
displayUrl url =
  let
    protocolSeperator = "://"
    pathSeperator = "/"
  in
     split protocolSeperator url
  |> tail
  |> withDefault [url]
  |> join protocolSeperator
  |> split pathSeperator
  |> head
  |> withDefault url

monthToInt : Month -> Int
monthToInt month =
  case month of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

displayDate : Maybe Date -> Date -> String
displayDate cur date =
  case cur of
    Just cur ->
      let
        fs    = [  year, monthToInt << month,   day,   hour,   minute,   second ]
        names = [ "year",             "month", "day", "hour", "minute", "second" ]
        result
          = Array.get 0 <| Array.fromList
         <| List.map snd <| List.filter (\pair -> fst pair /= 0)
         <| List.map2
            (\num name ->
              ( num
              , toString num ++ " " ++ name ++ (if num > 1 then "s" else "")
              )
            )
            (List.map2
              (-)
              (List.map (\f -> f cur)  fs)
              (List.map (\f -> f date) fs)
            )
            names
      in
      case result of
        Just str -> str
        Nothing -> "now"
    Nothing -> ""

space : Html msg
space = text " "

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
