module ListView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
import Dialog exposing (..)

init : (Model, Cmd Msg)
init =
  let
    model =
      { items = []
      , message = ""
      , loading = True
      , now = Nothing
      , sourceInput = "Reddit"
      , sourceInputChannel = ""
      , dialogs = []
      , sources = [ Reddit "haskell", Reddit "elm" ]
      }
  in
  ( model
  , fetchAll model.sources
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
  ShowManage ->
    ( { model | dialogs = model.dialogs ++ [ manageDialog model ] }
    , Cmd.none
    )
  ShowAbout ->
    ( { model | dialogs = model.dialogs ++ [ ] }
    , Cmd.none
    )
  HideDialog ->
    ( { model
      | dialogs =
          List.reverse
          <| Maybe.withDefault []
          <| tail
          <| List.reverse model.dialogs
      , items = []
      }
    , fetchAll model.sources
    )
  SourceInputChange name ->
    ( { model | sourceInput = name }
    , Cmd.none
    )
  SourceInputChangeChannel channel ->
    ( { model | sourceInputChannel = channel }
    , Cmd.none
    )
  SourceAdd ->
    let
      source = case model.sourceInput of
        "Reddit" -> Just <| Reddit model.sourceInputChannel
        _ -> Nothing
    in
    case source of
      Just source ->
        ( { model | sources = model.sources ++ [ source ] }
        , Cmd.none
        )
      Nothing -> ( model, Cmd.none )
  SourceRemove source ->
    ( { model | sources = List.filter (\source' -> source' /= source) model.sources }
    , Cmd.none
    )

view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ div
    [ class <| "dialog-container"
      ++ (if List.length model.dialogs > 0 then " active" else "")
    ]
    <| List.map Dialog.view model.dialogs
  ,  header []
    [ h1 [ class "logo" ] [ text "N" ]
    , button [ onClick ShowManage, class "primary" ] [ text "Manage" ]
    , button [ onClick ShowAbout] [ text "About" ]
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
