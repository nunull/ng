module ListView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)
import String exposing (..)
import List exposing (..)
import Date
import Date.Format
import Time

import Network exposing (..)
import Models exposing (..)

init : (Model, Cmd Msg)
init =
  ( { items = [], message = "", loading = True }
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

view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ header []
    [ h1 [ class "logo" ] [ text "N" ]
    , button [] [ text "Manage" ]
    , button [] [ text "About" ]
    ]
  , div [ class "loading-indicator", hidden <| not model.loading ] []
  , span [] [ text model.message ]
  , ul []
    <| List.map viewItem
    <| List.reverse
    <| sortBy (\item ->
         Time.inMilliseconds
         <| Date.toTime item.date
       ) model.items
  ]

viewItem : Item -> Html Msg
viewItem item =
  li []
    [ h2 [ ]
      [ a
        [ href item.url, target "_blank" ]
        [ text
          <| "(" ++ item.source ++ "/" ++ item.channel ++ ") " ++ item.name ] ]
    , a [ href item.commentsUrl, target "_blank" ] [ text "Comments" ]
    , space
    , a [ href item.url, target "_blank" ] [ text <| displayUrl item.url ]
    , space
    , time [] [ text <| Date.Format.format "%Y-%m-%d %H:%M" item.date ]
    , space
    , span
        [ class "source-channel" ]
        [ text <| item.source ++ "/" ++ item.channel]
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

space : Html msg
space = text " "

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
