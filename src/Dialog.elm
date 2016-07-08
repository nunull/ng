module Dialog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD

import Models exposing (..)

view : Dialog -> Html Msg
view dialog =
  div [ class "dialog" ]
  [ header []
    [ h3 [] [ text dialog.title ] ]
  , dialog.content
  , footer [] [ button [ onClick HideDialog, class "primary" ] [ text "OK" ] ]
  ]

manageDialog : Model -> Dialog
manageDialog model =
  { title = "Manage"
  , content = div []
    [ ul [] <| List.map viewSource model.sources
    , section []
      [ button [ onClick SourceAdd ] [ text "Add" ]
      , select
        [ on "change"
          (JD.map SourceInputChange <| JD.at ["target", "value"] JD.string)
        ]
        [ option [] [ text "Reddit" ]
        ]
      , input [ placeholder "Channel", onInput SourceInputChangeChannel ] []
      ]
    ]
  }

viewSource : Source -> Html Msg
viewSource source =
  case source of
    Reddit channel ->
      li []
        [ button [ onClick <| SourceRemove source ] [ text "Remove" ]
        , span [] [ text <| "Reddit " ++ "(" ++ channel ++ ")" ]
        ]

aboutDialog : Model -> Dialog
aboutDialog model =
  { title = "About"
  , content = div []
    [ p [] [ text about ]
    , section []
      [ b [] [ text "Repository: " ]
      , a
        [ href "https://github.com/nunull/ng", target "_blank" ]
        [ text "github.com/nunull/ng" ]
      , br [] []
      , b [] [ text "Bugtracker: " ]
      , a
        [ href "https://github.com/nunull/ng/issues", target "_blank" ]
        [ text "github.com/nunull/ng/issues" ]
      ]
    ]
  }

about : String
about = """
This is a simple news aggregator, which works entirely client side and doesn't
need authentication.
"""
