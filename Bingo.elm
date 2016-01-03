module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String exposing (toUpper, repeat, trimRight)

-- MODEL

newEntry phrase points id =
  {
    phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }

initialModel =
  { entries = [
     newEntry "Doing Agile" 400 2,
     newEntry "Learn Asana" 200 1,
     newEntry "Procastinate" 350 3
    ]}

-- UPDATE
type Action
  = NoOp
  | Sort

update action model =
  case action of
    NoOp ->
      model
    Sort ->
      { model | entries = List.sortBy .points model.entries }

-- VIEW

title message times =
  message ++ " "
  |> toUpper
  |> repeat times
  |> trimRight
  |> text


pageHeader =
  h1 [] [ title "Bingo!" 3 ]

pageFooter =
  footer [] [
            a [ href "http://citizenlab.co", target "_blank" ]
              [ text "CitizenLab" ]
           ]

listItem entry =
  li [ ] [
        span [ class "phrase" ] [ text entry.phrase ],
        span [ class "point" ] [ text ( toString entry.points ) ]
       ]
listEntries entries =
  ul [ ] ( List.map listItem entries )


view model =
  div [ id "container" ] [ pageHeader, listEntries model.entries, pageFooter ]


main =
  view ( update Sort initialModel )
