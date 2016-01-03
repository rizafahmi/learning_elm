module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String exposing (toUpper, repeat, trimRight)

newEntry phrase point id =
  {
    phrase = phrase,
    point = point,
    wasSpoken = False,
    id = id
  }

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
        span [ class "point" ] [ text ( toString entry.point ) ]
       ]
listEntries =
  ul [ ] [
        listItem ( newEntry "Future Proof" 100 1 ),
        listItem ( newEntry "Doing Agile" 200 2 )
       ]

view =
  div [ id "container" ] [ pageHeader, listEntries, pageFooter ]


main =
  view
