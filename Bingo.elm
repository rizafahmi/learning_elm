module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String exposing (toUpper, repeat, trimRight)

title message times =
  message ++ " "
  |> toUpper
  |> repeat times
  |> trimRight
  |> text

greet name colorPrimary colorSecondary colorTri =
  name ++ "'s favorites are: " ++ colorPrimary ++ " " ++ colorSecondary ++ " " ++ colorTri

pageHeader =
  h1 [] [ title "Bingo!" 3 ]

pageFooter =
  footer [] [
            a [ href "http://citizenlab.co", target "_blank" ]
              [ text "CitizenLab" ]
           ]

listItem phrase point =
  li [ ] [
        span [ class "phrase" ] [ text phrase ],
        span [ class "point" ] [ text ( toString point ) ]
       ]
listEntries =
  ul [ ] [
        listItem "Future Proof" 100,
        listItem "Doing Agile" 200
       ]

view =
  div [ id "container" ] [ pageHeader, listEntries, pageFooter ]


main =
  view
