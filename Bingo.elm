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
view =
  div [ id "container" ] [ pageHeader, pageFooter ]


main =
  view
