module Bingo where

import Html
import String

title message times =
  message ++ " "
  |> String.toUpper
  |> String.repeat times
  |> String.trimRight
  |> Html.text

greet name colorPrimary colorSecondary colorTri =
  name ++ "'s favorites are: " ++ colorPrimary ++ " " ++ colorSecondary ++ " " ++ colorTri


main =
  greet "Larry" "Blue" "Chocolate" "Lizard" |> Html.text
