module Bingo where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp
import Signal exposing (Address)

import String exposing (toUpper, repeat, trimRight)

-- MODEL

type alias Entry =
  {
    phrase: String,
    points: Int,
    id: Int,
    wasSpoken: Bool
  }

type alias Model =
  {
    entries: List Entry
  }

newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  {
    phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }

initialModel : Model
initialModel =
  { entries = [
     newEntry "Doing Agile" 400 2,
     newEntry "Learn Asana" 200 1,
     newEntry "Procastinate" 350 3
    ]}

totalPoints : List Entry -> Int
totalPoints entries =
  entries
    |> List.filter .wasSpoken
    |> List.foldl (\e sum -> sum + e.points) 0

-- UPDATE
type Aksi
  = NoOp
  | Sort
  | Delete Int
  | Mark Int

update : Aksi -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Sort ->
      { model | entries = List.sortBy .points model.entries }
    Delete id ->
      let
          remainingEntries =
            List.filter (\e -> e.id /= id) model.entries
          _ = Debug.log "the remaining entries" remainingEntries
      in
        { model | entries = remainingEntries }
    Mark id ->
      let
          updateEntry e =
            if e.id == id then { e | wasSpoken = (not e.wasSpoken) } else e
      in
          { model | entries = List.map updateEntry model.entries }

-- VIEW

title : String -> Int -> Html
title message times =
  message ++ " "
  |> toUpper
  |> repeat times
  |> trimRight
  |> text


pageHeader : Html
pageHeader =
  h1 [] [ title "Bingo!" 3 ]

pageFooter : Html
pageFooter =
  footer [] [
            a [ href "http://citizenlab.co", target "_blank" ]
              [ text "CitizenLab" ]
           ]

listItem : Address Aksi -> Entry -> Html
listItem address entry =
  li [
    classList [ ( "highlight", entry.wasSpoken ) ],
    onClick address (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text entry.phrase ],
        span [ class "point" ] [ text ( toString entry.points ) ],
        button [ class "delete", onClick address (Delete entry.id) ] [ ]
       ]

listEntries : Address Aksi -> List Entry -> Html
listEntries address entries =
  let
      listItems = List.map (listItem address) entries
      items = listItems ++ [ totalItem ( totalPoints entries ) ]
  in
      ul [ ] items


view : Address Aksi -> Model -> Html
view address model =
  div [ id "container" ]
        [ pageHeader,
          listEntries address model.entries,
          button
          [ class "sort",
            onClick address Sort
          ]
          [ text "sort" ],
          pageFooter
        ]

totalItem : Int -> Html
totalItem total =
  li [ class "total" ]
  [ span [ class "phrase" ] [ text "Total" ] ,
   span [ class "point" ] [ text ( toString total ) ]
  ]


main : Signal Html
main =
  StartApp.start {
              model = initialModel,
              view = view,
              update = update
            }
