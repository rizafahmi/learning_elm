module Bingo where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp
import Signal exposing (Address)

import BingoUtils as Utils

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
    entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextId: Int
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
    ],
    phraseInput = "",
    pointsInput = "",
    nextId = 5
  }

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
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | Add

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

    UpdatePhraseInput contents ->
      { model | phraseInput = contents }

    UpdatePointsInput contents ->
      { model | pointsInput = contents }

    Add ->
      let
          entryToAdd =
            newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextId
          isInvalid model =
            String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
          if isInvalid model
          then model
          else
            {
              model |
                entries = entryToAdd :: model.entries,
                phraseInput = "",
                pointsInput = "",
                nextId = model.nextId + 1
            }

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

entryForm : Address Aksi -> Model -> Html
entryForm address model =
  div [ ]
  [
    input
    [
      type' "text",
      placeholder "Phrase",
      value model.phraseInput,
      name "phrase",
      autofocus True,
      Utils.onInput address UpdatePhraseInput
    ] [],
    input
        [
          type' "number",
          placeholder "Point",
          value model.pointsInput,
          name "point",
          Utils.onInput address UpdatePointsInput
          ] [],
    button [ class "add", onClick address Add ] [ text "Add" ],
    h2 [] [ text ( model.phraseInput ++ " " ++ model.pointsInput ) ]
  ]

view : Address Aksi -> Model -> Html
view address model =
  div [ id "container" ]
        [ pageHeader,
          entryForm address model,
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
