module GameOfLife exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)

main : Program Never Model Msg
main = Html.beginnerProgram {model = model, view = view, update = update}

type alias Point = {
  y : Int, x : Int, cellStatus : Int
}

type alias Model = {
  points: Array Point
}

boardRowsAndColumns : Int
boardRowsAndColumns = 20


model : Model
model =
  Model (Array.initialize (boardRowsAndColumns * boardRowsAndColumns) (\index -> { y = (index // boardRowsAndColumns), x = (index % boardRowsAndColumns), cellStatus = 0}))


-- UPDATE
type Msg = NoOp | Step | ToggleCell Point

update : Msg -> Model -> Model
update msg oldModel =
  case msg of
    NoOp -> oldModel
    ToggleCell point ->
      let
        x = point.x
        y = point.y
        newPoint = {point | cellStatus = (abs (point.cellStatus - 1))}
      in
        {oldModel | points = (Array.set (x + (boardRowsAndColumns * y)) newPoint oldModel.points)}
    Step ->
      {oldModel | points = (step oldModel.points)}

step : Array Point -> Array Point
step points =
  let numNeighbors = Array.map (\point ->
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x - 1) + (boardRowsAndColumns * (handleEdges (point.y - 1)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x) + (boardRowsAndColumns * (handleEdges (point.y - 1)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x + 1) + (boardRowsAndColumns * (handleEdges (point.y - 1)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x - 1) + (boardRowsAndColumns * (handleEdges (point.y)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x + 1) + (boardRowsAndColumns * (handleEdges (point.y)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x - 1) + (boardRowsAndColumns * (handleEdges (point.y + 1)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x) + (boardRowsAndColumns * (handleEdges (point.y + 1)))) points))) +
    (.cellStatus (Maybe.withDefault defaultPoint (Array.get (handleEdges (point.x + 1) + (boardRowsAndColumns * (handleEdges (point.y + 1)))) points)))) points
  in
    Array.indexedMap (\index point ->
      let
        thisOnesNeighbors = (Maybe.withDefault 0 (Array.get index numNeighbors))
      in
        if isAlive point then
          if thisOnesNeighbors < 2 || thisOnesNeighbors > 3 then
            {point | cellStatus = 0}
          else
            point
        else
          if thisOnesNeighbors == 3 then
            {point | cellStatus = 1}
          else
            point
    ) points

defaultPoint : Point
defaultPoint =
  {x = 0, y = 0, cellStatus = 0}


isAlive : Point -> Bool
isAlive point =
  point.cellStatus == 1

handleEdges : Int -> Int
handleEdges coord =
  if coord < 0 then
    (coord + boardRowsAndColumns)
  else if coord >= boardRowsAndColumns then
    (coord - boardRowsAndColumns)
  else
    coord

-- VIEW
view : Model -> Html Msg
view oldModel =
  let
    firstsInEachRow = Array.filter isFirstInRow oldModel.points
  in
    div []
      [ p [] [h1 [] [text "Game of Leaf"]]
      , p [] [h3 [] [text "by Colin Jaffe"]]
      , p [] [text "Conway's Game of Life implemented in Elm."]
      , hr [] []
      , table [class "board"]
        (Array.toList (Array.map (\firstPoint -> tr [] (Array.toList (Array.map (\point ->
          if point.cellStatus == 1 then
            td [class "tdAlive", onClick (ToggleCell point)] [img [src "elm-logo-blue.svg", style [("display", "block")]] []]
          else
            td [class "tdDead", onClick (ToggleCell point)] []) (Array.filter (\point -> point.y == firstPoint.y) oldModel.points)))) firstsInEachRow))
      , button [class "myButton", onClick Step] [span [] [text "Step"]]
      ]



isFirstInRow : Point -> Bool
isFirstInRow point =
  point.x == 0


chooseColor : Point -> Html Msg
chooseColor point =
  if point.cellStatus == 1 then
    td [class "tdAlive"] [button [onClick (ToggleCell point)] [text " "]]
  else
    td [class "tdDead"] [button [onClick (ToggleCell point)] [text " "]]

