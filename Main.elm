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
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x - 1) + (boardRowsAndColumns * (handleEdges (point.y - 1)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x) + (boardRowsAndColumns * (handleEdges (point.y - 1)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x + 1) + (boardRowsAndColumns * (handleEdges (point.y - 1)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x - 1) + (boardRowsAndColumns * (handleEdges (point.y)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x + 1) + (boardRowsAndColumns * (handleEdges (point.y)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x - 1) + (boardRowsAndColumns * (handleEdges (point.y + 1)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x) + (boardRowsAndColumns * (handleEdges (point.y + 1)))) points))) +
    (.cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 0} (Array.get (handleEdges (point.x + 1) + (boardRowsAndColumns * (handleEdges (point.y + 1)))) points)))) points
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
      [p [] [text "Game of Leaf"]
      ,table [class "board"]
        (Array.toList (Array.map (\firstPoint -> tr [] (Array.toList (Array.map (\point ->
          if point.cellStatus == 1 then
            td [class "tdAlive"] [button [onClick (ToggleCell point)] [text " "]]
          else
            td [class "tdDead"] [button [onClick (ToggleCell point)] [text " "]]) (Array.filter (\point -> point.y == firstPoint.y) oldModel.points)))) firstsInEachRow)),
      button [class "btn btn-success", onClick Step] [span [] [text "Step"]]
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



{- CSS STYLES
styles : { div : List (String, String), board : List (String, String), tdAlive : List (String, String), tdDead : List (String, String), controlPanel : List (String, String), button : List (String, String)}
styles =
  {
    div =
      [ ("padding", "10px")
      , ("text-align", "center")
      , ("margin", "auto")
      ],
    board =
      [ ( "margin",  "0 auto")
      ],
    tdAlive =
      [ ("border", "1px solid #ddd")
      , ("width", "18px")
      , ("height", "18px")
      , ("background-color", "#8CC8DD")
      ],
    tdDead =
      [ ("border", "1px solid #ddd")
      , ("width", "18px")
      , ("height", "18px")
      , ("background-color", "#fff")
      ],
    controlPanel =
      [ ("margin", "10px auto")
      , ("text-align", "center")
      , ("align", "center")
      ],
    button =
      [ ("height", "30px")
      , ("width", "30px")
      , ("margin", "10px auto")
      , ("text-align", "center")
      ]
  }
-}


