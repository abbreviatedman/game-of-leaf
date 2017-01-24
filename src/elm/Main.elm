import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Point = {
  y : Int, x : Int, cellStatus : Int
}

type alias Model = {
  points: Array Point
}

model : Model
model =
  Model (Array.initialize 100 (\index -> { y = (index // 10), x = (index % 10), cellStatus = ((index // 3) % 2)}))


-- UPDATE
type Msg = NoOp | Step | ToggleCell Point

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
    ToggleCell point ->
      let
        x = point.x
        y = point.y
        newPoint = {point | cellStatus = (abs (point.cellStatus - 1))}
      in
        log "model in toggle" {model | points = (Array.set (x + y) newPoint model.points)}
    Step ->
      {model | points = (Array.map updateCell model.points)}

updateCell : Point -> Point
updateCell point =
  let
    logSquare = log "square" (point.x + (point.y * 10))
    numNeighbors = log "numNeighbors" (countNeighbors point)
    alive = log "alive?" (point.cellStatus == 1)
  in
    if alive then
      if numNeighbors < 2 || numNeighbors > 3 then
        {point | cellStatus = 0}
      else
        point
    else
      if numNeighbors == 3 then
        {point | cellStatus = 1}
      else
        point

countNeighbors : Point -> Int
countNeighbors point =
  let
    ul = log "ul" (getNeighbor (point.x - 1) (point.y - 1))
    u = log "u" (getNeighbor (point.x) (point.y - 1))
    ur = log "ur" (getNeighbor (point.x + 1) (point.y - 1))
    l = log "l" (getNeighbor (point.x - 1) (point.y))
    r = log "r" (getNeighbor (point.x + 1) (point.y))
    dl = log "dl" (getNeighbor (point.x - 1) (point.y + 1))
    d = log "d" (getNeighbor (point.x) (point.y + 1))
    dr = log "dr" (getNeighbor (point.x + 1) (point.y + 1))
  in
    ul + u + ur + l + r + dl + d + dr

getNeighbor : Int-> Int -> Int
getNeighbor x y =
  let
    neighborLocation = log "neighborLocation" ((handleEdges x) + ((handleEdges y) * 10))
  in
    .cellStatus (Maybe.withDefault {x = 0, y = 0, cellStatus = 100} (Array.get (neighborLocation) model.points))

handleEdges : Int -> Int
handleEdges coord =
  if coord < 0 then
    coord + 10
  else if coord > 9 then
    coord - 10
  else
    coord

-- VIEW
view : Model -> Html Msg
view model =
  let
    firstsInEachRow = Array.filter isFirstInRow model.points
  in
    {--}div [style styles.div]
      [table [style styles.board]
        (Array.toList (Array.map (\firstPoint -> tr [] (Array.toList (Array.map (\point ->
          if point.cellStatus == 1 then
            td [style styles.tdAlive] [button [onClick (ToggleCell point)] [text " "]]
          else
            td [style styles.tdDead] [button [onClick (ToggleCell point)] [text " "]]) ((Array.filter (\point -> point.y == firstPoint.y) model.points))))) firstsInEachRow)),
      button [class "btn btn-success", style styles.controlPanel, onClick Step] [span [] [text "Step"]]
      ]
      --}
 {--   div [style styles.div]
    [button [class "btn btn-success", style styles.controlPanel, onClick Step] [span [] [text "Step"]]]
    --}

isFirstInRow : Point -> Bool
isFirstInRow point =
  point.x == 0


chooseColor : Point -> Html Msg
chooseColor point =
  if point.cellStatus == 1 then
    td [style styles.tdAlive] [button [onClick (ToggleCell point)] [text " "]]
  else
    td [style styles.tdDead] [button [onClick (ToggleCell point)] [text " "]]



-- CSS STYLES
styles : { div : List (String, String), board : List (String, String), tdAlive : List (String, String), tdDead : List (String, String), controlPanel : List (String, String)}
styles =
  {
    div =
      [ ("padding", "10px")
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
      ]
  }
