import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


type alias Point = {
  y : Int, x : Int, cellStatus : Bool
}

type alias Model = {
  points: Array Point
}

model : Model
model =
  Model (Array.initialize 64 (\index -> { y = ((identity (index / 1)) / 8), x = (rem (identity (index / 1) 8)), cellStatus = False}))


-- UPDATE
type Msg = NoOp | ToggleCell

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
 {--   ToggleCell rowIndex cellIndex cellStatus ->
      {model | points = (Array.set rowIndex (Array.set cellIndex (not cellStatus) (Array.get rowIndex model.points)) model.points)}
    Step ->
      Array.indexedMap updateRow model.points

updateRow : Int -> Array Bool -> Array Bool
updateRow rowIndex row =


updateCell : Int -> Bool -> Bool
updateCell cellIndex cellStatus =

  --}





-- VIEW
view : Model -> Html Msg
view model =
  div [style styles.div]
    [table [style styles.board]
      (Array.toList (Array.indexedMap (renderRow) (Array.filter isFirstInRow model.points))),
    button [class "btn btn-success", style styles.controlPanel] [span [] [text "Step"]]
    ]

{--

renderRow : Int -> Bool -> Html Msg
renderRow rowIndex row =
  tr [] (Array.toList (Array.indexedMap renderCell row))

renderCell : Int -> Point -> Html Msg
renderCell cellIndex cell =
  let
    firstsInEachRow = Array.filter isFirstInRow model.points
  in
    Array.indexedMap renderRow firstsInEachRow




  td [style chooseColor cellStatus] [text " "]
 next step is to add button
 next after that: button onClick toggle

 button [onClick ToggleCell rowIndex cellIndex cellStatus] [text " "]

 --}

isFirstInRow : Point -> Bool
isFirstInRow point =
  point.x == 0


renderRow : Int -> Point -> Html Msg
renderRow rowIndex firstsInEachRow =
  let
    rowArray = (Array.filter (\point -> point.y == rowIndex) model.points)
  in
    tr [] (Array.toList (Array.map chooseColor rowArray))


chooseColor : Point -> Html Msg
chooseColor point =
  if point.cellStatus == True then
    td [style styles.tdAlive] [text " "]
  else
    td [style styles.tdDead] [text " "]



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
