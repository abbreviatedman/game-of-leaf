import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }





one : Bool
one = False
two : Bool
two = True
three : Bool
three = False
four : Bool
four = True
five : Bool
five = False
six : Bool
six = True
seven : Bool
seven = False
eight : Bool
eight = True
nine : Bool
nine = False

pointsList : Array Bool
pointsList = Array.fromList [one, two, three, four, five, six, seven, eight, nine]


type alias Model = {
  points : Array Bool
}

model : Model
model =
  Model pointsList



-- UPDATE
type Msg = Step | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    Step -> log "Model" (evaluateGrid model model 8)
    NoOp -> model


evaluateGrid : Model -> Model -> Int -> Model
evaluateGrid oldModel newModel gridPosition =
  if gridPosition < 1 then
    newModel
  else if gridPosition == 8 then
    evaluateGrid oldModel newModel (gridPosition - 1)
  else
    let
      cellStatus = Array.get gridPosition oldModel.points
      tooManySimilarNeighbors = (Array.get (gridPosition - 1) oldModel.points == Array.get (gridPosition + 1) oldModel.points)
    in
      case tooManySimilarNeighbors of
        True ->
          evaluateGrid oldModel {newModel | points = (Array.set gridPosition (cellStatus == Just False) newModel.points)} (gridPosition - 1)
        False ->
          evaluateGrid oldModel newModel (gridPosition - 1)






-- VIEW
view : Model -> Html Msg
view model =
  div [style styles.div]
    [table [style styles.board]
      [
        tr []
          [ td [style (chooseColor (Array.get 0 model.points))] [text " "]
          , td [style (chooseColor (Array.get 1 model.points))] [text " "]
          , td [style (chooseColor (Array.get 2 model.points))] [text " "]
          ],
        tr []
          [ td [style (chooseColor (Array.get 3 model.points))] [text " "]
          , td [style (chooseColor (Array.get 4 model.points))] [text " "]
          , td [style (chooseColor (Array.get 5 model.points))] [text " "]
          ],
        tr []
          [ td [style (chooseColor (Array.get 6 model.points))] [text " "]
          , td [style (chooseColor (Array.get 7 model.points))] [text " "]
          , td [style (chooseColor (Array.get 8 model.points))] [text " "]
          ]
      ],
    button [class "btn btn-success", style styles.controlPanel, onClick Step] [span [] [text "Step"]]
    ]

chooseColor : Maybe Bool -> List (String, String)
chooseColor cellStatus =
  if cellStatus == Just True then -- could be we could get rid of Just
    styles.tdAlive
  else
    styles.tdDead



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
