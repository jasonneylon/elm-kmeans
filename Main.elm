import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Html exposing (..)

--- randomPoints : List(Int,Int)
--- randomPoints =
---     Tuple.first (Random.step (Random.list 10 <| Random.pair (int 1 100) (int 1 100)) (Random.initialSeed 10))

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- UPDATE


randomPoints = 
  Random.list 10 (Random.pair (int 1 100) (int 1 100))

type Msg
  = GeneratePoints
  | DrawGraph (List(Int, Int)) 

-- update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GeneratePoints ->
      (model, (Random.generate DrawGraph randomPoints))
      --(model, Cmd.none)
    DrawGraph points ->
      (Model points, Cmd.none)


-- MODEL


type alias Model =
  { points : List(Int, Int)
  }


init : (Model, Cmd Msg)
init =
  (Model [], (Random.generate DrawGraph randomPoints))

-- VIEW

view model =
    svg
    [version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95"]
    (List.map (\p ->  circle [ cx (toString (Tuple.first p)), cy (toString (Tuple.second p)), r "1" ] []) model.points)
                     




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


