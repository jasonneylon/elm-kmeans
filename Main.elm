import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Array exposing (..)
import Maybe exposing (..)
import Html exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- UPDATE

type alias Point = { x:Int, y:Int }

type Msg
  = PickPoints (List Point)
  | DrawGraph (List Point) 

randomPoints : Generator (List(Point))
randomPoints = 
  Random.list 200 (Random.map (\(x, y) -> Point x y) (Random.pair (int 1 999) (int 1 499)))

defaultPoint : Point
defaultPoint =
  Point 0 0

pointByIndex : List Point -> Int -> Point
pointByIndex points index = 
  withDefault defaultPoint (Array.get index (Array.fromList points))

randomElements : Int -> List Point -> Generator (List Point)
randomElements k points =
  let 
    l = List.length points
  in 
    (Random.list k (Random.map (\i -> (pointByIndex points i)) (int 1 l)))

distance : Point -> Point -> Float
distance a b = 
  sqrt (toFloat (((a.x - b.x) ^ 2) + ((a.y - b.y) ^ 2)))

closestCentroid : Point -> List(Point) -> Point
closestCentroid point centroids = 
  let 
    centroidsByDistance = List.map (\c -> let d = (distance point c) in {centroid=c, distance=d}) centroids
  in
    centroidsByDistance
    |> List.sortBy .distance 
    |> List.head
    |> Maybe.withDefault {centroid=defaultPoint, distance=0}
    |> .centroid

groupIntoCentroidAndPoints : List { centroid : Point, point: Point } -> Point -> { centroid : Point, points : List Point }
groupIntoCentroidAndPoints pointsAndClosestCentroid c =
  let 
    points = List.map .point (List.filter (\pc -> pc.centroid == c) pointsAndClosestCentroid)
  in 
    { centroid=c, points=points }

groupIntoClusters : List Point -> List Point -> List Cluster
groupIntoClusters points centroids =
  let 
    pointsAndClosestCentroid = List.map (\p -> { point=p, centroid=(closestCentroid p centroids)}) points
  in
    List.map (groupIntoCentroidAndPoints pointsAndClosestCentroid) centroids

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PickPoints points ->
      ({model| points = points }, (Random.generate DrawGraph (randomElements model.k points)))
    DrawGraph centroids ->
      ({model | centroids = centroids, clusters = (groupIntoClusters model.points centroids) }, Cmd.none)
      -- ({model | centroids = centroids }, Cmd.none)

-- MODEL

type alias Cluster = 
  { centroid : Point
  , points : List(Point)
  }

type alias Model =
  { points : List(Point)
  , centroids : List(Point)
  , clusters : List(Cluster)
  , k : Int
  }

init : (Model, Cmd Msg)
init =
  (Model [] [] [] 4, (Random.generate PickPoints randomPoints))

-- VIEW

view model =
    svg
    [version "1.1", x "0", y "0", viewBox "0 0 1000 500"]
    (List.map (renderPoint model.centroids) model.points)

renderPoint centroids p =
  let 
    color = if List.member p centroids then "red" else "blue"
  in  
    circle [ cx (toString p.x), cy (toString p.y), r "2", fill color ] []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


