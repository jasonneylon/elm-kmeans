import Svg exposing (circle, svg, Svg)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Array exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, type_)
import Time exposing (Time, second)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- UPDATE

type alias Point = { x:Int, y:Int }
type alias Color = String

type Msg
  = DrawPoints (List Point)
  | PickCentroidsAndRunKmeans 
  | RunKmeans (List Point)
  | NextRunTick Time
  | ChangeK String
  | ResetModel

randomPoints : Generator (List(Point))
randomPoints = 
  Random.list 200 (Random.map (\(x, y) -> Point x y) (Random.pair (int 1 999) (int 1 399)))

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

mean: List Int -> Int
mean numbers =
  (numbers |> List.sum |> toFloat) / (numbers |> List.length |> toFloat) |> round

meanPoint : List Point -> Point
meanPoint points = 
  { x = (List.map .x points) |> mean, y = (List.map .y points) |> mean } 

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

pointsFromClusters : List Cluster -> List (List Point)
pointsFromClusters clusters =
  List.map (\c -> c.points) clusters
  
clustersFromCentroids : List Point -> List Cluster
clustersFromCentroids centroids = 
  List.map2 Cluster centroids (List.repeat (List.length centroids) [])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ResetModel ->
        initialModel
    DrawPoints points ->
      ({model| points = points }, Cmd.none) 
    ChangeK newk ->
      ({model| k = (Result.withDefault model.k (String.toInt newk)) }, Cmd.none) 
    PickCentroidsAndRunKmeans ->
      (model, (Random.generate RunKmeans (randomElements model.k model.points)))
    RunKmeans centroids ->
      ({model | iterations = 0, clusters = clustersFromCentroids centroids, running = True }, Cmd.none)
    NextRunTick time ->
      if model.running && model.iterations < 10 then
        ({model | clusters = (runKmeans model.points model.clusters model.iterations), iterations = model.iterations + 1}, Cmd.none)
      else
        (model,  Cmd.none)


updateClusters : List Point -> List Cluster -> List Cluster
updateClusters points clusters =
  let
    newCentroids = (List.map meanPoint (pointsFromClusters clusters))
  in 
    (groupIntoClusters points newCentroids) 

runKmeans : List Point -> List Cluster -> Int -> List Cluster
runKmeans points clusters iteration = 
  if iteration == 0 then
     groupIntoClusters points (List.map .centroid clusters)
   else
     updateClusters points clusters

-- MODEL

type alias Cluster = 
  { centroid : Point
  , points : List(Point)
  }

type alias Model =
  { points : List(Point)
  , clusters : List(Cluster)
  , k : Int
  , iterations : Int
  , running : Bool
  }

initialModel : (Model, Cmd Msg)
initialModel =
  (Model [] [] 6 0 False, (Random.generate DrawPoints randomPoints))

init : (Model, Cmd Msg)
init =
  initialModel

-- VIEW

view model =
    Html.div []
      [
        Html.h1 [] [Html.text "K-means in Elm"]
      , (svg
        [version "1.1", x "0", y "0", viewBox "0 0 1000 400"]
        (renderGraph model.points model.clusters))
      , Html.fieldset [] [
          Html.label [] [Html.text "K value: "]
        , Html.input [Html.Attributes.type_ "number", value (toString model.k), onInput ChangeK] []
        , Html.button [onClick PickCentroidsAndRunKmeans] [Html.text "Run k-means"]
        , Html.button [onClick ResetModel] [Html.text "Reset"]
        , span [] [Html.text (" Iteration: " ++ toString (model.iterations))]]
      ]

renderGraph points clusters = 
    renderPoints points ++ renderClusters clusters ++  renderCentroids clusters 

renderCentroids : List(Cluster) -> List (Svg msg)
renderCentroids clusters =
  List.map2 (\color cluster -> renderPoint color 4 cluster.centroid) clusterColors clusters

renderPoints : List(Point) -> List (Svg msg)
renderPoints points = 
  List.map (renderPoint "black" 2) points
 
renderClusters : List(Cluster) -> List (Svg msg)
renderClusters clusters =
  List.concat (List.map2 (\color cluster -> List.map (renderPoint color 2) cluster.points) clusterColors clusters)

clusterColors : List Color
clusterColors =
  ["red", "range", "yellow", "green", "purple", "brown"]

renderPoint : Color -> Int -> Point -> Svg msg
renderPoint color size p =
 circle [ cx (toString p.x), cy (toString p.y), r (toString size), fill color ] []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second NextRunTick
