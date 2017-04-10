import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)

randomPoints : List(Int,Int)
randomPoints =
        Tuple.first (Random.step (Random.list 10 <| Random.pair (int 1 100) (int 1 100)) (Random.initialSeed 10))

main =
  let
      ps = randomPoints
  in
     svg
             [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95"
             ]
             (List.map (\p ->  circle [ cx (toString (Tuple.first p)), cy (toString (Tuple.second p)), r "1" ] []) ps)
                     
