open Value
open Grid
open Printf

let testRows = [
  [none;two;none;two];
  [four;two;none;two];
  [four;none;none;none];
  [four;ttf;ttf;two]
]

let testGrid = gridFromRows testRows
let testGridTransposed = gridFromRows (transpose testRows)

let tinyTest desc grid =
  printf "%s:\n" desc ; printGrid grid ; printf "\n\n"

let () =
  printf "\n" ;
  tinyTest "Test grid" testGrid ;
  tinyTest "Transposed test grid" testGridTransposed ;
  tinyTest "Propagate left" (propagateLeft testGrid) ;
  tinyTest "Propagate right" (propagateRight testGrid) ;
  tinyTest "Propagate up" (propagateUp testGrid) ;
  tinyTest "Propagate down" (propagateDown testGrid) ;
  printf "\n" ;
