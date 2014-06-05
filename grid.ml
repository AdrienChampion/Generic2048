open Value

(** Exception raised when trying to insert a random value in a grid
    that is full already. *)
exception CannotInsertRandomValue

(** Type for 2048 grids. *)
type grid = {
  rows: (value list) list; cols: (value list) list
}

(** Reverse version of the transposition of a grid. *)
let transpose grid =
  let rec loop continuation current result grid =
    match (grid,continuation) with
    | ([], _ :: _) ->
      loop [] [] ((List.rev current) :: result) (List.rev continuation)
    | ([], []) -> List.rev result
    | (h :: t, _) -> match h with
      | h' :: t' -> loop (t' :: continuation) (h' :: current) result t
      | [] -> loop continuation current result t
  in
  loop [] [] [] grid

(** Creates a new empty grid. *)
let emptyGrid = {
  rows = [
    [none;none;none;none];
    [none;none;none;none];
    [none;none;none;none];
    [none;none;none;none]
  ];
  cols = [
    [none;none;none;none];
    [none;none;none;none];
    [none;none;none;none];
    [none;none;none;none]
  ]
}

(** Creates a grid from the input (value list) list encoding the rows
    of the grid. *)
let gridFromRows rs = {
  rows = rs ; cols = transpose rs
}

(** Creates a grid from the input (value list) list encoding the
    columns of the grid. *)
let gridFromCols cs = {
  rows = transpose cs ; cols = cs
}

(** Rows --i.e. (value list) list encoding rows-- to string. *)
let toStringRows grid = List.map (List.map Value.toString) grid
(** Grid to string. *)
let toString grid = toStringRows grid.rows

(** Prints a grid with an optional prefix for each line. *)
let printGrid ?p:(prefix="  ") grid = List.iter (
  function row ->
    Printf.printf "%s" prefix ; Printf.printf "| " ;
    List.iter (function s -> Printf.printf "%5s " (s ^ " ")) row ;
    Printf.printf "|\n"
) (toString grid)

(** Propagation to the left for a grid. *)
let propagateLeft grid =
  gridFromRows (List.map propagateLeftRow grid.rows)

(** Propagation to the right for a grid. *)
let propagateRight grid =
  gridFromRows (List.map propagateRightRow grid.rows)

(** Upward propagation for a grid. *)
let propagateUp grid =
  gridFromCols (List.map propagateLeftRow grid.cols)

(** Downward propagation for a grid. *)
let propagateDown grid =
  gridFromCols (List.map propagateRightRow grid.cols)


(** Initializing random. *)
let () = Random.self_init ()

(** Returns a random position between 0 (inclusive) and the number of
    cells on the grid (exclusive). *)
let randomPosition () =
  let gridSize = 16 in
  Random.int gridSize

(** Returns a random power of two (atm either 2 or 4). *)
let randomValue () =
  let maxRandomPower = 2 in
  value ((Random.int maxRandomPower) + 1)

(** Inserts a random power of two at a random "none" cell in the input
    grid. *)
let insertRandom grid =
  let position = randomPosition () in
  let value = randomValue () in
  let rec innerLoop count result row = match (row,count) with
    | (None :: tail,0) ->
      ((List.rev result) @ (value :: tail), -1)
    | (None :: tail,_) ->
      innerLoop (count - 1) (None :: result) tail
    | (head :: tail,_) ->
      innerLoop count (head :: result) tail
    | ([],_) -> (List.rev result, count)
  in
  let rec loop count result = function
    | [] -> loop count [] grid.rows
    | row :: tail -> match innerLoop count [] row with
      | (newRow, -1) -> (List.rev result) @ (newRow :: tail)
      | (_,newCount) -> loop newCount (row :: result) tail
  in
  gridFromRows (loop position [] grid.rows)

(** Creates a new grid with two random tiles. *)
let randomStartGrid = insertRandom (insertRandom emptyGrid)
