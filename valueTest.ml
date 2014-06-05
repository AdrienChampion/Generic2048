open Value
open Printf

(** Prints the results of a test. *)
let testResult s list =
  let total = List.length list in
  let trues = List.length (List.filter (function b -> b) list) in
  let falses = total - trues in
  begin
    printf "Result for %s:\n" s;
    printf
      "  %3i test cases, %3i successful, %3i errors.\n"
      total trues falses
  end

let test description testFun list =
  printf "%s\n" description;
  let resList = List.map testFun list in
  printf "\n";
  resList

let success () = printf "SUCCESS.\n" ; true
let error   () = printf "ERROR.\n" ; false

(** Tests the power of two function from Data. *)
let testPowTwo pair =
  let (n,res) = pair in
  printf "  2^%2i = " n;
  try
    let twoToN = powerOfTwo n in
    printf "%4i " twoToN ;
    if res = twoToN then success () else error ()
  with
  | IllegalPowerOfTwo ->
    printf "exception: illegal power of two.\n" ;
    if n < 0 then success () else error ()

(** Tests the increment function from Data. *)
let testIncrement pair =
  let (n,res) = pair in
  printf "  incrementing %4s -> " (toString n) ;
  try
    let inc = increment n in
    printf "%4s " (toString inc) ;
    if res = inc then success () else error ()
  with
  | TryingToSumNone ->
    printf "exception: trying to sum none.\n" ;
    if n = None then success () else error ()
  | Illegal2048Value ->
    printf "exception: illegal 2048 value.\n" ;
    match n with 
    | None -> success ()
    | TwoTo i when i >= 11 -> success ()
    | _ -> error ()

(** Tests both propagateLeft and propagateRight from data. *)
let testPropagate rows =
  let (row,resLeft,resRight) = rows in
  printf "-> Input row:\n" ; printf "     " ; printRow row ;
  printf "\n   Propagating left:  " ;
  let left = propagateLeftRow row in
  let leftLegal = if left = resLeft then success () else error () in
  printf "     " ; printRow left ;
  printf "\n   Propagating right: " ;
  let right = propagateRightRow row in
  let rightLegal = if right = resRight then success () else error () in
  printf "     " ; printRow right ; printf "\n" ; leftLegal && rightLegal

(** Running the tests. *)
let () = printf "\n" 

let powTwoRes = test "Testing power of two." testPowTwo [
  (0,1); (1,2); (2,4); (3,8); (4,16); (5,32); (6,64);
  (7,128); (8,256); (9,512); (10,1024); (11,2048); (12,4096)
]

let incrementRes = test "Testing increment." testIncrement [
  (two,four); (four,eight); (eight, sixteen); (sixtyFour,hte);
  (hte,thfs); (thfs,fht); (fht,ttf); (ttf,ttfe)
]

let propagateRes = test "Testing propagation functions." testPropagate [
  ([none; two; four; none],
   [two; four; none; none], [none; none; two; four]);
  ([none; two; two; sixteen],
   [four; sixteen; none; none],
   [none; none; four; sixteen]);
  ([none; two; two; none],
   [four; none; none; none],
   [none; none; none; four]);
  ([none; none; two; two ],
   [four; none; none; none],
   [none; none; none; four]);
  ([eight; eight; eight; eight],
   [sixteen; sixteen; none; none],
   [none; none; sixteen; sixteen]);
  ([eight; eight; fht; fht],
   [sixteen; ttf; none; none],
   [none; none; sixteen; ttf]);
  ([none; none; ttf; none],
   [ttf; none; none; none],
   [none; none; none; ttf]);
  ([none; ttf; ttf; ttf],
   [ttfe; ttf; none; none],
   [none; none; ttf; ttfe]);
  ([none; ttf; none; ttf],
   [ttfe; none; none; none],
   [none; none; none; ttfe])
]

let () =
  testResult "powerOfTwo" powTwoRes ;
  testResult "increment" incrementRes ;
  testResult "propagate" propagateRes ;
  printf "\n" ;
  exit 0
