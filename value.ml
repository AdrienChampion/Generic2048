(** A value is either None or an integer, i.e. the exponant of the
    corresponding power of two. *)
type value = None
	   | TwoTo of int

exception IllegalPowerOfTwo
exception Illegal2048Value
exception TryingToSumNone

(** Returns the power of two corresponding to the input value. This is
    the actual value of the cell in 2048. *)
let rec powerOfTwo = function
  | n when n < 0 -> raise IllegalPowerOfTwo
  | 0 -> 1
  | n -> 2 * (powerOfTwo (n - 1))

(** Converts a value to string, i.e. the corresponding power of two as
    a string. *)
let rec toString = function
  | None -> "-"
  | TwoTo n -> string_of_int (powerOfTwo n)

(** Creates a value. The input integer is the POWER OF TWO of the
    value (0 returns None). Only valid for input < 12, raises an
    exception otherwise. *)
let value = function
  | 0 -> None
  | n when ((0 < n) && (n < 12)) -> TwoTo n
  | n -> raise Illegal2048Value

(** Returns None. *)
let none = None
(** Returns 2 (2^1) .*)
let two = TwoTo 1
(** Returns 4 (2^2) .*)
let four = TwoTo 2
(** Returns 8 (2^3) .*)
let eight = TwoTo 3
(** Returns 16 (2^4) .*)
let sixteen = TwoTo 4
(** Returns 32 (2^5) .*)
let thirtyTwo = TwoTo 5
(** Returns 64 (2^6) .*)
let sixtyFour = TwoTo 6
(** Returns 128 (2^7) .*)
let hundredTwentyEight = TwoTo 7
(** Shorthand for 128. *)
let hte = hundredTwentyEight
(** Returns 256 (2^8). *)
let twoHundredFiftySix = TwoTo 8
(** Shorthand for 256. *)
let thfs = twoHundredFiftySix
(** Returns 512  (2^9) .*)
let fiveHundredTwelve = TwoTo 9
(** Shorthand for 512. *)
let fht = fiveHundredTwelve
(** Returns 1024 (2^10) .*)
let thousandTwentyFour = TwoTo 10
(** Shorthand for 1024. *)
let ttf = thousandTwentyFour
(** Returns 2048 (2^11) .*)
let twoThousandFortyEight = TwoTo 11
(** Shorthand for 2048. *)
let ttfe = twoThousandFortyEight

(** Increments the exponent of the power of two, corresponds to adding
    the input integer to itself. *)
let increment = function
  | None -> raise TryingToSumNone
  | TwoTo n when n < 11 -> TwoTo (n + 1)
  | _ -> raise Illegal2048Value

(** Prints a list of values as a row. *)
let printRow list = begin
  Printf.printf "| " ;
  List.iter
    (function s -> Printf.printf "%5s " (s ^ " "))
    (List.map toString list) ;
  Printf.printf "|"
end


(** Propagation to the left for a row. Follows the rules of 2048. *)
let propagateLeftRow =
  let translate list = None :: list in
  let rec loop left right = function
    | None :: tail -> loop left (translate right) tail
    | e :: e' :: tail when e = e' ->
      loop ((increment e) :: left) (translate right) tail
    | e :: None :: tail ->
      loop left (translate right) (e :: tail)
    | e :: tail -> loop (e :: left) right tail
    | [] -> (List.rev left) @ right
  in loop [] []

(** Propagation to the right for a row. Follows the rules of 2048. *)
let propagateRightRow list = List.rev (propagateLeftRow (List.rev list))
