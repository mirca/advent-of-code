(*** PART I ***)

#use "../../lib/helpers.ml" ;;
#load "str.cma" ;;
open Str ;;

let lines = lines_of_file "input.txt" ;;

let rec parse_input lines = 
  match lines with 
  | [] -> []
  | h::t -> (List.map int_of_string (Str.split (Str.regexp "[^0-9]+") h)) @ (parse_input t);;

(* PART I *)

let is_contained a b c d =
  if ((c >= a) && (d <= b)) then true
  else if ((a >= c) && (b <= d)) then true
  else false

let how_many_contained lst =
  let rec how_many_contained_ lst n =
    match lst with
    | [] -> n
    | a :: b :: c :: d :: tail -> 
      if is_contained a b c d then how_many_contained_ tail (n + 1)
      else how_many_contained_ tail n
  in how_many_contained_ lst 0 ;;

let pairs = parse_input lines ;;
let _ = how_many_contained pairs ;;


(* PART II*)

let donot_overlap a b c d =
  if ( (a < c) && (b < c) ) then true
  else if ( (c < a) && (d < a)) then true
  else false ;;

let how_many_overlap lst =
  let rec how_many_overlap_ lst n =
    match lst with
    | [] -> n
    | a :: b :: c :: d :: tail ->
      if donot_overlap a b c d then how_many_overlap_ tail n
      else how_many_overlap_ tail (n + 1)
  in how_many_overlap_ lst 0 ;;

let _ = how_many_overlap pairs ;;

