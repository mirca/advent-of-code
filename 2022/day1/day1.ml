(*** PART I ***)

#use "../../lib/helpers.ml"

let lines = lines_of_file "input.txt" ;;

(* compute max calories *)
let rec get_max lst max_sum =
  match lst with
  | [] -> max_sum
  | head :: tail ->
    match head with
    | "" -> max max_sum (get_max tail 0)
    | s ->  get_max tail (max_sum + int_of_string s) ;;

get_max lines 0;;

(*** PART II ***)

let rec get_top_three lst max1 max2 max3 =
  match lst with
  | [] -> max1 + max2 + max3
  | head :: tail ->
                  let first_max = max max3 (max max1 max2) in
                  let second_max = max (min max1 max2) (min (max max1 max2) max3) in
    match head with
    | "" -> max (max1 + max2 + max3)
                (get_top_three tail first_max second_max 0)
    | s ->  get_top_three tail first_max second_max (max3 + int_of_string s) ;;

get_top_three lines 0 0 0;;
