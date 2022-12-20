(*** PART I ***)

#use "../../lib/helpers.ml"

let lines = lines_of_file "input.txt" ;;

(* compute max weight *)
let rec get_max lst max_sum =
  match lst with
  | [] -> max_sum
  | head :: tail ->
    match head with
    | "" -> max max_sum (get_max tail 0)
    | s ->  get_max tail (max_sum + int_of_string s) ;;

get_max lines 0;;

(*** PART II ***)
