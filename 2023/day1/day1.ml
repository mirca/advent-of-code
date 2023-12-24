#require "str" ;;
#use "../../lib/helpers.ml" ;;
open Str ;;

let lines = lines_of_file "input" ;;
let string_to_list_char s = List.init (String.length s) (String.get s) ;;
let list_char_to_string lst = String.of_seq (List.to_seq lst) ;;
let digits = string_to_list_char "0123456789" ;;
let rec reverse_list lst =
  match lst with
  | [] -> []
  | h :: tail -> (reverse_list tail) @ [h] ;;
let rec is_in_list lst x =
  match lst with
    | [] -> false
    | h :: tail ->
      if x = h then true
      else is_in_list tail x ;;
let is_digit x =
  is_in_list digits x ;;
let rec find_digit lst =
  match lst with
  | [] -> raise (Invalid_argument "find_digit is not defined on []")
  | h :: tail -> if is_digit h then h else find_digit tail ;;
let build_number d1 d2 =
  let s = list_char_to_string [d1; d2] in
  int_of_string s ;;
(*** PART I ***)
let rec part_1 lst =
  match lst with
  | [] -> 0
  | h :: tail ->
    let lc = string_to_list_char h in
    let d1 = find_digit lc in
    let d2 = find_digit (reverse_list lc) in
    (build_number d1 d2) + (part_1 tail) ;;
let result_part_1 = part_1 lines ;;

let digits = ["one"
              ; "two"
              ; "three"
              ; "four"
              ; "five"
              ; "six"
              ; "seven"
              ; "eight"
              ; "nine" ] ;;
let rec reverse_list_items lst =
   match lst with
     | [] -> []
     | h :: tail -> [list_char_to_string (reverse_list (string_to_list_char h))] @ reverse_list_items tail ;;
let reverse_digits = reverse_list_items digits ;;
let num_map x =
  match x with
  | "one"   -> '1'
  | "eno"   -> '1'
  | "two"   -> '2'
  | "owt"   -> '2'
  | "three" -> '3'
  | "eerht" -> '3'
  | "four"  -> '4'
  | "ruof"  -> '4'
  | "five"  -> '5'
  | "evif"  -> '5'
  | "six"   -> '6'
  | "xis"   -> '6'
  | "seven" -> '7'
  | "neves" -> '7'
  | "eight" -> '8'
  | "thgie" -> '8'
  | "nine"  -> '9'
  | "enin"  -> '9'
  | _ -> raise (Invalid_argument ("num_map not defined for " ^ x)) ;;
let rec find_digit_word lw s =
  match lw with
  | [] -> None
  | h :: tail -> if StdLabels.String.starts_with ~prefix:h s then Some((num_map h))
                 else find_digit_word tail s ;;
let rec find_first_digit lst lw =
  match lst with
  | [] -> raise (Invalid_argument "find_first_digit is not defined on []")
  | h :: tail -> let dig = find_digit_word lw (list_char_to_string lst) in
                 match dig with
                 | None -> if (is_digit h) then h
                           else find_first_digit tail lw
                 | Some c -> c ;;
(*** PART II ***)
let rec part_2 lst =
  match lst with
  | [] -> 0
  | h :: tail ->
    let lc = string_to_list_char h in
    let rev_lc = reverse_list lc in
    let d1 = find_first_digit lc digits in
    let d2 = find_first_digit rev_lc reverse_digits in
    (build_number d1 d2) + (part_2 tail) ;;
let result_part_22 = part_2 lines ;;
