(*** PART I ***)

#use "../../lib/helpers.ml" ;;
#load "str.cma" ;;
open Str ;;

let lines = lines_of_file "input.txt" ;;

(* PART I *)

let rec string_to_list_of_chars s =
  match s with
  | "" -> []
  | x -> [x.[0]] @ string_to_list_of_chars (Str.last_chars x ((String.length x) - 1)) ;;


let split_list lst n =
  let rec split_list_ lst first_list n =
    match lst with
    | [] -> [], []
    | head :: tail -> if (List.length tail) = n then
                        first_list @ [head], tail
                      else
                        split_list_ tail (first_list @ [head]) n
  in split_list_ lst [] n ;;

let rec is_in_list elem lst =
  match lst with
  | [] -> false
  | h :: tail -> if elem = h then true
                 else is_in_list elem tail


let rec find_repeated_character lst1 lst2 =
  match lst1 with
  | [] -> ' '
  | h :: tail -> if is_in_list h lst2 then h
              else find_repeated_character tail lst2


 let rec find_repeated_char_list lst1 lst2 =
   match lst1 with
   | [] -> []
   | h :: tail -> if is_in_list h lst2 then [h] @ (find_repeated_char_list tail lst2)
               else find_repeated_char_list tail lst2

let rec find_priorities lines =
  match lines with
  | [] -> []
  | head :: tail ->
                  let lst = string_to_list_of_chars head in 
                  let lst1, lst2 = split_list lst ((List.length lst) / 2) in 
                  let rep_char = find_repeated_character lst1 lst2 in 
                  [rep_char] @ find_priorities tail;;

let rec convert_priorities priorities =
  match priorities with
  | [] -> []
  | head :: tail ->
    if (head <= 'Z' && head >= 'A') then
      [(int_of_char head) - (int_of_char 'A') + 27] @ convert_priorities tail
    else if (head <= 'z' && head >= 'a') then
      [(int_of_char head) - (int_of_char 'a') + 1] @ convert_priorities tail
    else
      [] @ convert_priorities tail ;;


let sum_list_int lst =
  let rec sum_list_int_ lst total =
    match lst with
    | [] -> total
    | h::t -> sum_list_int_ t (total + h)
  in sum_list_int_ lst 0 ;;


let priorities = find_priorities lines ;;
let num_priorities = convert_priorities priorities ;;
let _ = sum_list_int num_priorities ;;

(* PART II *)

let rec find_badges lines =
  match lines with
  | [] -> []
  | s1 :: s2 :: s3 :: tail ->
                  let lst1 = string_to_list_of_chars s1 in
                  let lst2 = string_to_list_of_chars s2 in 
                  let lst3 = string_to_list_of_chars s3 in
                  let badge = find_repeated_character (find_repeated_char_list lst1 lst2) lst3 in 
                  [badge] @ (find_badges tail);;

let badges = find_badges lines ;;
let num_badges = convert_priorities badges ;;
let _ = sum_list_int num_badges ;;