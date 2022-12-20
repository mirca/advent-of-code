(*** PART I ***)

#use "../lib/helpers.ml"

let lines = lines_of_file "input.txt" ;;
let length = List.length lines ;;
let half_length = length / 2 ;;
let length_of_num =  String.length (List.hd lines) ;;

let rec sum_along_position_inner l i acc =
  match l with
  | [] -> acc
  | h :: t -> sum_along_position_inner t i (acc + int_of_string (String.make 1 h.[i])) ;;

let sum_along_position l i =
  sum_along_position_inner l i 0;;

let rec get_freq_of_digits l a b =
  if a > b then []
  else (sum_along_position l a) :: get_freq_of_digits l (a+1) b ;;

let digits_freq = get_freq_of_digits lines 0 (length_of_num-1) ;;

let rec decode l token =
  match l with
  | [] -> []
  | h :: t -> (if h < token then 0 else 1) :: decode t token ;;

let rec flip_bits l =
  match l with
  | [] -> []
  | h :: t -> (if h = 0 then 1 else 0) :: flip_bits t ;;

let rec binary_to_decimal l =
  match l with
  | [] -> 0.
  | h :: t -> (float_of_int h) *. 2. ** float_of_int (List.length t) +. binary_to_decimal t ;;

let gamma_rate_binary = decode digits_freq half_length ;;
let epsilon_rate_binary = flip_bits gamma_rate_binary ;;
let gamma_rate_decimal = binary_to_decimal gamma_rate_binary;;
let epsilon_rate_decimal = binary_to_decimal epsilon_rate_binary;;

gamma_rate_decimal *. epsilon_rate_decimal ;;

(*** PART II ***)

let rec filter_by_bit_criteria_along_position list bit pos =
  match list with
  | [] -> []
  | h :: t -> let condition = ((int_of_string (String.make 1 h.[pos])) = bit) in
               (if condition then [h] else []) @
               filter_by_bit_criteria_along_position t bit pos ;;

let rec filter_by_bit_criteria list kind pos =
  let list_len = List.length list in
  let token = if list_len mod 2 = 0 then list_len / 2 else 1 + (list_len / 2) in
  let df :: _ = get_freq_of_digits list pos pos in
  let mcb =
    match kind with
    | "oxygen" -> if df >= token then 1 else 0
    | "CO2" -> if df >= token then 0 else 1 in
  match list with
  | [] -> []
  | _ -> if list_len = 1 then list
         else let curr_list = filter_by_bit_criteria_along_position list mcb pos in
          if (List.length curr_list) = 1 then curr_list
          else filter_by_bit_criteria curr_list kind (pos+1) ;;

let oxygen_binary_string :: _ = filter_by_bit_criteria lines "oxygen" 0 ;;
let oxygen_binary = string_to_list oxygen_binary_string 0 ((String.length oxygen_binary_string) - 1) ;;
let oxygen_binary_int = list_string_to_int oxygen_binary ;;
let oxygen_decimal = binary_to_decimal oxygen_binary_int ;;

let co2_binary_string :: _ = filter_by_bit_criteria lines "CO2" 0 ;;
let co2_binary = string_to_list co2_binary_string 0 ((String.length co2_binary_string) - 1) ;;
let co2_binary_int = list_string_to_int co2_binary ;;
let co2_decimal = binary_to_decimal co2_binary_int ;;
oxygen_decimal *. co2_decimal ;;