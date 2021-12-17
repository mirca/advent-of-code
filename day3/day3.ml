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