(*** PART 1 ***)

(* read input file into a list of strings *)
(* credit: https://www.rosettacode.org/wiki/Read_a_file_line_by_line *)
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line::acc)
    | None -> (List.rev acc)
  in
  aux []

let lines_of_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  (lines)

let lines = lines_of_file "input.txt" ;;

(* convert list of strings to list of ints*)
let rec list_string_to_int l =
  match l with
  | [] -> []
  | h :: t -> (int_of_string h) :: list_string_to_int t ;;

let int_lines = list_string_to_int lines ;;

(* compute total number of measurements that are larger
than the previous measurement *)
let rec total l =
  match l with
  | [] -> 0
  | _ :: [] -> 0
  | x :: y :: z -> if y > x then 1 + total ( y::z ) else total ( y::z ) ;;

total int_lines ;;

(*** PART 2 ***)

(*warning: my solution is totally not scalable for an arbitraty
window of length n*)

let rec total_slide l =
  match l with
  | [] -> 0
  | _ :: [] -> 0
  | _ :: _ ::[] -> 0
  | _ :: _ :: _ ::[] -> 0
  | a :: b :: c :: d :: t -> if d > a then 1 + total_slide ( b :: c :: d :: t )
                             else total_slide ( b :: c :: d :: t ) ;;

total_slide int_lines ;;