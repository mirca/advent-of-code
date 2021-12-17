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

(* convert list of strings to list of ints*)
let rec list_string_to_int l =
  match l with
  | [] -> []
  | h :: t -> (int_of_string h) :: list_string_to_int t ;;