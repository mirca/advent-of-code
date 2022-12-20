(*** PART I ***)

#use "../lib/helpers.ml"

let lines = lines_of_file "input.txt" ;;

(* function to add two lists of same length because i'm a noob in ocaml *)
let rec add_lists a b =
  match a, b with
  | _, [] -> a
  | [], _ -> b
  | a_head :: a_tail, b_head :: b_tail ->
      (a_head + b_head) :: add_lists a_tail b_tail ;;

(* decodes the positions of the submarine *)
let decodes_position s =
  let command :: value :: _ = String.split_on_char ' ' s in
  match command with
  | "forward" -> [0; int_of_string value]
  | "down" -> [int_of_string value; 0]
  | "up" -> [- int_of_string value; 0]
  | _ -> [0; 0] ;;

(* traverse through the input *)
let rec get_position l =
  match l with
  | [] -> []
  | head :: tail -> add_lists (decodes_position head) (get_position tail) ;;

(* returns a list containing depth and horizontal position
 and find out the product of those two quantities *)
let pos = get_position lines ;;
let depth :: horizontal_pos :: _ = pos ;;
depth * horizontal_pos ;;

(*** PART II ***)

(* decodes the positions of the submarine *)
let decodes_position_with_aim s aim =
  let command :: value :: _ = String.split_on_char ' ' s in
  match command with
  | "forward" -> [0; aim * (int_of_string value); int_of_string value]
  | "down" -> [int_of_string value; 0; 0]
  | "up" -> [- int_of_string value; 0; 0]
  | _ -> [0; 0; 0] ;;

(* traverse through the input *)
let rec get_position_with_aim l aim =
  match l with
  | [] -> []
  | head :: tail -> let aim_increment :: measurements = decodes_position_with_aim head aim in
                    add_lists (aim_increment :: measurements) (get_position_with_aim tail (aim_increment + aim));;

(* returns a list containing aim, depth and horizontal position
 and find out the product of those two quantities *)
let pos = get_position_with_aim lines 0 ;;
let aim :: depth :: horizontal_pos :: _ = pos ;;
depth * horizontal_pos ;;