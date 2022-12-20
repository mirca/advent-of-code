(*** PART I ***)

#use "../../lib/helpers.ml"

let lines = lines_of_file "input.txt" ;;

(* PART I *)

let decode_player player =
  match player with
  | 'A' -> "rock"
  | 'X' -> "rock"
  | 'B' -> "paper"
  | 'Y' -> "paper"
  | 'C' -> "scissor"
  | 'Z' -> "scissor"
  | _ -> failwith "not a valid char";;

let action_payoff player =
  match player with
  | "rock" -> 1
  | "paper" -> 2
  | "scissor" -> 3
  | _ -> failwith "not a valid action";;

let result_payoff outcome =
  match outcome with
  | "win" -> 6
  | "draw" -> 3
  | "loss" -> 0
  | _ -> failwith "not a valid payoff";;

let game_pay_off player1 player2 =
  let m_player1 = decode_player player1 in
  let m_player2 = decode_player player2 in
  if m_player2 = "rock" then
    match m_player1 with
    | "rock" -> result_payoff "draw"
    | "paper" -> result_payoff "loss"
    | "scissor" -> result_payoff "win"
    | _ -> failwith "not a valid action"
  else if m_player2 = "paper" then
    match m_player1 with
    | "rock" -> result_payoff "win"
    | "paper" -> result_payoff "draw"
    | "scissor" -> result_payoff "loss"
    | _ -> failwith "not a valid action"
  else
    match m_player1 with
    | "rock" -> result_payoff "loss"
    | "paper" -> result_payoff "win"
    | "scissor" -> result_payoff "draw"
    | _ -> failwith "not a valid action" ;;

let rec compute_score lst total =
  match lst with
  | [] -> total
  | head :: tail -> compute_score tail (total + (
                                        (action_payoff (decode_player head.[2]))
                                        + game_pay_off head.[0] head.[2])) ;;

compute_score lines 0;;
