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

(* PART II *)

let decode_result result =
  match result with
  | 'X' -> "loss"
  | 'Y' -> "draw"
  | 'Z' -> "win"
  | _ -> failwith "not a valid char";;

let inverse_decode_player player =
  match player with
  | "rock" -> 'X'
  | "paper" -> 'Y'
  | "scissor" -> 'Z'
  | _ -> failwith "not a valid char";;

let decode_action player1 result =
  let m_player1 = decode_player player1 in
  let dec_result = decode_result result in
  if m_player1 = "rock" then
    match dec_result with
    | "loss" -> inverse_decode_player "scissor"
    | "win" -> inverse_decode_player "paper"
    | "draw" -> inverse_decode_player "rock"
    | _ -> failwith "not a valid result"
  else if m_player1 = "paper" then
    match dec_result with
    | "loss" -> inverse_decode_player "rock"
    | "win" -> inverse_decode_player "scissor"
    | "draw" -> inverse_decode_player "paper"
    | _ -> failwith "not a valid result"
  else
    match dec_result with
    | "loss" -> inverse_decode_player "paper"
    | "win" -> inverse_decode_player "rock"
    | "draw" -> inverse_decode_player "scissor"
    | _ -> failwith "not a valid result" ;;

let rec compute_new_score lst total =
  match lst with
  | [] -> total
  | head :: tail -> compute_new_score tail (total + (
                                        (action_payoff (decode_player(decode_action head.[0] head.[2])))
                                        + game_pay_off head.[0] (decode_action head.[0] head.[2])
                                        )) ;;

compute_new_score lines 0;;
