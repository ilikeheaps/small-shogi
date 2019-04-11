open Extensions
open Shogi

(* TODO how to constrain this file with PlayerSig.PLAYER signature?
 *      (except duplicating the signature)
 *)

let name = "AI"

(* get movement patterns of a piece *)
let moves_of_piece player piece =
  let list =
    match piece with
       | x when x = unpromoted pawn -> [(1, 0)]
       | x when x = unpromoted silver -> [(1, -1); (1, 0); (1, 1); (-1, -1); (-1, 1)]
       | x when x = unpromoted(knight) -> [(2, -1); (2, 1)]
       | x when x = unpromoted golden || x = promoted pawn || x = promoted silver || x = promoted knight ->
	  [(1, -1); (1, 0); (1, 1); (0, -1); (0, 1); (-1, 0)]
       | x when x = unpromoted(king) -> [(1, -1); (1, 0); (1, 1); (0, -1); (0, 1); (-1, -1); (-1, 0); (-1, 1)]
       | _ -> []
  in
  match player  with
  | Sente -> List.map (fun (y, x) -> (-y, x)) list
  | Gote -> list

(* TODO refactor this *)
(* get available moves (excluding drops) *)
let moves board current_player : move list =
  Array.fold_2d_ll
    (fun acc (y, x, field) ->
      match field with
      | Some(piece, player) when player = current_player ->
	 let rec add_moves acc moves =
	   match moves with
	   | (move_y, move_x)::moves ->
	      let start = (y, x) in
	      let destination = (move_y + y, move_x + x) in
	      let board_y, board_x = Array.length board, Array.length board.(0) in
	      if 0 <= move_y + y && move_y + y < board_y && 0 <= move_x + x && move_x + x < board_x
		&& match Array.get_2d board destination with
		| Some(piece, player) when player = current_player  -> false
		| _ -> true
	      then
		let acc = (Move({from = (y, x); toWhere = destination; promote=false}))::acc in
		if in_promotion board current_player start || in_promotion board current_player destination then
		  add_moves (Move({from = (y, x); toWhere = destination; promote=true})::acc) moves
		else
		  add_moves acc moves
	      else
		add_moves acc moves
	   | [] -> acc
	 in
	 add_moves acc (moves_of_piece current_player piece)
      | _ -> acc)
    []
    (Array.mapi_2d
       (fun y x el -> (y, x, el))
       board)

let drops (board : board) (komadai : komadai) (player : player) : move list =
  let drops_of_piece (piece : physPiece) =
    if piece = pawn
    then [ (* TODO (remember about illegal pawn drops) *) ]
    else Array.foldi_2d_ll
           (fun acc x y field ->
             match field with
             | Some _ -> acc
             | None -> Drop (piece, (x, y)) :: acc)
           []
           board
  in
  List.fold_left
    (fun acc (piece, _count) ->
      drops_of_piece piece @ acc)
    []
    komadai
  
module GameShogi =
struct
  type gameState = Shogi.matchState
  type move = Shogi.move

  let string_of_move m = Shogi.string_of_move m
  let applyMove = Shogi.applyMove
  let moves matchState =
    match matchState.Shogi.status with
    | Ongoing -> (moves (matchState.board) (matchState.currentPlayer))
                 @ (drops matchState.board matchState.handCurrent matchState.currentPlayer)
    | _ -> []
end

module ShogiEvaluator =
struct
  type gameState = Shogi.matchState
                 
  type value = int
             
  let (<) = ( (<) : value -> value -> bool )

  let inf = 10000
    
  let value_of_piece piece =
    if piece = unpromoted king then 1000
    else if piece = unpromoted golden then 6
    else if piece = unpromoted silver then 5
    else if piece = promoted silver then 4
    else if piece = unpromoted pawn then 1
    else if piece = promoted pawn then 2
    else 1
    
  (* TODO: account for pieces on komadai *)
  let evalPosition matchState =
    (Array.fold_2d_ll
       (fun sum field ->
	 match field with
	 | Some(piece, player) ->
	    if player = matchState.currentPlayer then sum + (value_of_piece piece)
	    else sum - (value_of_piece piece)
	 | None -> sum)
       0
       matchState.board)	     
end

module MinmaxShogi = Minmax.Minmax (GameShogi) (ShogiEvaluator)

(* quick wrapper to make the new interface compile (and perhaps work) *)
type internalState = (Shogi.move * MinmaxShogi.searchState)
let firstMove matchState _time =
  let s0 = MinmaxShogi.init matchState in
  let (s1, m) = MinmaxShogi.increment s0
  in (m, (m, s1), false)
let initialMove matchState _m time = firstMove matchState time
let incrementMove matchState _m time = firstMove matchState time
