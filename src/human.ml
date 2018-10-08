open Extensions
open LTerm_widget
open Lwt
  
let name = "Human"

let komadai_labels (komadai : Shogi.komadai) =
  List.map
    (fun (piece, count) ->
      (string_of_int count) ^ " * " ^ (Shogi.string_of_phys piece))
    komadai

(*
select_stuff matchState : move optional * (unit -> unit)

makeMove ~=
  let rec stepMove (optmove, f) =
     match optmove with
     | Some(move) -> move
     | None -> stepMove( f () ) in
  stepMove( ... )

  -> fixpoint?
*)
    
let string_of_field (field : Shogi.field) =
  match field with
  | Some(piece, player) ->
     (if player = Shogi.Sente then
	 (* assuming uppercase is default *)
	 (fun a -> a)
      else
	 (* might be deprecated? *)
	 String.lowercase)     
       (Shogi.string_of_piece piece)
  | None -> "  "
     
let labels_of_board (board : Shogi.board) =
  Array.map_2d
    string_of_field
    board

type 'a lcalc = Result of 'a | Cont of (unit -> 'a lcalc)
    
let makeMove matchState =
  let rec step opt_move =
    match opt_move with
    | Result(move) -> move
    | Cont(cont) -> step (cont ())
  in
  let board_interact = DisplayShogi.board_interact matchState in
  let rec trygetting ()=
    (board_interact
       (Some(fun piece count ->
	 Cont(fun () ->
	   board_interact
	     None
	     None
	     (Some(fun y x -> Result(Shogi.Drop(piece, (y, x)))))
	     ["Select place to drop"]
	     [ ("Cancel", Cont(trygetting)) ]
	 )))
       None	 
       (Some(fun y x ->
	 Cont(fun() ->
	   board_interact
	     None
	     None
	     (Some(fun y2 x2 ->
	       Cont(fun () ->
		 board_interact
		   None
		   None
		   None
		   ["Do you promote?"; "(it's a tricky question)"]
		   [("Yup", Result(Shogi.Move({Shogi.from = (y, x);
					       Shogi.toWhere = (y2, x2);
					       Shogi.promote = true})));
		    ("Nah", Result(Shogi.Move({Shogi.from = (y, x);
					      Shogi.toWhere = (y2, x2);
					      Shogi.promote = false})));
		    ("Cancel", Cont(trygetting))])))
	     ["Select destination"]
	     [ ("Cancel", Cont(trygetting)) ])))
       ["Press stuff to move it"; "Do not move somebody else's stuff"]
       [("Resign", Result(Shogi.Resign))])
  in
  step (Cont(trygetting))
    
let print_move (move : Shogi.move) =
  match move with
  | Shogi.Move({Shogi.from=(y1, x1); Shogi.toWhere = (y2, x2); Shogi.promote = _}) ->
     Lwt_io.printf "Move: %d %d -> %d %d\n" y1 x1 y2 x2
  | Shogi.Drop(piece, (y, x)) -> 
     Lwt_io.printf "Drop: %s %d %d\n" (Shogi.string_of_phys piece) y x
  | _ ->
     Lwt_io.printf "Welp\n"

(* 
 * some quick aliases to get new interface working
 *)
(* TODO: reconsider the interface
 *       - now we need to store the move here for incrementMove
 *         - but it seems we don't after all?
 *)
type internalState = Shogi.move
let firstMove matchState _time =
  let m = makeMove matchState in (m, m, false)
let initialMove matchState _m time = firstMove matchState time
let incrementMove matchState _m time = firstMove matchState time
                                   
     
