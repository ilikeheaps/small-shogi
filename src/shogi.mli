(** type representing the player's side (Black/White) **)
type player = Sente | Gote
(** type for a piece as a physical object (piece consisting of two sides) with no differentiation between promoted and non-promoted figure **)
type physPiece
(** used for indicating if a piece is promoted **)
type promotion = bool
type piece = physPiece * promotion
type field = (piece * player) option
type position = int * int
type board = field array array
type pieceMove = {from: position; toWhere: position; promote: bool}
type move = Drop of physPiece * position | Move of pieceMove | Resign
type outcome = Win of player | Draw
type status = Finished of outcome * string | Ongoing
type komadai = (physPiece * int) list
type matchState = {currentPlayer : player;
		   moveHistory : move list;
		   board :  board;
		   handCurrent: komadai;
		   handNext: komadai;
		   status : status}
(*
val isPromoted : piece -> bool
val promote : piece -> piece
(** gives a list of possible moves of a given piece **)
val moves : piece -> move list
(** gives a list of possible moves for dropping a given piece **) 
val drops : physPiece -> move list
*)
val pawn : physPiece
val silver : physPiece
val golden : physPiece
val king : physPiece
val knight : physPiece

val promoted : physPiece -> piece
val unpromoted : physPiece -> piece
  
  (* ignoring ranged pieces
val lance : physPiece
val rook : physPiece
val bishop : physPiece
  *)

val in_promotion : board -> player -> position -> bool
  
val string_of_phys : physPiece -> string
  
val string_of_piece : piece -> string

val string_of_move : move -> string

val string_of_position : position -> string
  
val applyMove : matchState -> move -> matchState

val defaultState : matchState

val revPlayer : player -> player
