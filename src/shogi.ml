open Extensions

type player = Sente | Gote
type physPiece = char
type promotion = bool
type piece = physPiece * promotion
type field = (piece * player) option
(* (row, column) *)
type position = int * int
type board = field array array
type pieceMove = {from: position; toWhere: position; promote: bool}
type move = Drop of physPiece * position | Move of pieceMove | Resign
type outcome = Win of player | Draw
(* alt: Ongoing of player <- no need for currentPlayer *)
type status = Finished of outcome * string | Ongoing
type komadai = (physPiece * int) list
(* alt: handSente; handGote *)
type matchState = {currentPlayer : player;
                   moveHistory : move list;
                   board :  board;
                   handCurrent : komadai; (* hand of the current player *)
                   handNext : komadai;
                   status : status}

let pawn : physPiece = 'P'
let silver : physPiece = 'S'
let golden : physPiece = 'G'
let king : physPiece = 'K'
let knight : physPiece = 'N'
(* ignoring ranged pieces:
   let lance : physPiece = 'L'
   let rook : physPiece = 'R'
   let bishop : physPiece = 'B' *)

let promoted (piece : physPiece) : piece = (piece, true)

let unpromoted (piece : physPiece) : piece = (piece, false)


let string_of_phys (piece : physPiece) =
  Char.escaped(piece)

let string_of_piece ((piece, promoted) : piece) =
  if promoted then "+"^(Char.escaped piece)
  else " "^(Char.escaped piece)

let string_of_position (x, y) = "("^string_of_int x^", "^string_of_int y^")"

let string_of_move = function
  | Drop(p, pos) -> "drop: "^String.make 1 p^" -> "^string_of_position pos
  | Resign -> "resign"
  | Move {from; toWhere; promote} ->
     let strp = string_of_position in
     "move: "^strp from^"->"^strp toWhere^if promote then "+" else ""

let revPlayer (p : player) : player =
  match p with
  | Sente -> Gote
  | Gote -> Sente

let addToHand (hand : komadai) (piece : physPiece) : komadai =
  try
    let count = List.assoc piece hand in
    (piece, count + 1) :: (List.remove_assoc piece hand)
  with
  | Not_found -> (piece, 1) :: hand


(* throws Not_found failure if there is no such piece on hand *)
let removeHand (hand : komadai) (piece : physPiece) : komadai =
  let count = List.assoc piece hand in
  if count = 1 then
    List.remove_assoc piece hand
  else
    (piece, count - 1) :: (List.remove_assoc piece hand)

let column ((y, x) : position) : int = x

let in_promotion board player  position =
  let board_y = Array.length board in
  let (pos_y, _) = position in
  match player with
  | Sente -> pos_y  < board_y/3
  | Gote -> pos_y >= board_y*2/3

(* checks if the piece is allowed to move in specified way *)
(* assumes endPos and startPos are on board *)
let is_move_valid board startPos endPos =
  match Array.get_2d board startPos with
  | Some(piece, player) ->
     let (start_y, start_x) = startPos in
     let (end_y, end_x) = endPos in
     let movement =
       if player = Gote then (end_y - start_y, end_x - start_x)
       else (start_y - end_y, start_x - end_x) in
     List.exists
       (fun move -> move = movement)
       (match piece with
       | x when x = unpromoted pawn -> [(1, 0)]
       | x when x = unpromoted silver -> [(1, -1); (1, 0); (1, 1); (-1, -1); (-1, 1)]
       | x when x = unpromoted(knight) -> [(2, -1); (2, 1)]
       | x when x = unpromoted golden || x = promoted pawn || x = promoted silver || x = promoted knight ->
          [(1, -1); (1, 0); (1, 1); (0, -1); (0, 1); (-1, 0)]
       | x when x = unpromoted(king) -> [(1, -1); (1, 0); (1, 1); (0, -1); (0, 1); (-1, -1); (-1, 0); (-1, 1)]
       | _ -> [])
  | None -> false

(* TODO: check repetition rule, promote *)
let applyMove (currentMatch : matchState) (newMove : move) : matchState =
  match currentMatch.status with
  | Ongoing ->
     let currentPlayer = currentMatch.currentPlayer in
     let opponent = revPlayer currentMatch.currentPlayer in
     let newBoard = Array.copy_matrix currentMatch.board in
     let illegalMoveMatch reason =
       {currentPlayer = opponent;
        moveHistory = newMove :: currentMatch.moveHistory;
        board = newBoard;
        handCurrent = currentMatch.handNext;
        handNext = currentMatch.handCurrent;
        status = Finished(Win(opponent), reason)
       }
     in
     (match newMove with
     (* moving an own piece *)
     | Move({from=from; toWhere=toWhere; promote=promote }) ->
        (match Array.get_2d currentMatch.board from with
        | Some(oPiece, player) when player = currentPlayer ->
           let ownPiece, status =
             if promote then
               let (piece, promoted) = oPiece in
               if promoted then oPiece, Ongoing
               else
                 if (in_promotion newBoard currentPlayer from || in_promotion newBoard currentPlayer toWhere) then
                   (piece, true), Ongoing
                 else
                   (piece, true), Finished(Win(opponent), "Trying to promote outside promotion zone")
             else oPiece, Ongoing
           in
           if status != Ongoing then
             {currentMatch with status = status}
           else if not (is_move_valid currentMatch.board from toWhere) then
             {currentMatch with status = Finished(Win(opponent), "Incorrect move")}
           else
             (match Array.get_2d currentMatch.board toWhere with
             (* capturing opponent's piece *)
             | Some(capPiece, opp) when opp = opponent ->
                (Array.set_2d newBoard toWhere (Some(ownPiece, player)) ;
                 Array.set_2d newBoard from None ;
                 let status =
                   (match fst capPiece with
                   | a when a = king -> Finished(Win(currentPlayer), "King captured")
                   | _ -> currentMatch.status)
                 in
                 {currentPlayer = opponent;
                  moveHistory = newMove :: currentMatch.moveHistory;
                  board = newBoard;
                  handNext = addToHand currentMatch.handCurrent (fst capPiece);
                  handCurrent = currentMatch.handNext;
                  status = status}
                )
             (* moving to an empty field *)
             | None ->
                (Array.set_2d newBoard toWhere (Some(ownPiece, player)) ;
                 Array.set_2d newBoard from None ;
                 {currentPlayer = opponent;
                  moveHistory = newMove :: currentMatch.moveHistory;
                  board = newBoard;
                  handCurrent = currentMatch.handNext;
                  handNext = currentMatch.handCurrent;
                  status = currentMatch.status}
                )
             | _ -> illegalMoveMatch "Illegal move: trying to capture own piece")
        | _ -> illegalMoveMatch "Illegal move: trying to move opponent's / no piece")
     (* dropping a piece *)
     | Drop(piece, toWhere) ->
        (match Array.get_2d newBoard toWhere with
        | Some(_) -> illegalMoveMatch "Trying to drop a piece onto an occupied square"
        | None ->
           try
             let handCurrent = removeHand currentMatch.handCurrent piece in
             (* TODO: check for mate with pawn drop *)
             let _, x = toWhere in
             if piece = pawn &&
               (Array.exists
                  (fun row -> row.(x) = (Some((pawn, false), currentPlayer)))
                  currentMatch.board)
             then
               illegalMoveMatch "Opponent did illegal pawn drop"
             else
               (Array.set_2d newBoard toWhere (Some((piece, false), currentPlayer)) ;
                (* TODO: could check for checkmate here *)
                {currentPlayer = opponent;
                 moveHistory = newMove :: currentMatch.moveHistory;
                 board = newBoard;
                 handCurrent = currentMatch.handNext;
                 handNext = handCurrent;
                 status = currentMatch.status}
               )
           with
           (* player doesn't have the piece on hand *)
           | Failure("Not_found") ->
              {currentMatch with status = Finished(Win(opponent), "Incorrect move")})
     | Resign -> {currentPlayer = opponent;
                  moveHistory = newMove :: currentMatch.moveHistory;
                  board = newBoard;
                  handCurrent = currentMatch.handNext;
                  handNext = currentMatch.handCurrent;
                  status = Finished(Win(opponent), "Opponent resigned")})
  | Finished(_) -> failwith "Match_already_finished"




(* TODO: poprawić; brzydko, ale powinno być stosunkowo rzadko używane *)
(* mirrors the board and swaps the players [can be used for game setup so that players have mirrored configurations] *)
let flip_board_players (b : board) : board =
  Array.of_list
    (List.rev_map
       (fun arr ->
         Array.map
           (fun (f : field) ->
             match f with
             | None -> None
             | Some(piece, player) -> Some(piece, revPlayer player))
           arr)
       (Array.to_list b))

let default_board : board =
  let half =
    Array.map
      (fun arr ->
        Array.map
          (fun c -> if c == ' ' then None else Some((c, false), Gote))
          arr
      )
      (* ' ' for empty fields *)
      [| [| 'S'; 'G'; 'K'; 'G'; 'S' |];
         [| ' '; ' '; ' '; ' '; ' ' |];
         [| ' '; 'P'; 'P'; 'P'; ' ' |] |]
  in
  Array.append half (flip_board_players half)
;;

let defaultState = {currentPlayer = Sente;
                    board = default_board;
                    handNext = [];
                    handCurrent = [];
                    moveHistory = [];
                    status = Ongoing}
