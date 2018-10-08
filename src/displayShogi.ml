open Extensions
open Shogi
open LTerm_widget
open Lwt

let string_of_player player =
  match player with
  | Sente -> "Sente"
  | Gote -> "Gote"

let string2_of_status (status : status) =
  match status with
  | Ongoing -> ("Ongoing", "")
  | Finished(result, reason) ->
     (match result with
     | Win(player) -> ((string_of_player player)^" won", reason)
     | Draw -> ("Draw", reason))

let komadai_labels (komadai : komadai) =
  List.map
    (fun (piece, count) ->
      (string_of_int count) ^ " * " ^ (string_of_phys piece))
    komadai

(* TODO: differentiate bettween players *)
let string_of_field (field : field) =
  match field with
  | Some(piece, player) ->
     (if player = Sente then
	 (fun a -> a)
      else
	 (* might be deprecated? *)
	 String.lowercase)     
       (string_of_piece piece)
  | None -> "  "

let print_move (move : Shogi.move) =
  match move with
  | Shogi.Move({Shogi.from=(y1, x1); Shogi.toWhere = (y2, x2); Shogi.promote = _}) ->
     Printf.printf "Move: %d %d -> %d %d\n" y1 x1 y2 x2
  | Shogi.Drop(piece, (y, x)) -> 
     Printf.printf "Drop: %s %d %d\n" (Shogi.string_of_phys piece) y x
  | _ ->
     print_endline "Welp\n"
     
let labels_of_board (board : board) =
  Array.map_2d
    string_of_field
    board

(* This blocks until one of the events occurs *)
(* TODO could this display players' names? *)
let rec board_interact
          (matchState : matchState)
          (* current player's *)
          (komadai_curr_fun : (physPiece -> int -> 'a) option)
          (* next player's *)
          (komadai_next_fun : (physPiece -> int -> 'a) option)
          (board_fun : (int -> int -> 'a) option)
          (info : string list)
          (add_buttons: (string * 'a) list) =
  let waiter, wakener = Lwt.wait() in
  let komadai_but_ls komadai mapping =
    List.map
      (fun (piece, count) ->
	((string_of_int count) ^ " * " ^ (string_of_phys piece),
	 match mapping with
	 | Some(value) -> fun () -> Lwt.wakeup wakener (value piece count)
	 | None -> fun () -> () ))
    komadai in
  let komadai_current = new vbox in
  Display.box_add_ls
    komadai_current
    (komadai_but_ls matchState.handCurrent komadai_curr_fun)
    (fun () -> new hline);
  let komadai_next = new vbox in
  Display.box_add_ls
    komadai_next
    (komadai_but_ls matchState.handNext komadai_curr_fun)
    (fun () -> new hline);
  (* gote on left, sente on right *)
  let komadai_gote, komadai_sente =
    if matchState.currentPlayer = Sente then
      komadai_next, komadai_current
    else
      komadai_current, komadai_next
  in
  let vbox = new vbox in
  (* TODO refactor this *)
  (* List.map (fun x -> vbox#add ~expand_false x) *)
  vbox#add ~expand:false (new label "  Gote   ");
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false komadai_gote;
  vbox#add ~expand:false (new hline);
  vbox#add (new LTerm_widget.t "glue");
  let komadai_left = new frame in
  komadai_left#set vbox;
  let vbox = new vbox in
  vbox#add (new LTerm_widget.t "glue");
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false komadai_sente;
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false (new label "  Sente  ");
  let komadai_right = new frame in
  komadai_right#set vbox;
  let board_lab = labels_of_board matchState.Shogi.board in
  let board = Display.button_matrix board_lab
    (fun y x -> fun () ->
      match board_fun with
      | Some(mapping) -> wakeup wakener (mapping y x)
      | None -> ()) in
  let middle = new vbox in
  let hbox = new hbox in
  hbox#add ~expand:false board;
  hbox#add (new t "glue");
  middle#add ~expand:false
    (new label ("Current player: "
                ^ (string_of_player matchState.currentPlayer)));
  middle#add ~expand:false hbox;
  List.iter
    (fun lab ->
      middle#add ~expand:false (new label lab))
    info;
  List.iter
    (fun (lab, value) ->
      let button = new button lab in
      button#on_click (fun () -> wakeup wakener value);
      middle#add ~expand:false (new hline);
      middle#add ~expand:false button)
    add_buttons;
  middle#add ~expand:false (new hline);
  middle#add (new t "glue");
  let stuff = new hbox in
  stuff#add komadai_left;
  stuff#add ~expand:false middle;
  stuff#add komadai_right;
  Lwt_main.run
    (Lazy.force LTerm.stdout >>= fun term ->
     LTerm.enable_mouse term >>= fun () ->
     Lwt.finalize
       (fun () -> run term stuff waiter)
       (fun () -> LTerm.disable_mouse term))
  
(* TODO improve this *)
let displayBoard : string list -> matchState -> unit =
  fun info matchState -> board_interact matchState None None None
                      (List.append info
                         [String.concat " "
                            [ string_of_player (Shogi.revPlayer matchState.currentPlayer)
                            ; "moved."
                            ]
                         ])
                      [("Okay", ())]
  
