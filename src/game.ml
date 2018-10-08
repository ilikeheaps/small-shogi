open PlayerSig

let players : (module PLAYER) list =
  [ (module Human)
  ; (module Ai)
  ]
  
let choosePlayer (players : (module PLAYER) list) : (module PLAYER) =
  Menu.select
    (List.map
       (* TODO why couldn't this be inferred? *)
       (fun (module Player : PLAYER) ->
         (Player.name, (module Player : PLAYER)))
       players)

(* TODO move this or replace *)
(* TODO is the order of arguments usual? *)
let append (str1 : string) (str2 : string) : string =
  String.concat "" [str2 ; str1]
let output_string_nl chan str = output_string chan (String.concat "" [str; "\n"]) ; flush chan

(* TODO make it exit by exiting the function *)
(* TODO make the debug file optional *)
let rec loop (debug : out_channel) : unit =
  (Menu.choose
     [("Graj",
       fun () ->
       let (module FirstPlayer) = renamePlayer (append " 1") (choosePlayer players) in
       let (module SecondPlayer) = renamePlayer (append " 2") (choosePlayer players) in
       (* play a match *)
       let matchFinished = Match.playMatch
                             (module FirstPlayer)
                             (module SecondPlayer)
                             Shogi.defaultState
                             ~displayMatch:(fun board ->
                               output_string_nl debug "display" ;
                               DisplayShogi.displayBoard
                                 [ "Sente: " ^ FirstPlayer.name
                                 ; "Gote: " ^ SecondPlayer.name
                                 ]
                                 board;
                               output_string_nl debug "end_display")
                             ~game_log:(fun str -> output_string_nl debug "moved")
                             ~debug_log:(fun str -> output_string_nl debug str)
       in
       (* get the result or something of the match *)
       let line1, line2 = DisplayShogi.string2_of_status matchFinished.Shogi.status in
       (* display results *)
       DisplayShogi.board_interact
	 matchFinished
	 None
	 None
	 None
	 [line1; line2]
	 [("Okay.", ())]);
      ("Wyjdź", fun () -> close_out debug; exit 0)]); 
  (loop debug)

let () =
  let debug_out = open_out "debug.log" in
  output_string_nl debug_out "started program" ;
  loop debug_out
  
  
  
