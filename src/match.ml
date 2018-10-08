(* poprawność ruchów:
   - poruszana figura ma mieć możliwość dalszego ruchu
   - król nie może wejść na bite pole [w pewnym sensie zrobione]
   - nie można powtórzyć 4. raz tej samej sytuacji (tego samego stanu gry) szachując
   - nie można zrzucić piona w kolumnie z niepromowanym pionem [zrobione]
   - nie można bezpośrednio matować przez zrzucenie piona
     - pod szachem jedyne dozwolone ruchy to te, które wychodzą z szachu; zatem inaczej mówiąc nie można zrzucić piona tak, że przeciwnik nie ma dozwolonych ruchów

   Inne zasady:
   - 4-krotne powtórzenie bez szachu -> remis
     --> jak liczyć szybko? :C hashowanie?
         - właściwie to czy trzeba liczyć tak szybko? mała plansza, mało ruchów
   - jishougi
 *)

open Shogi

module type PLAYER = (PlayerSig.PLAYER)

(* TODO: move somewhere or import from a library if available *)       
let rec fix : (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b) =
  fun f x -> f (fix f) x

(*
  Play a match between two players from a given state. Use =output= to display board.
  TODO: timer, incrementing move
 *)
let playMatch
      ?(displayMatch : matchState -> unit = fun _ -> ())
      ?(debug_log : string -> unit  = fun _ -> ())
      (* TODO is this the right type?*)
      ?(game_log : move -> unit  = fun _ -> ())
      (module Sente : PLAYER)
      (module Gote : PLAYER)
      (initial : matchState) : matchState =
  (* Make the given move; perform the given action; if the move ended the game, return game state, otherwise pass the result to continuation
   *)
  let commitMove : move -> matchState -> (matchState -> unit) -> (matchState -> matchState)
                   -> matchState =
    fun move state afterMoveAct cont ->
    let moveResult = applyMove state move
    in
    afterMoveAct moveResult ;
    match moveResult.status with
    | Ongoing -> cont moveResult
    | _ -> moveResult
  in
  (* Get the move of one turn
   * TODO timer
   * TODO iterate (increment move)
   *)
  let getMove : type state .
                      (module PLAYER with type internalState = state)
                      -> state
                      -> matchState
                      -> Timer.time
                      -> move * state
    = fun (module Player) playerState matchState remaining ->
    (* here the decision to continue is ignored *)
    let (m, s, _c) = Player.initialMove matchState playerState remaining
    in (m, s)
  in
  (* like above but when there is no playerState for the player (because it doesn't exist yet)
   * TODO timer
   * TODO incrementing move *)
  let getFirstMove : type state .
                          (module PLAYER with type internalState = state)
                          -> matchState
                          -> Timer.time
                          -> move * state
    = fun (module Player) currentMatch remaining ->
    let (m, s, _c) = Player.firstMove currentMatch remaining
    in (m, s)
  in
  (* compare makeFirstMove and makeMove:
     (s -> (ms -> ms)) -> (ms -> ms)
     (s -> (ms -> ms)) -> (s -> (ms -> ms))

     (s -> t) -> t
     (s -> t) -> (s -> t)
     
     Does a type exist that generalizes these two types?
     
     Yes!
     a -> (s -> t) -> a
     a <- { t, s -> t }

     Is this the simplest type? I don't know.
   *)
  (* progress the game by one move (current player doesn't have state yet) *)
  (* TODO timer - default value *)
  let makeFirstMove : type state .
                           (module PLAYER with type internalState = state)
                           -> (state -> matchState -> matchState)
                           -> matchState
                           -> matchState
    = fun player cont matchState ->
    debug_log "makeFirstMove" ;
    let (move, compState) = getFirstMove player matchState Timer.defaultValue
    in
    debug_log "makeFirstMove before commitMove" ;
    commitMove move matchState displayMatch (cont compState)
  in
  let makeMove : type state .
                      (module PLAYER with type internalState = state)
                      -> (state -> matchState -> matchState)
                      -> state
                      -> matchState
                      -> matchState
    = fun player cont playerState matchState ->
    debug_log "makeMove" ;
    let (move, newPlayerState) = getMove player playerState matchState Timer.defaultValue
    in
    debug_log "makeMove before commitMove"
    ; commitMove move matchState displayMatch (cont newPlayerState)
  in
  (* TODO make this type
  let makeGeneralMove2 : type state .
                             (module PLAYER with type internalState = state)
                             -> ((move -> state -> matchState -> matchState) -> 'a)
                             -> (state -> matchState -> matchState)
                             -> 'a
    = fun player stuff cont ->
    stuff (fun move playerSt matchSt -> commitMove move matchSt output (cont playerSt))
  in
  let makeMove' : type state .
                       (module PLAYER with type internalState = state)
                       -> (state -> matchState -> matchState)
                       -> state
                       -> matchState
                       -> matchState
                          (*let makeMove'*)
    = fun (type state)
          (player : (module PLAYER with type internalState = state))
          (cont : state -> matchState -> matchState) ->
    (* = fun player cont -> *)
    (makeGeneralMove2 : (module PLAYER with type internalState = state)
                        -> ((move -> state -> matchState -> matchState) -> 'a)
                        -> (state -> matchState -> matchState)
                        -> 'a)
      player
      (fun (f : move -> state -> matchState -> matchState)
           (playerSt)
           matchSt ->
        let (move, (newPlayerSt : state)) =
          getMove
            (player : (module PLAYER with type internalState = state))
            (playerSt : state)
            matchSt
            Timer.defaultValue
        in f move newPlayerSt matchSt)
      cont
  in
  let makeFirstMove' player cont matchState =
    makeGeneralMove2 player
      (fun f playerSt matchSt ->
        let (move, playerSt) = getFirstMove player matchSt Timer.defaultValue
        in f move playerSt matchSt)
  in
   *)
  (*
  let makeGeneralMove1 : type state .
                             (module PLAYER with type internalState = state)
                             -> ((Shogi.move -> state -> matchState) -> 'a)
                             -> (state -> matchState -> matchState)
                             -> matchState
                             -> 'a
    = fun player stuff cont matchState ->
    stuff (fun move st -> commitMove move matchState output (cont st))
  in
   *)
  (*
  let foo player cont playerState matchState =
    let (move, newPlayerState) = getMove player playerState matchstate Timer.defaultValue
    in  makeGeneralMove1
   *)
  (* Play a match from the given state *)
  let play : type stateA stateB .
                 (module PLAYER with type internalState = stateA)
                 -> (module PLAYER with type internalState = stateB)
                 -> stateA
                 -> stateB
                 -> matchState
                 -> matchState
    = fun pl1 pl2 -> (* "implicitly" passing: st1_init st2_init matchState *)
    fix (fun (cont : stateA -> stateB -> matchState -> matchState) st1 st2 ms ->
        makeMove pl1
          (fun st1' ms' ->
            makeMove pl2
              (fun st2' ms'' -> cont st1' st2' ms'') st2 ms') st1 ms)
            
  in
  (* TODO check if this is right when things start working *)
  (* note: you can't fold matchSt in function application because it will make
   *       stuff be evaluated earlier than expected (these things have side effects!) 
   * I suppose it would be okay if the side effects were restricted to things
   * like logging. *)
  makeFirstMove (module Sente)
    (fun (stateSente : Sente.internalState) matchSt ->
      makeFirstMove (module Gote)
        (fun (stateGote : Gote.internalState) newMatchSt ->
          play (module Sente) (module Gote) stateSente stateGote newMatchSt)
        matchSt)
    initial
