(*
 * Functions in here are supposed to take the state of a match, time remaining (for the current move) and state of the computation (taken from previous steps) and return the move, a new state of the computation and whether you want to continue the computation (in simple implementations you can always return true for that).
 *
 * Note on continuing the computation:
 *   in future I might implement some clock counting total time spent thinking or let your opponent think while you're thinking or something - right now it's needed for human interface to be bearable (TODO: what did I mean here?).
 *
 * After a move is calculated, =incrementMove= will be called repeatedly until you run out of time so in simple implementations you can ignore the time argument and just return a new move whenever you find a better one than the previous move.
 *
 * =initialMove= is called right after the opponent makes move to indicate that the state of the board changed.
 *
 * =incrementMove= is called if the state of computation in argument was calculated using the same match state (board hasn't changed since the last step of computation)
 * 
 * =firstMove= is supposed to propose the initial move and create the initial state of computation.
 * 
 * It would probably much easier for me to make just one function in this module with optional internalState and it would be easy to split that function into three in the implementation, but that would make unnecessary branching. It probably wouldn't matter in performance, but I'll leave it this way for now.
 *)
module type PLAYER =
  sig
    type internalState

    (* make your very first move in the match *)
    val firstMove : Shogi.matchState -> Timer.time
                    -> Shogi.move * internalState * bool

    (* This function will be called after the opponent's move *)
    val initialMove : Shogi.matchState -> internalState -> Timer.time
                      -> Shogi.move * internalState * bool

    (* try to find a better move than the last one *)
    val incrementMove : Shogi.matchState -> internalState -> Timer.time
                        -> Shogi.move * internalState * bool
                                                        
    val name : string
  end

let renamePlayer (f : string -> string) (module Player : PLAYER) : (module PLAYER) =
  (module struct
     include Player

     let name = f Player.name
   end)
           
