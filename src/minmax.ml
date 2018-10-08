open Extensions

(*
 * TODO: add some debug options, like:
module type DEBUGGER =
  sig
    type gameState
    type move
    val onEvaluate : move -> unit
    val onStuff : gameState -> unit
  end

and/or:
functor : GAMESEARCH -> DEBUGGER -> GAMESEARCH
 *)

(* TODO: is this even remotely useful? *)
module type TREETRAVERSAL =
  sig
    (* state of traversal *)
    type state

    val next : state -> state list
  end
  
module type GAME =
  sig
    (* gameState includes information from the perspective of current player *)
    type gameState
    type move (* = Shogi.move *) (* for debugging only *)

    val applyMove : gameState -> move -> gameState
    val moves : gameState -> move list
  end

module type GAMEEVALUATOR =
  sig
    type gameState
    type value

    val (<) : value -> value -> bool
    val inf : value
    val evalPosition : gameState -> value
  end

module type GAMESEARCH =
  sig
    type searchState
    type move
    type gameState

    val init : gameState -> searchState
    (* this should be a fast operation; only minimal needed work done *)
    val applyMove : searchState -> gameState -> move -> searchState
    val increment : searchState -> searchState * move
  end

(* simple minmax
 * It will try to make a deeper search at each =increment= and the state of computation will be the depth of last search
 *)
module Minmax
         (Game : GAME)
         (Evaluator : GAMEEVALUATOR with type gameState = Game.gameState)
       : (GAMESEARCH with type gameState = Game.gameState
                      and type move = Game.move) =
  struct
    open Game
    open Evaluator

    type move = Game.move
    type gameState = Game.gameState

    (* depth of last search and current game state *)
    (* TODO: pass the game tree and reuse it *)
    type searchState = int * gameState

    (* TODO move it to somewhere reasonable *)
    let undefined () = raise (Failure "welp")
    let fail str = raise (Failure str)

                 
    (* lazy tree with node and edge labels 
     * - always has one forced node (no empty trees)
     *)
    type ('a, 'b) lvetree = LNode of 'a * ('b * ('a, 'b) lvetree) list Lazy.t

    (* likewise, but not lazy *)
    type ('a, 'b) vetree = Node of 'a * ('b * ('a, 'b) vetree) list

    (* TODO why didn't compiler complain when it was "f y" instead of "g y"?
     *      It should see that types weren't right
     *)
    let map_pair : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd) =
      fun f g (x, y) -> (f x, g y)

    let id x = x
    let map_fst f x = map_pair f id x
    let map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b =
      fun f x -> map_pair id f x

    let rec takeLTree (n : int) (LNode (x, lazy children) : ('a, 'b) lvetree) : ('a, 'b) vetree =
      if n == 1
      then Node (x, [])
      else Node (x, List.map (map_snd (takeLTree (n-1))) children)
      
    (* regular tree with only node labels; non-empty *)
    type 'a vtree = Node of 'a * 'a vtree list

    let rec gameTree : gameState -> (gameState, move) lvetree =
      fun gs ->
      LNode (gs, lazy (List.map (fun move -> (move, gameTree (applyMove gs move))) (moves gs)))
   
    let maxmin : gameState -> int -> move =
      fun gs depth ->
      (takeLTree depth (gameTree gs)) ; undefined ()

    (* TODO
    let minP ((x, _) as e1) ((y, _) as e2) = if x < y then e1 else e2
    let maxP ((x, _) as e1) ((y, _) as e2) = if x < y then e2 else e1


    let min x y = if x < y then x else y
    let max x y = if x < y then y else x
      
    let rec foo : (gameState, move) vetree -> value * move =
      function
      | Node (x, []) -> evalPosition x ; undefined ()
      | (Node (x, cs)) ->
        (List.fold_left1
           minP
           ( List.map
               (fun (move, subtree) ->
                 (fst (foo subtree), move))
               cs ) )

    let rec foo1 : (value -> 'a) -> (gameState, move) vetree -> 'a =
      fun cont -> function
               | Node (x, []) -> cont (evalPosition x)
               | Node (x, cs) ->
                  (List.fold_left1
                     min
                     ( List.map
                         (fun (move, subtree) ->
                           foo1 cont subtree)
                         cs ) )
      
    let foo1' : (gameState, move) vetree -> value * move =
      function
      | Node (_, []) -> fail "nope"
      | Node (_, cs) ->
         (List.fold_left1
            minP
            (List.map
               (fun (move, subtree) ->
                 foo1 (fun v -> (v, move)) subtree)
            cs))
     *)
    let init g = (0, g)
    let applyMove s g _m = (0, g)
                         
    let increment (prevDepth, g) = let d = prevDepth + 1 in ((d, g),  maxmin g d)

  end
