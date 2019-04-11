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

    val string_of_move : move -> string
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

 * TODO better minmaxes:
 * 1) state = (int, game tree)
 * 2) state = (game tree, list of leaf nodes on last level)
      But you need to discard a part of them when applying a move but you can't decide which exactly!
 * 3) alpha-beta pruning
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


    let log str =
      let out = open_out_gen [Open_append] 0 "minmax.log" in
      output_string out (str^"\n");
      close_out out

    (* TODO move it to somewhere reasonable *)
    (* TODO is there any difference from 'failwith'? *)
    let undefined _ = raise (Failure "welp")
    let fail str = raise (Failure str)
    let rec fix (f : ('a -> 'b) -> 'a -> 'b ) : 'a -> 'b =
      f (fun x -> fix f x)

    (* lazy tree with node and edge labels
     * - always has one forced node (no empty trees)
     * TODO children could be a lazy stream for finer laziness
     *)
    type ('a, 'b) lvetree = LNode of 'a * ('b * ('a, 'b) lvetree) list Lazy.t

    let ltVal (LNode(x, _)) = x

    (* likewise, but not lazy *)
    type ('a, 'b) vetree = Node of 'a * ('b * ('a, 'b) vetree) list

    type gameTree = (gameState, move) lvetree

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

    let rec gameTree : gameState -> gameTree =
      fun gs ->
      LNode (gs, lazy (List.map (fun move -> (move, gameTree (applyMove gs move))) (moves gs)))

    let some x = Some x
    let option : 'b -> ('a -> 'b) -> 'a option -> 'b =
      fun def f -> function
                | Some x -> f x
                | None -> def

    let map_opt : ('a -> 'b option) -> 'a list -> 'b list =
      fun f xs ->
      let rec aux acc = function
        | x::xs -> aux (option acc (fun x' -> (x'::acc)) (f x)) xs
        | [] -> acc
      in
      aux [] xs

    (* TODO work from here *)
    let chooser : ('a -> 'a -> bool) -> ('a * 'c) -> ('a * 'c) -> ('a * 'c) =
      fun gt p1 p2 ->
      if gt (fst p1) (fst p2) (* x1 > x2 *)
      then p1
      else p2
    (* aliases just to make following type shorter *)
    type moval = value * move
    type 'a binop = 'a -> 'a -> 'a
    (* if depth is 0, return the value; otherwise fold values from cont on children *)
    (* TODO what should you return for no possible move? *)
    (*      -> this function should probably have a different type *)
    (*      -> maybe tree type should have a leaf option for clearer code *)
    let fold_step : int -> moval binop -> (move * gameTree -> moval) -> (move * gameTree)
                       -> moval =
      fun depth choose cont (move, LNode(x, lchildren)) ->
      if depth = 0
      then (evalPosition x, move)
      else let lazy children = lchildren in
           match List.map cont children with
           | [] -> (evalPosition x, move) (* NOPE this is opponents move *)
           | x::xs -> List.fold_left choose x xs
    let fold_step' =
      fun depth choose cont (move, LNode(x, lchildren)) ->
      if depth = 0 then (evalPosition x, move)
      else List.fold_left choose (failwith "no score", failwith "no move") (Lazy.force lchildren)
    let fold_left1_opt f = function
      | [] -> None
      | x::xs -> Some (List.fold_left f x xs)
    (* note that move from arguments is only used to return stuff from parent call *)
    let foldl_step'' : int
                       -> moval binop
                       -> (gameTree -> moval option)
                       -> gameTree
                       -> moval option =
      fun depth choose cont (LNode(x, lchildren)) ->
      fold_left1_opt choose
        (map_opt
           (fun (m, (LNode(x, _) as t)) ->
             if depth = 1
             then Some (evalPosition x, m)
             else cont t)
           (Lazy.force lchildren))

    (* but it seems it can be usable *)
    let foo : (int -> move * gameTree -> moval) -> int -> (move * gameTree) -> moval =
      fun cont depth ->
      fold_step depth (chooser (<)) (cont (depth - 1))
    (* like so *)
    let bar = fix foo


    (* TODO this is the last version of minmax *)
    let step selector cont depth (LNode(x, lchildren)) =
      if depth = 0
      then evalPosition x
      else
        match fold_left1_opt selector (List.map (fun (m, t) -> cont (depth-1) t) (Lazy.force lchildren)) with
        | None -> evalPosition x
        | Some v -> v

    let max x y = if x < y then y else x
    let min x y = if x < y then x else y

    let maxP ((x, _) as p1) ((y, _) as p2) = if x < y then p2 else p1
    let minP ((x, _) as p1) ((y, _) as p2) = if x < y then p1 else p2

    let maxmin : gameState -> int -> move =
      fun gs depth ->
      let LNode(x, lazy lchildren) = gameTree gs in
      snd (List.fold_left1 maxP
             (List.map
                (fun (m, t) -> (fix (fun cont -> step min (step max cont)) (depth-1) t, m))
                lchildren))

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
    let applyMove s g m =
      log "move";
      (0, g)

    let increment (prevDepth, g) =
      let d = prevDepth + 5 in
      let m = maxmin g d in
      log ("incr: depth "^string_of_int d);
      log ("  move: "^string_of_move m);
      ((d, g),  m)

  end
