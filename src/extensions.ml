module Array =
struct
  include Array

  let copy_matrix m =
    let l = Array.length m in
    if l = 0 then m else
      let result = Array.make l m.(0) in
      let rec cp_nth n =
	if n=l then result
	else
	  (result.(n) <- Array.copy m.(n);
	   cp_nth (n+1))
      in
      cp_nth 0
	
  let get_2d (m : 'a array array) ((y, x) : int * int) = m.(y).(x)

  let set_2d (m: 'a array array) ((y, x) : int*int) v = m.(y).(x) <- v
    
  let find (f : 'a -> bool) (arr: 'a array) =
    let max = Array.length arr in
    let rec check nth =
      if nth = max then failwith "Not_found"
      else
	if f arr.(nth) then arr.(nth)
	else check (nth + 1)
    in
    check 0

  let exists (f : 'a -> bool) (arr : 'a array) =
    try
      find f arr ;
      true
    with
    | Failure("Not_found") -> false

  let map_2d f mat =
    map
      (fun row ->
	map f row)
      mat

  let mapi_2d f mat =
    mapi
      (fun y row ->
	mapi (f y) row)
      mat

  let fold_2d_ll f acc mat =
    fold_left
      (fun acc row -> fold_left f acc row)
      acc
      mat

  let foldi_left f acc arr =
    fold_left (fun a (k, el)-> f a k el) acc (mapi (fun k x -> (k, x)) arr)

  let foldi_2d_ll f acc mat =
    fold_2d_ll (fun a ((x, y), e) -> f a y x e) acc (mapi_2d (fun y x el -> ((x, y), el)) mat)
       
end

module List =
struct
  include List

  let fold_left1 f xs =
    match xs with
    | x::xs -> fold_left f x xs
    | [] -> raise (Failure "Empty list to fold_left1")

  let fold_right1 f xs =
    match xs with
    | x::xs -> fold_right f x xs
    | [] -> raise (Failure "Empty list to fold_left1")
end
