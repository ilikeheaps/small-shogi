open Lwt
open LTerm_widget
open Extensions

let string_of_field (field : Shogi.field) : string =
  match field with
  | Some (piece, _ (*player*)) -> Shogi.string_of_piece piece
  | None -> "  "

     
let button_matrix mat on_click =
  let vbox =
    Array.fold_left
      (* didn't work without explicit type for vbox *)
      (fun (vbox : vbox) row ->
	let hbox =
	  Array.fold_left
	    (* for some reason couldn't didn't work without the type for hbox *)
	    (fun (hbox : hbox) el ->
	      (* what would happen if expand was true? *)
	      hbox#add ~expand:false (new vline);
	      hbox#add ~expand:false el;
	      hbox)
	    (new hbox)
	    row
	in
	(* removing the first, unnecessary vline *)
	hbox#remove (List.hd hbox#children);
	vbox#add ~expand:false (new hline);
	vbox#add ~expand:false hbox;
	vbox)
      (new vbox)
      (Array.mapi
	 (fun y row ->
	   Array.mapi
	     (fun x label ->
	       let button = new button label in
	       button#on_click (on_click y x) ;
	       button)
	     row)
	 mat)
  in
  let frame = new frame in
  (* removing the first, unnecessary hline *)
  vbox#remove (List.hd vbox#children);
  frame#set vbox;
  frame

let button_col_lst ls on_click =
  let vbox =
    List.fold_left
      (fun (vbox : vbox) el ->
	vbox#add ~expand:false (new hline);
	vbox#add ~expand:false el;
	vbox)
      (new vbox)
      (List.mapi
	 (fun ind label ->
	   let button = new button label in
	   button#on_click (on_click ind);
	   button)
	 ls)
  in
  vbox#remove (List.hd vbox#children);
  vbox
    
let button_column arr on_click=
  let vbox =
    Array.fold_left
      (fun (vbox : vbox) el ->
	vbox#add ~expand:false (new hline);
	vbox#add ~expand:false el;
	vbox)
      (new vbox)
      (Array.mapi
	 (fun ind label ->
	   let button = new button label in
	   button#on_click (on_click ind);
	   button)
	 arr)
  in
  vbox#remove (List.hd vbox#children);
  vbox
  
let box_add_ls (box : LTerm_widget.box) ls sep =
  List.iteri
    (fun ind (name, on_click) ->
      (if ind = 0 then ()
       else box#add ~expand:false (sep ()));	  
      let button = new button name in
      button#on_click on_click;
      box#add ~expand:false button)
    ls
      
    
let button_row arr on_click =
  let hbox =
    Array.fold_left
      (fun (hbox : hbox) el ->
	hbox#add ~expand:false (new vline);
	hbox#add ~expand:false el;
	hbox)
      (new hbox)
      (Array.mapi
	 (fun ind label ->
	   let button = new button label in
	   button#on_click (on_click ind);
	   button)
	 arr)
  in
  hbox#remove (List.hd hbox#children);
  hbox

  
