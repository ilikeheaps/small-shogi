open LTerm_widget
open Lwt

let select (options : (string * 'a) list) : 'a =
  let (waiter, wakener) = Lwt.wait ()
  in
  let vbox = new vbox in
  vbox#add (new LTerm_widget.t "glue_up") ;
  vbox#add ~expand:false (new hline) ;
  List.iter
    (fun (name, value) ->
      let button = new button name
      in
      button#on_click (fun () -> wakeup wakener value) ;
      vbox#add ~expand:false button ;
      vbox#add ~expand:false (new hline)
    )
    options ;
  vbox#add (new LTerm_widget.t "glue_down");
  Lwt_main.run
    (Lazy.force LTerm.stdout >>= fun term ->
     LTerm.enable_mouse term >>= fun () ->
     Lwt.finalize
       (fun () -> run term vbox waiter)
       (fun () -> LTerm.disable_mouse term))

(* TODO how to combine mouse selection with keyboard? *)
let choose (action_list : (string * (unit -> 'a)) list) : 'a =
  select action_list ()
