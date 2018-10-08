

(** button_matrix m f creates a vbox (of hboxes) of buttons, where button in (y, x) cell (coordinates starting in upper left corner) has text of m.(y).(x) and on_click function of (f y x) **)
val button_matrix :
  string array array ->
  (int -> int -> unit -> unit)
  -> LTerm_widget.frame

val button_column : string array -> (int -> unit -> unit) -> LTerm_widget.vbox

val button_col_lst : string list -> (int -> unit -> unit) -> LTerm_widget.vbox

val button_row : string array -> (int -> unit -> unit) -> LTerm_widget.hbox

val box_add_ls : LTerm_widget.box -> (string * (unit -> unit)) list -> (unit -> LTerm_widget.t) -> unit
