type path_style =
  | Absolute of string (* Should start with a "/" *)
  | Relative of string (* Should not start with a "/" *)

type t =
  { coqlib : string
  ; coqlibsuffix : path_style
  ; docdir : string
  ; docdirsuffix : path_style
  ; configdir : string
  ; configdirsuffix : path_style
  ; datadir : string
  ; datadirsuffix : path_style }

val install_dirs :
  CmdArgs.Prefs.t
  -> string -> ((string * string) * (string * path_style)) list

val resolve_coqenv :
  ((string * 'a) * (string * path_style)) list -> t
