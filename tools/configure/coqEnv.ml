open CmdArgs.Prefs

(** NB: [arch_is_win32] is broader than [os_type_win32], cf. cygwin *)
let arch_is_win32 arch = arch = "win32"
let unix arch = Util.os_type_cygwin || not (arch_is_win32 arch)

(** Variable name, description, ref in prefs, default dir, prefix-relative *)

type path_style =
  | Absolute of string (* Should start with a "/" *)
  | Relative of string (* Should not start with a "/" *)

module InstallDir = struct

  type t =
    { var : string
    (** Makefile variable to write *)
    ; msg : string
    (** Description of the directory  *)
    ; uservalue : string option
    (** Value given explictly by the user *)
    ; selfcontainedlayout : path_style
    (** Path style when layout is "local" *)
    ; unixlayout : path_style
    (** Path style for installation *)
    }

  let make var msg uservalue selfcontainedlayout unixlayout =
    { var; msg; uservalue; selfcontainedlayout; unixlayout }

end

let install prefs =
  [ InstallDir.make "COQPREFIX" "Coq" prefs.prefix (Relative "") (Relative "")
  ; InstallDir.make "COQLIBINSTALL" "the Coq library" prefs.libdir (Relative "lib") (Relative "lib/coq")
  ; InstallDir.make "CONFIGDIR" "the Coqide configuration files" prefs.configdir (Relative "config") (Absolute "/etc/xdg/coq")
  ; InstallDir.make "DATADIR" "the Coqide data files" prefs.datadir (Relative "share") (Relative "share/coq")
  ; InstallDir.make "MANDIR" "the Coq man pages" prefs.mandir (Relative "man") (Relative "share/man")
  ; InstallDir.make "DOCDIR" "documentation prefix path for all Coq packages" prefs.docdir (Relative "doc") (Relative "share/doc")
  ]

let strip_trailing_slash_if_any p =
  if p.[String.length p - 1] = '/' then String.sub p 0 (String.length p - 1) else p

let use_suffix prefix = function
  | Relative "" -> prefix
  | Relative suff -> prefix ^ "/" ^ suff
  | Absolute path -> path

let relativize = function
  (* Turn a global layout based on some prefix to a relative layout *)
  | Relative _ as suffix -> suffix
  | Absolute path -> Relative (String.sub path 1 (String.length path - 1))

let find_suffix prefix path = match prefix with
  | None -> Absolute path
  | Some p ->
     let p = strip_trailing_slash_if_any p in
     let lpath = String.length path in
     let lp = String.length p in
     if lpath > lp && String.sub path 0 lp = p then
       Relative (String.sub path (lp+1) (lpath - lp - 1))
     else
       Absolute path

(* This computes the actual effective path for an install directory,
   based on the given prefix; if prefix is absent, it is assumed that
   the profile is "local" *)
let do_one_instdir ~prefix ~arch InstallDir.{var; msg; uservalue; selfcontainedlayout; unixlayout} =
  (var,msg),
  match uservalue, prefix with
  | Some d, p -> d, find_suffix p d
  | None, Some p ->
    let suffix = if (arch_is_win32 arch) then selfcontainedlayout else relativize unixlayout in
    use_suffix p suffix, suffix
  | None, None ->
    let suffix = if (unix arch) then unixlayout else selfcontainedlayout in
    let base = if (unix arch) then "/usr/local" else "C:/coq" in
    let dflt = use_suffix base suffix in
    let () = Printf.printf "Where should I install %s [%s]? " msg dflt in
    let line = read_line () in
    if line = "" then (dflt,suffix) else (line,find_suffix None line)

let install_dirs prefs arch =
  let prefix =
    match prefs.prefix with
    | None ->
      begin
        try Some (Sys.getenv "COQ_CONFIGURE_PREFIX")
        with
        | Not_found when prefs.interactive -> None
        | Not_found -> Some Sys.(getcwd () ^ "/../install/default")
      end
    | p -> p
  in
  List.map (do_one_instdir ~prefix ~arch) (install prefs)

let select var install_dirs = List.find (fun ((v,_),_) -> v=var) install_dirs |> snd

(** Coq core paths, for libraries, documentation, configuration, and data *)
type t =
  { coqlib : string
  ; coqlibsuffix : path_style
  ; docdir : string
  ; docdirsuffix : path_style
  ; configdir : string
  ; configdirsuffix : path_style
  ; datadir : string
  ; datadirsuffix : path_style }

let resolve_coqenv install_dirs =
  let coqlib, coqlibsuffix = select "COQLIBINSTALL" install_dirs in
  let docdir, docdirsuffix = select "DOCDIR" install_dirs in
  let configdir, configdirsuffix = select "CONFIGDIR" install_dirs in
  let datadir,datadirsuffix = select "DATADIR" install_dirs in
  { coqlib; coqlibsuffix; docdir; docdirsuffix
  ; configdir; configdirsuffix; datadir; datadirsuffix }
