let pp_arch arch ch =
  Printf.fprintf ch "%s" arch

let idearchdef ocamlfind arch =
  match arch with
    | "Darwin" ->
      let osxdir,_ = Conf.Util.tryrun ocamlfind ["query";"lablgtkosx"] in
      if osxdir <> "" then "QUARTZ" else "X11"
    | "win32" ->
      "WIN32"
    | _ -> "X11"

let write_configide coqenv browser arch idearchdef o =
  let { Conf.CoqEnv.configdir; datadir } = coqenv in
  let pr s = Printf.fprintf o s in
  let pr_s = pr "let %s = %S\n" in
  pr_s "configdir" configdir;
  pr_s "datadir" datadir;
  pr_s "arch" arch;
  pr_s "browser" browser;
  pr_s "gtk_platform" idearchdef;
  ()

let arch_is_win32 arch = arch = "win32"

(** * Browser command *)
let browser arch =
  match arch with
  | "Darwin" -> "open %s"
  | arch when arch_is_win32 arch -> "start %s"
  | _ -> "firefox -remote \"OpenURL(%s,new-tab)\" || firefox %s &"

let main () =
  let arch = Conf.Util.arch None in
  let idearch = idearchdef "ocamlfind" arch in
  (* XXX This is wrong *)
  let prefs = Conf.CmdArgs.parse_args () in
  let install_dirs = Conf.CoqEnv.install_dirs prefs arch in
  let coqenv = Conf.CoqEnv.resolve_coqenv install_dirs in
  Conf.Util.write_config_file ~file:"gtk_platform.conf" (pp_arch idearch);
  Conf.Util.write_config_file ~file:"config.ml" (write_configide coqenv (browser arch) arch idearch);
  ()

let () = main ()
