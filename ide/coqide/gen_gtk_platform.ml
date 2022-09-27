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

let write_configide arch browser idearchdef o =
  let pr s = Printf.fprintf o s in
  let pr_s = pr "let %s = %S\n" in
  pr "let gtk_platform = `%s\n" idearchdef;
  pr_s "arch" arch;
  pr_s "browser" browser

(** NB: [arch_is_win32] is broader than [os_type_win32], cf. cygwin *)
let arch_is_win32 arch = arch = "win32"

(** * Browser command *)
let browser arch = match arch with
  | arch when arch_is_win32 arch -> "start %s"
  | arch when arch = "Darwin" -> "open %s"
  | _ -> "firefox -remote \"OpenURL(%s,new-tab)\" || firefox %s &"

let main () =
  let arch = Conf.Util.arch None in
  let idearch = idearchdef "ocamlfind" arch in
  let browser = browser arch in
  Conf.Util.write_config_file ~file:"gtk_platform.conf" (pp_arch idearch);
  Conf.Util.write_config_file ~file:"config.ml"
    (write_configide arch browser idearch);
  ()

let () = main ()
