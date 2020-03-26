let edeclare ?hook ~name ~poly ~scope ~kind ~opaque ~udecl ~impargs sigma body tyopt  =
  let info = DeclareDef.Info.make ~scope ~kind ~impargs ?hook ~opaque ~poly ~udecl () in
  DeclareDef.declare_definition ~name ~info ~types:tyopt ~body sigma

let declare_definition ~poly name sigma body =
  let udecl = UState.default_univ_decl in
  edeclare ~name ~poly ~scope:(DeclareDef.Global Declare.ImportDefaultBehavior)
    ~kind:Decls.(IsDefinition Definition) ~opaque:false ~impargs:[] ~udecl sigma body None
