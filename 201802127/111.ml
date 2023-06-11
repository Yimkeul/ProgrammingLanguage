let interp_def (def : Ast.fundef) (store : Fstore.t) : Fstore.t =
  match def with
  | Ast.FunDef (name, params, body) ->
      Fstore.add name params body store

  
  let rec interp_expr (expr : Ast.expr) (fstore : Fstore.t) (store : Store.t) : Store.value =
    match expr with
    | Ast.Num n -> Store.NumV n
    | Ast.Id name -> Store.find name store
    | Ast.Add (e1, e2) ->
        let v1 = interp_expr e1 fstore store in
        let v2 = interp_expr e2 fstore store in
        (match v1, v2 with
         | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)
         )
    | Ast.Sub (e1, e2) ->
        let v1 = interp_expr e1 fstore store in
        let v2 = interp_expr e2 fstore store in
        (match v1, v2 with
         | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2)
         )
    | Ast.LetIn (name, e1, e2) ->
        let value = interp_expr e1 fstore store in
        let new_store = Store.add name value store in
        interp_expr e2 fstore new_store
    | Ast.Call (name, args) ->
        let func = Fstore.find name fstore in
        match func with
        | (params, body) ->
            let arg_values = List.map (fun arg -> interp_expr arg fstore store) args in
            if List.length params <> List.length arg_values then
              failwith "[Error] Unmatched the number of arguments"
            else
              let new_store = List.fold_left2 (fun acc param value -> Store.add param value acc) store params arg_values in
              interp_expr body fstore new_store

              

let interp_prog (prog : Ast.prog) : Store.value =
  match prog with
  | Ast.Prog (fundefs, expr) ->
      let fstore = List.fold_left (fun acc def -> interp_def def acc) Fstore.empty fundefs in
      let store = Store.empty in
      interp_expr expr fstore store



