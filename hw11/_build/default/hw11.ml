let rec interp (ast : Ast.expr) (store : Store.t) : Store.value =
    match ast with
    | Ast.Num n -> Store.NumV n
    | Ast.Id x -> Store.find x store
    | Ast.Add (e1,e2) -> 
        let v1 = interp e1 store in
        let v2 = interp e2 store in 
        ( match (v1, v2) with 
         | NumV n1, NumV n2 -> NumV( n1 + n2)
         | _, _ -> failwith (Format.asprintf "[Error] Not a number: %a + %a"  Ast.pp e1 Ast.pp e2)
        )
    | Ast.Sub (e1, e2) -> 
        let v1 = interp e1 store in
        let v2 = interp e2 store in 
        ( match (v1, v2) with 
         | NumV n1, NumV n2 -> NumV(n1 - n2)
         | _, _ -> failwith (Format.asprintf "[Error] Not a number: %a - %a"  Ast.pp e1 Ast.pp e2)
        )
    | Ast.LetIn (x, e1, e2) ->
        let v1 = interp e1 store in
        let new_store = Store.add x v1 store in
        interp e2 new_store    
    | Ast.App (e1, e2) ->
        let v1 = interp e1 store in
        let v2 = interp e2 store in
        (match v1 with
        | Store.ClosureV (x, body, closure) ->
            let new_store = Store.add x v2 closure in
            interp body new_store
        | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp e1))
    | Ast.Lambda (x, body) -> Store.ClosureV (x, body, store)
    | Ast.LessThan (e1, e2) ->
        let v1 = interp e1 store in
        let v2 = interp e2 store in
        (match (v1, v2) with
        | Store.NumV n1, Store.NumV n2 ->
            if n1 < n2 then interp Ast.true_encoding store else interp Ast.false_encoding store
            (* if n1 < n2 then Store.ClosureV ("x", Ast.Lambda("y", Ast.Id "x"), store) else Store.ClosureV ("x", Ast.Lambda("y", Ast.Id "y"), store) *)
        | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp e1 Ast.pp e2))

    (*
            Ast.Lambda("x", Ast.Lambda("y", Ast.Id "x"))
            Ast.Lambda("x", Ast.Lambda("x", Ast.Lambda("y", Ast.Id "x")) )
    *)
    (* | Ast.LessThan (e1, e2) ->
        let v1 = interp e1 store in
        let v2 = interp e2 store in
        (match (v1, v2) with
        | Store.NumV n1, Store.NumV n2 ->
            if n1 < n2 then Store.ClosureV ("x", Ast.true_encoding, store)
            else Store.ClosureV ("x", Ast.false_encoding, store)
        | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp e1 Ast.pp e2)) *)
    

(* let _ = assert (interp (ParserMain.parse "if true then 1 else 2") Store.empty  = Store.NumV 1) *)
(* let _ = assert (interp (ParserMain.parse "if (0 - 1) < (0 + 1) then 0 + 7 else 3") Store.empty = Store.NumV 7) *)
(* let _ = assert (interp (ParserMain.parse "1 + 2 < 3 + 4") Store.empty = Store.ClosureV ("x", Ast.Lambda ("y", Ast.Id "x"), [])) *)
(* let _ = assert (interp (ParserMain.parse "(if true then (fun x -> x - 1) else (fun x -> x + 1)) 2") Store.empty = Store.NumV 1) *)