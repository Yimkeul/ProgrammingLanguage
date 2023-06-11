
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


(* let _ = assert( interp (ParserMain.parse "(fun x y -> x + y) 1 3") (Store.empty) = Store.NumV 4) *)
(* let _ = assert (interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") (Store.empty) = Store.NumV 4) *)


