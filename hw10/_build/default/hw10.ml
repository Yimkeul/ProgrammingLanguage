
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



let _ = 
    assert (interp (ParserMain.parse "(fun x -> x + 10) 10") [] = Store.NumV 20);
    assert (interp (ParserMain.parse "let f = (fun x -> x + 10) in f 10") [] = Store.NumV 20);
    assert (interp (ParserMain.parse "(fun x -> x + 10) 10") Store.empty = Store.NumV 20);
    assert (interp (ParserMain.parse "(fun x -> x + 10) y") [("y", Store.NumV 1)] = Store.NumV 11);
    assert (interp (ParserMain.parse "let f = (fun x -> x + 10) in f 10") Store.empty = Store.NumV 20);
    assert (interp (ParserMain.parse "(fun x y  -> x + y) 1 3") [] = Store.NumV 4);
    assert (interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") [] = Store.NumV 4);
    assert (interp (ParserMain.parse "(fun x y  -> x + y) 1 3") Store.empty = Store.NumV 4);
    assert (interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") Store.empty = Store.NumV 4);
    assert (interp (ParserMain.parse "(fun x y z -> x + y - z) 5 5 5") [] = Store.NumV 5);
    assert (interp (ParserMain.parse "let f = (fun x y z -> x + y - z) in f 5 5 5") [] = Store.NumV 5);
    assert (interp (ParserMain.parse "let f = (fun x y z -> x + y - z) in let y = 5 in (f 5 y 5)") [] = Store.NumV 5);  
    assert (try (interp (ParserMain.parse "(x + y)") [("x", Store.NumV 1)]) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: y");
    assert (try (interp (ParserMain.parse "let f = (fun x -> x + 10) in x + f") [("x", Store.NumV 1)]) = Store.NumV (-1) with Failure str -> str = "[Error] Not a number: x + f");
    assert (try (interp (ParserMain.parse "let f = (fun x -> x + 10) in f + f") Store.empty) = Store.NumV (-1) with Failure str -> str = "[Error] Not a number: f + f");
    assert (try (interp (ParserMain.parse "let f = (fun x -> x + 10) in x - f") [("x", Store.NumV 1)]) = Store.NumV (-1) with Failure str -> str = "[Error] Not a number: x - f");
    assert (try (interp (ParserMain.parse "let f = (fun x -> x + 10) in f - f") Store.empty) = Store.NumV (-1) with Failure str -> str = "[Error] Not a number: f - f");
    assert (try (interp (ParserMain.parse "x 1") [("x", Store.NumV 1)]) = Store.NumV (-1) with Failure str -> str = "[Error] Not a function: x");