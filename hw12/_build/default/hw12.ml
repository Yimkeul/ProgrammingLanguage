let rec interp (ast : Ast.expr) (store : Store.t) : Store.value =
  match ast with
  | Ast.Num n -> Store.NumV n
  | Ast.Id x ->
    (
    match Store.find x store with
    | Store.FreezedV (expr, store) -> interp expr store
    (* 언 경우 *)
    | value -> value 
    (* 얼지 않은 경우 *)
    )

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
  | Ast.RLetIn (x,e1,e2) ->  
        (
         match interp e1 store with
        | Store.ClosureV (x',e, s') ->
            let rec s'' = (x, Store.ClosureV (x',e,s'')) :: s' in
            interp e2 s''
        | _ -> failwith (Format.asprintf "[Error] Not a function: %a"  Ast.pp e1 )
        )
    
  | Ast.App (e1, e2) ->
      let v1 = interp e1 store in
      let v2 =  Store.FreezedV(e2, store) in
     ( match v1 with
      | Store.ClosureV (x,body, closure) ->
        let store' = Store.add x v2 closure in
        interp body store'
      | _  -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp e1) 
     )
      
  | Ast.Lambda (x, body) -> Store.ClosureV (x, body, store)
  | Ast.LessThan (e1, e2) ->
      let v1 = interp e1 store in
      let v2 = interp e2 store in
      (match (v1, v2) with
      | Store.NumV n1, Store.NumV n2 ->
          if n1 < n2 then interp Ast.true_encoding store else interp Ast.false_encoding store
          (* if n1 < n2 then Store.ClosureV ("x", Ast.Lambda("y", Ast.Id "x"), store) else Store.ClosureV ("x", Ast.Lambda("y", Ast.Id "y"), store) *)
      | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp e1 Ast.pp e2))

(* let prog = "let rec sum = (fun x -> if (x < 1) then 0 else x + (sum (x - 1))) in sum 10" *)
(* let _ = assert(interp (ParserMain.parse prog) Store.empty = Store.NumV 55) *)
