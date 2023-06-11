(* module F = Format *)

(* let rec interp (ast :Ast.expr) : Value.t = 
  match ast with
  | Num n -> NumV n
  
  | Add (e1, e2) -> 
    let v1 = interp e1 in 
    let v2 = interp e2 in 
      match v1, v2 with
      | NumV n1, NumV n2 -> NumV (n1 + n2)
      | _ -> failwith "rr"
  | Sub (e1, e2) ->
    let v1 = interp e1 in 
    let v2 = interp e2 in 
    match v1, v2 with
      | NumV n1, NumV n2 -> NumV (n1 - n2)      
      | _ -> failwith "rr" *)
      
      let rec interp (ast :Ast.expr) : Value.t = 
        match ast with
        | Num n -> NumV n
        | Add (e1, e2) -> 
          let v1 = interp e1 in 
          let v2 = interp e2 in 
          (match v1, v2 with
            | NumV n1, NumV n2 -> NumV (n1 + n2))
        | Sub (e1, e2) ->
          let v1 = interp e1 in 
          let v2 = interp e2 in 
          (match v1, v2 with
            | NumV n1, NumV n2 -> NumV (n1 - n2)      
          )          


let _ = Format.printf "%a" Value.pp (interp (ParserMain.parse "1 + 2 - 3"))