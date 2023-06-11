type t = (string * (string list * Ast.expr)) list
let empty : t = []

let rec find (f_name : string) (store : t) : string list * Ast.expr =
  match store with
  | [] -> failwith ("[Error] Unbound function: " ^ f_name)
  | (name, (params, body)) :: rest ->
      if name = f_name then
        (params, body)
      else
        find f_name rest


    let add (f_name : string) (params : string list) (body : Ast.expr) (store : t) : t =
      let rec remove_function (f_name : string) (store : t) : t =
        match store with
        | [] -> []
        | (name, _) :: rest ->
            if name = f_name then
              rest
            else
              (name, find name store) :: remove_function f_name rest
      in
      let new_store = remove_function f_name store in
      (f_name, (params, body)) :: new_store
    

(* let _ = assert( add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))])
let _ = assert (add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")));
("f", (["z"], Ast.Id "z"))] = [("f", (["x"; "y"], (Ast.Add (Ast.Id "x", Ast.Id "y")))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))])  *)

