(* open Store 
open Ast *)

let rec interp (e : Ast.expr) (store : Store.t) : Store.value =
  match e with
  | Num n -> NumV n
  | Id x -> Store.find x store
  | Add (e1, e2) ->
      let v1 = interp e1 store in
      let v2 = interp e2 store in
      (match (v1, v2) with 
      | NumV n1, NumV n2 -> NumV (n1 + n2) 
       )
  | Sub (e1, e2) ->
      let v1 = interp e1 store in
      let v2 = interp e2 store in
      (match (v1, v2) with NumV n1, NumV n2 -> NumV (n1 - n2) )
  | LetIn (x, e1, e2) ->
      let v1 = interp e1 store in
      let new_store = Store.add x v1 store in
      interp e2 new_store

(* 
interp (ParserMain.parse "let x = 3 in let y = 1 in x + y") Store.empty   
*)

(* let _ = assert (find "x" [ ("a", NumV 1); ("x", NumV 5) ] = NumV 5)

let _ =
  assert (add "x" (NumV 3) [ ("y", NumV 1) ] = [ ("x", NumV 3); ("y", NumV 1) ])

let _ = assert (interp (parse "let x = 3 in let y = 1 in x + y") empty = NumV 4) *)