(* 변수와 변수 주고값 매핑 스토어랑 같음 *)
type addr = int
type t = (string * addr) list

let empty :t =[]

(* find : string -> t -> addr *)
let rec find (name : string) (store : t) : addr =
  match store with
  | [] -> failwith ("[Error] Free identifier: " ^ name)
  | (n, v) :: tl ->
      if n = name then v
      else find name tl

(*add : string -> addr -> t -> t*)
let add (name : string) (value : addr) (store : t) : t =
  let rec remove_variable (name : string) (store : t) : t =
    match store with
    | [] -> []
    | (n, _) :: rest ->
        if n = name then
          rest
        else
          (n, find n store) :: remove_variable name rest
  in
  let new_store = remove_variable name store in
  (name, value) :: new_store



