type t = (Env.addr * Value.t) list
let empty : t = []

(* find : Env.addr -> t -> Value.t *)
let rec find (addr : Env.addr) (store : t) : Value.t =
  match store with
  | [] -> failwith (Format.asprintf "[Error] Free address: %d"  addr)
  | (n, v) :: tl ->
      if n = addr then v
      else find addr tl

(*add : Env.addr -> Value.t -> t -> t*)
let add (addr : Env.addr) (value : Value.t) (store : t) : t =
  let rec remove_variable (addr : Env.addr) (store : t) : t =
    match store with
    | [] -> []
    | (n, _) :: rest ->
        if n = addr then
          rest
        else
          (n, find n store) :: remove_variable addr rest
  in
  let new_store = remove_variable addr store in
  (addr, value) :: new_store



