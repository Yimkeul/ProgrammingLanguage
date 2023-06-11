type value = NumV of int

type t = (string * value) list

let empty : t = []

let rec find (name : string) (store : t) : value =
  match store with
  | [] -> failwith ("[Error] Free identifier: " ^ name)
  | (n, v) :: tl ->
      if n = name then v
      else find name tl

  let add (name : string) (value : value) (store : t) : t =
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
  