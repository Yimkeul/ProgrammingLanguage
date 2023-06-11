(* value == 시그마 , 스토어의 원소 *)
type value = NumV of int

type t = (string * value) list
(* string : 변수 이름  , value : 변수 값 *)

let empty : t = []

let rec find (name : string) (store : t) : value =
  match store with
  | [] -> failwith ("[Error] Free identifier:" ^ name)
  | (n, v) :: tl ->
      if n = name then v
      else find name tl
   
let add (name : string) (value : value) (store : t) : t =
  (* name : 함수 이름 , value : 시그마 (store의 원소), store : 추상 메모리, t : (함수이름,시그마 튜플 구조인 리스트) *)
  let rec remove_variable (name : string) (store : t) : t =
    (* name : 함수이름 , store : 추상메모리 , t : 튜플 *)
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

        
(* let rec add (name : string) (value : value) (store : t) : t =
  match store with
  | [] -> [(name, value)]
  | (n, v) :: tl ->
      if n = name then List.rev_append tl ((name, value) :: tl)
      else List.rev_append tl ((n, v) :: add name value tl   ) *)


(* 
find "x" [("a", NumV 1); ("x", NumV 5)]

-> NumV 5

find "x" empty
error

add "x" (NumV 3) empty
 -> [(“x”, NumV 3)]

add "x" (NumV 3) [("y", NumV 1)] 
 -> [(“x”, NumV 3); (“y”, NumV 1)]

add "x" (NumV 3) [("y", NumV 1); ("x", NumV 0)] 
-> [(“x”, NumV 3); (“y”, NumV 1)]


*)

