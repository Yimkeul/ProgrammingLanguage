type op = 
  | ADD
  | SUB
  | MUL
  | DIV


type value = 
  | Int of int 
  | Float of float 
  | Err


type expr = 
  (* 튜플이여서 *로 연결 *)
  | E of op * expr * expr 
  | V of value


  
  let rec eval2 expr = 
    match expr with
    | V v -> v
    | E (op,e1,e2) -> 
      let e1 = eval2 e1 in
      let e2 = eval2 e2 in
      match (e1, e2) with
      | (Int i , Float f ) -> eval2(E (op, V (Float (float_of_int i)) , V (Float f)))
      | (Float f , Int i ) -> eval2(E (op,  V (Float f) , V (Float (float_of_int i))))
      | (Int i1, Int i2) ->
        begin match op with
        | ADD -> Int (i1 + i2)
        | SUB -> Int (i1 - i2)
        | MUL -> Int (i1 * i2)
        | DIV ->
          if i2 = 0 then Err
          else Int (i1 / i2)
        end
      | (Float f1, Float f2) ->
        begin match op with
        | ADD -> Float (f1 +. f2)
        | SUB -> Float (f1 -. f2)
        | MUL -> Float (f1 *. f2)
        | DIV ->
          if f2 = 0.0 then Err
          else Float (f1 /. f2)
        end      
      | _ -> Err



(* let eval expr =
  let rec eval_expr e =
    match e with
    | V v -> v
    | E (op, e1, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      match (v1, v2) with
      (* 치환 정수,실수 연산시 정수를 실수로 먼저 변경 *)
      | (Int i , Float f) ->
        eval_expr(E (op, V (Float (float_of_int i))  , V (Float f)))
      | (Float f , Int i) ->
        eval_expr(E (op, V (Float f), V (Float (float_of_int i))))

      | (Int i1, Int i2) ->
        begin match op with
        | ADD -> Int (i1 + i2)
        | SUB -> Int (i1 - i2)
        | MUL -> Int (i1 * i2)
        | DIV ->
          if i2 = 0 then Err
          else Int (i1 / i2)
        end
      | (Float f1, Float f2) ->
        begin match op with
        | ADD -> Float (f1 +. f2)
        | SUB -> Float (f1 -. f2)
        | MUL -> Float (f1 *. f2)
        | DIV ->
          if f2 = 0.0 then Err
          else Float (f1 /. f2)
        end      
      | _ -> Err
  in
  eval_expr expr *)

(* (eval2 (V (Int 3))) = Int 3
  (eval2 (V (Int 0))) = Int 0
    (eval2 (E (ADD, (V (Int 3)), (V (Float 4.1))))) = Float 7.1
    (eval2 (E (ADD, (V (Float 4.1)) , (V (Int 3))))) = Float 7.1
    (eval2 (E (MUL, (V (Int 2)), (V (Int 7))))) = Int 14
    (eval2 (E (MUL, (V (Int 2)), (V (Float 7.0))))) = Int 14
    (eval2 (E (DIV, (V (Float 3.2)), (E (SUB, (V (Int 2)), (V (Int 2)))))));; *)

    
  