type token =
  | TL of string  (* 0~9로 이루어진 숫자 *)
  | TO of string  (* 연산자 *)

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

let to_token str_list =
  List.map (fun str ->
    match Stdlib.int_of_string_opt str with
    | Some n -> TL (string_of_int n) (* 정수일 경우 TL 생성자를 사용하여 토큰 생성 *)
    | None -> TO str (* 문자일 경우 TO 생성자를 사용하여 토큰 생성 *)
  ) str_list



let to_postfix (lst : token list) : token list =
    let rec loop input_lst stack  =
      match input_lst with
      | [] -> List.rev stack
      | TL v :: tail  -> loop tail (TL v :: stack)
      | TO op :: tail ->
        match tail with
        | TL v :: stack_poped -> loop stack_poped (TO op :: TL v :: stack)
        | _ -> failwith "rejected"
      in
    loop lst []
    





let parse (lst : token list) : expr =
  let rec loop input_lst stack : expr=
    match input_lst with
    | [] -> failwith "ff"
    | TL v :: tail -> loop tail (V (Int (int_of_string v)) :: stack ) 
    | _ -> failwith "tt"
    (* | TO op :: tail ->
       match op with
       | "+" -> E (ADD, loop tail (stack),V (Int (int_of_string stack))   )
       | "-" -> E (SUB, loop tail (stack),V (Int (int_of_string stack))   )
       | "*" -> E (MUL, loop tail (stack),V (Int (int_of_string stack))   )
       | "/" -> E (DIV, loop tail (stack),V (Int (int_of_string stack))   )
       | _ -> failwith "rejected" *)

  in 
  loop lst []


(* let parse (lst : token list) : expr =
  let rec loop input_lst stack : expr =
    match input_lst with
    | [] -> List.hd stack
    | TL v :: tail -> loop tail (V (Int (int_of_string v)) :: stack)
    | TO op :: tail ->
       match op with
       | "+" -> let arg2 = List.hd stack in
                let arg1 = List.hd (List.tl stack) in
                let result = E (ADD, arg1, arg2) in
                loop tail (result :: List.tl (List.tl stack))
       | "-" -> let arg2 = List.hd stack in
                let arg1 = List.hd (List.tl stack) in
                let result = E (SUB, arg1, arg2) in
                loop tail (result :: List.tl (List.tl stack))
       | "*" -> let arg2 = List.hd stack in
                let arg1 = List.hd (List.tl stack) in
                let result = E (MUL, arg1, arg2) in
                loop tail (result :: List.tl (List.tl stack))
       | "/" -> let arg2 = List.hd stack in
                let arg1 = List.hd (List.tl stack) in
                let result = E (DIV, arg1, arg2) in
                loop tail (result :: List.tl (List.tl stack))
       | _ -> loop tail stack
  in 
  loop lst [] *)


  (* let parse (lst : token list) : expr =
    let rec loop input_lst stack : expr =
      match input_lst with
      | [] -> (match stack with
               | [x] -> x
               | _ -> failwith "rejected")
      | TL v :: tail -> loop tail (V (Int (int_of_string v)) :: stack)
      | TO op :: tail ->
         match stack with
         | v1 :: v2 :: stack' ->
            let op' = match op with
                      | "+" -> ADD
                      | "-" -> SUB
                      | "*" -> MUL
                      | "/" -> DIV
                      | _ -> failwith "rejected"
            in
            loop tail (E (op', v2, v1) :: stack')
         | _ -> failwith "rejected"
    in 
    loop lst [] *)
  