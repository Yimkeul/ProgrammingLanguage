type state = 
  | Q0
  | Q1
  | Q2
  | Q3
  | Q4
  | Q5

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
 
 let transfer state input =
   match state, input with
   |Q0, '0'..'9' -> Q1
   |Q1, '0'..'9' -> Q1
   |Q1, '+' -> Q2
   |Q1, '-' -> Q3
   |Q1, '*' -> Q4
   |Q1, '/' -> Q5
   |Q2, '0'..'9' -> Q1
   |Q3, '0'..'9' -> Q1
   |Q4, '0'..'9' -> Q1
   |Q5, '0'..'9' -> Q1
   |Q0, _-> failwith "rejected"
   |Q2, _ -> failwith "rejected"
   |Q3, _ -> failwith "rejected"
   |Q4, _ -> failwith "rejected"
   |Q5, _-> failwith "rejected"
   |_ -> failwith "rejected"


let tokenize s = 
  let lst = List.of_seq(String.to_seq s ) in 
  let rec tokenize2 char_lst state str str_lst : string list = 
    match char_lst with
    | [] -> (match state with
              | Q1 -> str :: str_lst
              | _ -> failwith "rejected")
    | h::t -> 
      let next_state = transfer state h in 
      begin
      match h with
      | '0'..'9' -> tokenize2 t next_state (str ^ String.make 1 h) str_lst
      | '+'|'-'|'*'|'/' -> tokenize2 t next_state "" ((String.make 1 h)::str::str_lst)
      | _ -> failwith "rejected"
      end
  in 
  List.rev  (tokenize2 lst Q0 "" [])
  (* match (tokenize2 lst Q0 "" []) with
    | [] -> failwith "rejected"
    | _ -> List.rev  (tokenize2 lst Q0 "" []) *)
  
  
  

let lex_to_expr (s : string) : expr = 
  (* 문자열 token화 *)
  let token_lst = tokenize s in 
  (* str_list : transfer 이후 문자열(token) str : 이전 숫자 *)
  let rec lex_to_expr2 str_list str :expr = 
    match str_list with
    | [] -> V (Int (int_of_string str))
    | h :: t -> 
      match h with
      | "+" -> E (ADD, (V (Int (int_of_string str))) , lex_to_expr2 t "")
      | "-" -> E (SUB, (V (Int (int_of_string str))) , lex_to_expr2 t "")
      | "*" -> E (MUL, (V (Int (int_of_string str))) , lex_to_expr2 t "")
      | "/" -> E (DIV, (V (Int (int_of_string str))) , lex_to_expr2 t "")
      | _ -> lex_to_expr2 t h
    in 
    lex_to_expr2 token_lst ""  
    
       

    




let lex s =
  let lst = List.of_seq(String.to_seq s) 
  in 
  let rec lex2 cur_state lst : state = 
    match lst with
    | h :: t -> lex2 (transfer cur_state h) t
    | [] -> cur_state
  in
  let st = lex2 Q0 lst in
  match st with
  | Q1 -> true
  | _ -> failwith "rejected"




let _ = assert (transfer Q0 '0' = Q1)
let _ = assert (transfer Q0 '9' = Q1)
let _ = assert (transfer Q1 '1' = Q1)
let _ = assert (transfer Q1 '+' = Q2)
let _ = assert (transfer Q1 '-' = Q3)
let _ = assert (transfer Q1 '*' = Q4)
let _ = assert (transfer Q1 '/' = Q5)
let _ = assert (transfer Q2 '1' = Q1)
let _ = assert (transfer Q3 '2' = Q1)
let _ = assert (transfer Q4 '3' = Q1)
let _ = assert (transfer Q5 '4' = Q1)
let _ = assert ((try transfer Q0 '+' with Failure _ -> Q0) = Q0)
let _ = assert ((try transfer Q0 'a' with Failure _ -> Q0) = Q0)
let _ = assert ((try transfer Q1 'a' with Failure _ -> Q0) = Q0)
let _ = assert ((try transfer Q2 '+' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q2 'a' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q3 '+' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q3 'a' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q4 '+' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q4 'a' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q5 '+' with Failure _ -> Q2) = Q2)
let _ = assert ((try transfer Q5 'a' with Failure _ -> Q2) = Q2)
let _ = assert (lex "1+23/0" = true)
let _ = assert (lex "000" = true)
let _ = assert (lex "0000010101203129309128390128390128903810938+1" = true)
let _ = assert (lex "1+1-2+1" = true)
let _ = assert (lex "1+1-2+1/1/1/1/1/1/1/11/1/1/1/1/1/1/1" = true)
let _ = assert (lex "100010010/0" = true)
let _ = assert (lex "0/0" = true)
let _ = assert (lex "00" = true)
let _ = assert (lex "0011123123412341234123412341234123412" = true)
let _ = assert (lex "001112312341234*1234123412341234123412" = true)
let _ = assert ((try lex "+" with Failure _ -> false) = false)
let _ = assert ((try lex "1.1" with Failure _ -> false) = false)
let _ = assert ((try lex "1+1+" with Failure _ -> false) = false)
let _ = assert ((try lex "1+" with Failure _ -> false) = false)
let _ = assert ((try lex "joejiowoi" with Failure _ -> false) = false)
let _ = assert ((try lex "1.1+20321" with Failure _ -> false) = false)
let _ = assert ((try lex "" with Failure _ -> false) = false)
let _ = assert ((try lex "1////////////////1" with Failure _ -> false) = false)
let _ = assert ((try lex "1/1/+1" with Failure _ -> false) = false)
let _ = assert ((try lex "1+010." with Failure _ -> false) = false)
let _ = assert ((try lex "." with Failure _ -> false) = false)
let _ = assert ((try lex "1.1" with Failure _ -> false) = false)
let _ = assert ((try lex "1a1" with Failure _ -> false) = false)
let _ = assert ((try lex "(0)" with Failure _ -> false) = false)
let _ = assert ((try lex "()" with Failure _ -> false) = false)
let _ = assert (tokenize "1+23/4" = [ "1"; "+"; "23"; "/"; "4" ])
let _ = assert (tokenize "1+23/4" = [ "1"; "+"; "23"; "/"; "4" ])
let _ = assert (tokenize "2" = [ "2" ])
let _ = assert (tokenize "123123123/123" = [ "123123123"; "/"; "123" ])
let _ = assert (tokenize "13/0" = [ "13"; "/"; "0" ])
let _ = assert (tokenize "00" = [ "00" ])
let _ = assert (tokenize "010101" = [ "010101" ])
let _ = assert (tokenize "1*1*1" = [ "1"; "*"; "1"; "*"; "1" ])

let _ =
  assert (tokenize "1+2/1/2+1" = [ "1"; "+"; "2"; "/"; "1"; "/"; "2"; "+"; "1" ])

let _ = assert ((try tokenize "1+" with Failure _ -> []) = [])
let _ = assert ((try tokenize "1+22+" with Failure _ -> []) = [])
let _ = assert ((try tokenize "1+_11123123" with Failure _ -> []) = [])
let _ = assert ((try tokenize "." with Failure _ -> []) = [])
let _ = assert ((try tokenize "1-1-1-1-1-21 2" with Failure _ -> []) = [])
let _ = assert ((try tokenize "1.1.1.1.1.1" with Failure _ -> []) = [])
let _ = assert ((try tokenize "-" with Failure _ -> []) = [])
let _ = assert ((try tokenize "hello" with Failure _ -> []) = [])
let _ = assert ((try tokenize "1**1" with Failure _ -> []) = [])
let _ = assert ((try tokenize "-1" with Failure _ -> []) = [])
let _ = assert ((try tokenize "-01" with Failure _ -> []) = [])
let _ = assert ((try tokenize "0/1//1" with Failure _ -> []) = [])

let _ =
  assert (
    lex_to_expr "1+23*4" = E (ADD, V (Int 1), E (MUL, V (Int 23), V (Int 4))) )

let _ =
  assert (
    lex_to_expr "1-23*4+5"
    = E (SUB, V (Int 1), E (MUL, V (Int 23), E (ADD, V (Int 4), V (Int 5)))) )

let _ = assert (lex_to_expr "1" = V (Int 1))
let _ = assert (lex_to_expr "123123123" = V (Int 123123123))

let _ =
  assert (lex_to_expr "1/1/1" = E (DIV, V (Int 1), E (DIV, V (Int 1), V (Int 1))) )

let _ =
  assert (
    lex_to_expr "1-12312-1"
    = E (SUB, V (Int 1), E (SUB, V (Int 12312), V (Int 1))) )

let _ =
  assert (lex_to_expr "1/1/1" = E (DIV, V (Int 1), E (DIV, V (Int 1), V (Int 1))) )

let _ = assert (lex_to_expr "1*2312" = E (MUL, V (Int 1), V (Int 2312)))

let _ =
  assert (
    (try lex_to_expr "1+" with Failure _ -> V (Int (-999))) = V (Int (-999)) )

let _ =
  assert (
    (try lex_to_expr "1.1+2.3" with Failure _ -> V (Int (-999)))
    = V (Int (-999)) )

let _ =
  assert (
    (try lex_to_expr "1.1" with Failure _ -> V (Int (-999))) = V (Int (-999)) )

let _ =
  assert (
    (try lex_to_expr "hello" with Failure _ -> V (Int (-999))) = V (Int (-999)) )

let _ =
  assert (
    (try lex_to_expr "-1" with Failure _ -> V (Int (-999))) = V (Int (-899)) )
