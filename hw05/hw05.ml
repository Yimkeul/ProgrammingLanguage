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


let tokenize s = 
  let lst = List.of_seq(String.to_seq s ) in 
  (* char_lst = lst , state = 상태, str = 현재 타겟 , str_lst = 답 *)
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
    
       

    









  




  (* 
    let to_postfix (token_lst : token list ) : token list = 
      let st = Stack.create () in
    
      let rec to_postfix2 lst stack output =
        match lst with
        | [] -> begin
            while not (Stack.is_empty stack) do
              output := Stack.pop stack :: !output
            done;
            List.rev !output
          end
        | token :: tl -> begin
            match token with
            | TL _ -> 
                output := token :: !output;
                to_postfix2 tl stack output
            | TO _ ->
                while not (Stack.is_empty stack) &&
                      (match Stack.top stack with 
                      | TO o -> o <> "(" && (o = "+" || o = "-" || o = "*" || o = "/") | _ -> false) do
                  output := Stack.pop stack :: !output
                done;
                Stack.push token stack;
                to_postfix2 tl stack output
          end
    
      in to_postfix2 token_lst st (ref [])
    

      
      
      *)
      (* let parse = () *)
