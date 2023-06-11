let rec interp_expr (ast : Ast.expr) ( (e , m): (Env.t *  Memory.t)) : Value.t = 
  match ast with
  | Ast.Num n -> Value.NumV n
  | Ast.Ref r ->  Value.AddrV(Env.find r e)
  | Ast.Id x -> let addr = Env.find x e 
                in Memory.find addr m
  | Ast.Bool b -> Value.BoolV b
  | Ast.Add (e1, e2) ->
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    (
      match (v1, v2) with
      | NumV n1, NumV n2 -> NumV( n1 + n2)
       | _, _ -> failwith (Format.asprintf "[Error] Not a number: %a + %a"  Ast.pp_expr e1 Ast.pp_expr e2)
    )
  | Ast.Sub (e1, e2) ->
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    (
      match (v1, v2) with
      | NumV n1, NumV n2 -> NumV( n1 - n2)
      | _, _ -> failwith (Format.asprintf "[Error] Not a number: %a - %a"  Ast.pp_expr e1 Ast.pp_expr e2)
    )
  | Ast.Lt (e1,e2) -> 
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    (
      match (v1, v2) with
      | NumV n1, NumV n2 ->  BoolV(n1 < n2)
      | _, _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a"  Ast.pp_expr e1 Ast.pp_expr e2)
    )
  | Ast.Gt (e1,e2) -> 
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    (
      match (v1, v2) with
      | NumV n1, NumV n2 ->  BoolV(n1 > n2)
      | _, _ -> failwith (Format.asprintf "[Error] Not a number: %a > %a"  Ast.pp_expr e1 Ast.pp_expr e2)
    )
  | Ast.Eq (e1,e2) -> 
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    BoolV(v1 = v2)
  | Ast.And (e1,e2) -> 
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    (
      match (v1, v2) with
      | BoolV b1, BoolV b2 ->  BoolV(b1 && b2)
      | _, _ -> failwith (Format.asprintf "[Error] Not a boolean: %a > %a"  Ast.pp_expr e1 Ast.pp_expr e2)
    )
  | Ast.Or (e1,e2) -> 
    let v1 = interp_expr e1 (e,m) in
    let v2 = interp_expr e2 (e,m) in
    (
      match (v1, v2) with
      | BoolV b1, BoolV b2 ->  BoolV(b1 || b2)
      | _, _ -> failwith (Format.asprintf "[Error] Not a boolean: %a > %a"  Ast.pp_expr e1 Ast.pp_expr e2)
    )

let rec interp_stmt (ast : Ast.stmt) ( (e , m): (Env.t *  Memory.t)) : (Env.t *  Memory.t) = 
match ast with
  | Ast.DefStmt s1 -> Env.add s1 (AddrManager.new_addr ()) e , m

  (* *e1 = e2 *)
  | Ast.StoreStmt (e1, e2) ->
    let addr =
      match  interp_expr e1 (e, m) with
      | Value.AddrV a -> a
      | _ -> failwith (Format.asprintf "[Error] Not an address: %a" Ast.pp_expr e1)
    in
    let v = interp_expr e2 (e, m) in
    (e, Memory.add addr v m)


  (* x = *e *)
  | Ast.LoadStmt (s1, e1) ->
    let x = Env.find s1 e in 
    let addr = 
      (match interp_expr e1 (e,m) with
      | Value.AddrV a -> a
      | _ -> failwith (Format.asprintf "[Error] Not an address: %a" Ast.pp_expr e1)
      )
    in 
    let addr' = Memory.find addr m in
    (e, Memory.add x addr' m)

  (* e? s1^- : s2^- *)
     | Ast.IfStmt  (e1,slst1, slst2) ->  
      let rec interp_stmt_list (slst : Ast.stmt list) ((e, m) : (Env.t * Memory.t)) : (Env.t * Memory.t) =
        (match slst with
        | [] -> (e, m)
        | stmt :: rest ->
          let (e', m') = interp_stmt stmt (e, m) in
          interp_stmt_list rest (e', m') )
          (* stmt_list 정의 끝 *)
        in
         let (_,m') = ( match interp_expr e1 (e, m) with
            | Value.BoolV true -> interp_stmt_list slst1 (e, m)
            | Value.BoolV false -> interp_stmt_list slst2 (e, m) 
            | _ -> failwith (Format.asprintf "[Error] Not a boolean: %a" Ast.pp_expr e1)
         )
      in (e, m')

  (* while e slst *)
  | Ast.LoopStmt (e1, slst) ->
    let rec interp_stmt_list (slst : Ast.stmt list) ((e, m) : (Env.t * Memory.t)) : (Env.t * Memory.t) =
      (match slst with
      | [] -> (e, m)
      | stmt :: rest ->
        let (e', m') = interp_stmt stmt (e, m) in
        interp_stmt_list rest (e', m')) in
        (* stmt_list 정의 끝 *)
    let rec loop (e, m) =
        match interp_expr e1 (e, m) with
            | Value.BoolV true ->
              let (_, m') = interp_stmt_list slst (e, m) in
              loop (e, m')
            | Value.BoolV false -> (e, m)
            | _ -> failwith (Format.asprintf "[Error] Not a boolean: %a" Ast.pp_expr e1)
        in loop (e, m)    
      

    

let interp_prog (ast : Ast.prog)  : (Env.t *  Memory.t) = 
let rec interp_stmt_list (slst : Ast.stmt list) ((e, m) : (Env.t * Memory.t)) : (Env.t * Memory.t) =
  match slst with
  | [] -> (e, m)
  | stmt :: rest ->
    let (e', m') = interp_stmt stmt (e, m) in
    interp_stmt_list rest (e', m') in
match ast with
| Ast.Program slst -> interp_stmt_list slst (Env.empty, Memory.empty)



(* 
(* interp_prog (ParserMain.parse prog);;  *)
let prog = "def x; x = 3;" 
let _ = assert (interp_prog (ParserMain.parse prog) = ([("x", 0)], [(0, Value.NumV 3)])) 
let _ = AddrManager.init ()
(* -> ([("x", 0)], [(0, Value.NumV 3)])) *)

let prog = "def x; def y; x = 1; if (x < 3) { y = 1; } else { y = 99; }"
let _ = assert (interp_prog (ParserMain.parse prog) = ([("y", 1); ("x", 0)], [(1, Value.NumV 99); (0, Value.NumV 3)])) 
let _ = AddrManager.init ()
  (* -> ([("y", 1); ("x", 0)], [(1, Value.NumV 99); (0, Value.NumV 3)]) *)

let prog = "def pc; def c; def z; c = 5; pc = &c;"
let _ = assert (interp_prog (ParserMain.parse prog) = ([("z", 2); ("c", 1); ("pc", 0)], [(0, Value.AddrV 1); (1,Value.NumV 5)])) 
let _ = AddrManager.init ()
(* -> ([("z", 2); ("c", 1); ("pc", 0)], [(0, Value.AddrV 1); (1,Value.NumV 5)]) *)
let prog = "def x; def y; x = 3; y = 0; while x > 0 { y = y + x; x = x - 1;}"
let _ = assert(interp_prog (ParserMain.parse prog) = ([("y", 1); ("x", 0)], [(0, Value.NumV 0); (1, Value.NumV 6)]))
let _ = AddrManager.init ()
(* -> ([("y", 1); ("x", 0)], [(0, Value.NumV 0); (1, Value.NumV 6)]) *)
 *)


 (* let prog = "def x; def y; x = 1; y = 1; if (x == y) {x = 5;} else{y = 5;}" *)