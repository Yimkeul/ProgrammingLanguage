let interp_def (def : Ast.fundef) (store : Fstore.t) : Fstore.t =
  match def with
  | Ast.FunDef (name, params, body) ->
      Fstore.add name params body store

  let rec interp_expr (e : Ast.expr) (fstore : Fstore.t) (store : Store.t) : Store.value =
    match e with
    | Num n -> Store.NumV n
    | Id x -> Store.find x store
    | Add (e1, e2) ->
      let v1 = interp_expr e1 fstore store in
      let v2 = interp_expr e2 fstore store in
      (
        match (v1, v2) with
        | NumV v1, NumV v2 -> NumV (v1 + v2)
      )
    | Sub (e1, e2) ->
      let v1 = interp_expr e1 fstore store in
      let v2 = interp_expr e2 fstore store in
      (
        match (v1, v2) with
        | NumV v1, NumV v2 -> NumV (v1 - v2)
      )
    | LetIn (x, e1, e2) ->
      let v1 = interp_expr e1 fstore store in
      let new_store = Store.add x v1 store in
      interp_expr e2 fstore new_store
    | Call (x, elst) ->
      (
        match Fstore.find x fstore with
        | (x1, e1) ->
          let rec tstore p_lst v_lst temp =
            match p_lst, v_lst with
            | [], [] -> temp
            | h1 :: t1, h2 :: t2 ->
              tstore t1 t2 (Store.add h1 (interp_expr h2 fstore store) temp)
            | _, _ -> failwith "[Error] Unmatched the number of arguments"
          in let sigma1 = tstore x1 elst Store.empty
          in interp_expr e1 fstore sigma1
      )
let interp_prog (prog : Ast.prog) : Store.value =
  match prog with
  | Ast.Prog (fundefs, expr) ->
      let fstore = List.fold_left (fun acc def -> interp_def def acc) Fstore.empty fundefs in
      let store = Store.empty in
      interp_expr expr fstore store

(*       
      let _ = 
        (* 실습 파일에 있는 테스트 코드 *)
        assert (Fstore.find "f" [("f", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = (["x"], Ast.Add (Ast.Num 1, Ast.Id "x"))) ;
        assert (try let _ = Fstore.find "f" Fstore.empty in false with Failure str -> str = "[Error] Unbound function: f") ;
        assert (Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]) ;
        assert (Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]) ;
        assert (Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x"))); ("f", (["z"], Ast.Id "z"))] = [("f", (["x"; "y"], (Ast.Add (Ast.Id "x", Ast.Id "y")))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]) ;
        assert (interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = Store.NumV 4) ;
        assert (interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]) ; 
        assert (interp_expr (Ast.Add (Ast.Call ("f", [Ast.Num 3; Ast.Num 2]), Ast.Id "x")) [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))] [("x", Store.NumV 10)] = Store.NumV 15) ;
        
      
      
        (**** test structures ****)
        let test_fstore : Fstore.t = [
          ("func_with_1_param", (["param1"], Ast.Num 1)) ;
          ("func_with_2_param", (["param1"; "param2"], Ast.Num 2)) ;
          ("func_with_3_param", (["param1"; "param2"; "param3"], Ast.Num 3)) ;
          ("func_with_0_param_free_id", ([], Ast.Id "param_free")) ;
        ] in
        (**** test structures end ****)
        
        (**** fstore.ml ***)
        (* Fstore.empty *)
        assert (Fstore.empty = []) ;
        (* Fstore.find *)
        assert ((Fstore.find "func_with_1_param" test_fstore) = (["param1"], Ast.Num 1)) ;
        assert ((Fstore.find "func_with_2_param" test_fstore) = (["param1"; "param2"], Ast.Num 2)) ;
        assert ((Fstore.find "func_with_3_param" test_fstore) = (["param1"; "param2"; "param3"], Ast.Num 3)) ;
        assert ((Fstore.find "func_with_0_param_free_id" test_fstore) = ([], Ast.Id "param_free")) ;
        assert (try let _ = Fstore.find "not_found" test_fstore in false with Failure str -> str = "[Error] Unbound function: not_found") ;
        assert (try let _ = Fstore.find "func_with_010101010" test_fstore in false with Failure str -> str = "[Error] Unbound function: func_with_010101010") ;
        assert (try let _ = Fstore.find "0101010" test_fstore in false with Failure str -> str = "[Error] Unbound function: 0101010") ;
        assert (try let _ = Fstore.find "func_with_1_param" [] in false with Failure str -> str = "[Error] Unbound function: func_with_1_param") ;
        (* fstore.ml end *)
        (* Fstore.add *)
        assert (Fstore.add "f1" [] (Ast.Num 1) [] = [("f1", ([], Ast.Num 1))]) ;
        assert (Fstore.add "f1" [] (Ast.Num 1) test_fstore = ("f1", ([], Ast.Num 1)) :: test_fstore) ;
        assert (Fstore.add "func_with_1_param" [] (Ast.Num 1) test_fstore = match test_fstore with [] -> [] | _ :: t -> (("func_with_1_param", ([], Ast.Num 1)) :: t)) ;
        assert (Fstore.add "func_with_3_param" [] (Ast.Num 1) test_fstore = [
          ("func_with_3_param", ([], Ast.Num 1)) ;
          ("func_with_1_param", (["param1"], Ast.Num 1)) ;
          ("func_with_2_param", (["param1"; "param2"], Ast.Num 2)) ;
          ("func_with_0_param_free_id", ([], Ast.Id "param_free")) ;
        ]) ;
        assert (Fstore.add "func_with_4_param" [] (Ast.Num 1) test_fstore = [
          ("func_with_4_param", ([], Ast.Num 1)) ;
          ("func_with_1_param", (["param1"], Ast.Num 1)) ;
          ("func_with_2_param", (["param1"; "param2"], Ast.Num 2)) ;
          ("func_with_3_param", (["param1"; "param2"; "param3"], Ast.Num 3)) ;
          ("func_with_0_param_free_id", ([], Ast.Id "param_free")) ;
        ]) ;
        (* Fstore.add end *)
      
        (* store.ml *)
        assert (Store.find "x" [("a", Store.NumV 1); ("x", Store.NumV 5)] = Store.NumV 5);
        assert (Store.find "xxx" [("x", Store.NumV 2); ("xx", Store.NumV 3); ("xxx", Store.NumV 999); ("xxxx", Store.NumV 998)] = Store.NumV 999);
        assert (Store.find "y" [("y", Store.NumV 1); "xy", Store.NumV 2] = Store.NumV 1);
        assert (Store.find "xyxy" [("xyxy", Store.NumV 111)] = Store.NumV 111);
        assert (Store.find "asd123" [("asd123", Store.NumV 0)] = Store.NumV 0);
        assert (try (Store.find "x" Store.empty) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: x");
        assert (try (Store.find "variable" [("a", Store.NumV 1); ("b", Store.NumV (2)); ("variabl", Store.NumV 33)]) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: variable");
        assert (try (Store.find "asdf" [("asd", Store.NumV 1); ("asdz", Store.NumV 123); ("ssss", Store.NumV 111)]) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: asdf");
      
        assert (Store.add "x" (Store.NumV 3) Store.empty = [("x", Store.NumV 3)]);
        assert (Store.add "x" (Store.NumV 3) [("y", Store.NumV 1)] = [("x", Store.NumV 3); ("y", Store.NumV 1)]);
        assert (Store.add "x" (Store.NumV 3) [("y", Store.NumV 1); ("x", Store.NumV 0)] = [("x", Store.NumV 3); ("y", Store.NumV 1)]);
        assert (Store.add "xy" (Store.NumV 123) [] = [("xy", Store.NumV 123)]);
        assert (Store.add "xxx" (Store.NumV 1) [("a", Store.NumV 2); ("x", Store.NumV 3); ("xxx", Store.NumV 4); ("xxxx", Store.NumV 5)] = [("xxx", Store.NumV 1); ("a", Store.NumV 2); ("x", Store.NumV 3); ("xxxx", Store.NumV 5)]);
        assert (Store.add "hi" (Store.NumV 123) [("xxx", Store.NumV 1); ("a", Store.NumV 2); ("x", Store.NumV 3); ("xxxx", Store.NumV 5)] = [("hi", Store.NumV 123); ("xxx", Store.NumV 1); ("a", Store.NumV 2); ("x", Store.NumV 3); ("xxxx", Store.NumV 5)]);
      
        (* hw09.ml *)
        (* interp_prog *)
        assert (try let _ = interp_prog (ParserMain.parse "def f1 x y = x + a endef def f2 x y = x - y endef let a = 3 in let b = f1(a, 4) - f2(4, 7) in a + b") in false with Failure str -> str = "[Error] Free identifier: a") ;
        assert (interp_prog (ParserMain.parse "def f1 x = x endef def f2 x y = x - y + f1(x) endef f1(3) + f2(4, 7)") = Store.NumV 4) ;
        assert (interp_prog (ParserMain.parse "def f2 x y = x - y + f1(x) endef def f1 x = x endef f1(3) + f2(4, 7)") = Store.NumV 4) ;
        assert (try let _ = interp_prog (ParserMain.parse "def f1 x y = x + y endef f1(1)") in false with Failure str -> str = "[Error] Unmatched the number of arguments") ;
        assert (try let _ = interp_prog (ParserMain.parse "def f1 x y = x + y endef f1(1, 2, 3)") in false with Failure str -> str = "[Error] Unmatched the number of arguments") ;
        assert (try let _ = interp_prog (ParserMain.parse "def f1 x y = x + y endef f1()") in false with Failure str -> str = "[Error] Unmatched the number of arguments") ;
        assert (try let _ = interp_prog (ParserMain.parse "def f1 = 0 endef f1(1)") in false with Failure str -> str = "[Error] Unmatched the number of arguments") ;
        assert (try let _ = interp_prog (ParserMain.parse "def f1 = 0 endef f1(1, 2)") in false with Failure str -> str = "[Error] Unmatched the number of arguments") ;
        assert (interp_prog (ParserMain.parse "def f1 = 999 endef f1()") = Store.NumV 999) ;
        (* interp_prog end *)
      
      
      
        (* test *)
        assert (Store.find "x" [("a", Store.NumV 1); ("x", Store.NumV 5)] = Store.NumV 5);
        assert (Store.find "xxx" [("x", Store.NumV 2); ("xx", Store.NumV 3); ("xxx", Store.NumV 999); ("xxxx", Store.NumV 998)] = Store.NumV 999);
        assert (Store.find "y" [("y", Store.NumV 1); "xy", Store.NumV 2] = Store.NumV 1);
        assert (Store.find "xyxy" [("xyxy", Store.NumV 111)] = Store.NumV 111);
        assert (Store.find "asd123" [("asd123", Store.NumV 0)] = Store.NumV 0);
        assert (try (Store.find "x" Store.empty) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: x");
        assert (try (Store.find "variable" [("a", Store.NumV 1); ("b", Store.NumV (2)); ("variabl", Store.NumV 33)]) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: variable");
        assert (try (Store.find "asdf" [("asd", Store.NumV 1); ("asdz", Store.NumV 123); ("ssss", Store.NumV 111)]) = Store.NumV (-1) with Failure str -> str = "[Error] Free identifier: asdf");
      
        assert (Store.add "x" (Store.NumV 3) Store.empty = [("x", Store.NumV 3)]);
        assert (Store.add "x" (Store.NumV 3) [("y", Store.NumV 1)] = [("x", Store.NumV 3); ("y", Store.NumV 1)]);
        assert (Store.add "x" (Store.NumV 3) [("y", Store.NumV 1); ("x", Store.NumV 0)] = [("x", Store.NumV 3); ("y", Store.NumV 1)]);
        assert (Store.add "xy" (Store.NumV 123) [] = [("xy", Store.NumV 123)]);
        assert (Store.add "xxx" (Store.NumV 1) [("a", Store.NumV 2); ("x", Store.NumV 3); ("xxx", Store.NumV 4); ("xxxx", Store.NumV 5)] = [("xxx", Store.NumV 1); ("a", Store.NumV 2); ("x", Store.NumV 3); ("xxxx", Store.NumV 5)]);
        assert (Store.add "hi" (Store.NumV 123) [("xxx", Store.NumV 1); ("a", Store.NumV 2); ("x", Store.NumV 3); ("xxxx", Store.NumV 5)] = [("hi", Store.NumV 123); ("xxx", Store.NumV 1); ("a", Store.NumV 2); ("x", Store.NumV 3); ("xxxx", Store.NumV 5)]);
      
      
        assert (Fstore.find "f" [("f", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")));
        assert (Fstore.find "f" [("f", (["x"; "y"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = (["x"; "y"], Ast.Add (Ast.Num 1, Ast.Id "x")));
        assert (Fstore.find "fff" [("fff", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))] = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")));
        assert (Fstore.find "f" [("f", (["x"], Ast.Sub (Ast.Num 1, Ast.Id "x")))] = (["x"], Ast.Sub (Ast.Num 1, Ast.Id "x")));
        assert (Fstore.find "f" [("f", (["x"], Ast.Sub ((Ast.Add (Ast.Id "x", Ast.Num 1)), Ast.Id "x")))] = (["x"], Ast.Sub ((Ast.Add (Ast.Id "x", Ast.Num 1)), Ast.Id "x")));
        assert (try (Fstore.find "f" Fstore.empty) = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))  with Failure str -> str  = "[Error] Unbound function: f");
        assert (try (Fstore.find "f" [("a", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]) = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))  with Failure str -> str  = "[Error] Unbound function: f");
        assert (try (Fstore.find "f" [("a", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x"))); ("b", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]) = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))  with Failure str -> str  = "[Error] Unbound function: f");
        assert (try (Fstore.find "ff" Fstore.empty) = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))  with Failure str -> str  = "[Error] Unbound function: ff");
        assert (try (Fstore.find "fff" [("a", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]) = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))  with Failure str -> str  = "[Error] Unbound function: fff");
        assert (try (Fstore.find "f21" [("a", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x"))); ("b", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]) = (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))  with Failure str -> str  = "[Error] Unbound function: f21");
      
        assert (Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]);
        assert (Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] =  [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]);
        assert (Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x"))); ("f", (["z"], Ast.Id "z"))] =  [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))]);
        assert (Fstore.add "ff" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [] = [("ff", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]);
        assert (Fstore.add "xxx" ["x"] (Ast.Add (Ast.Id "x", Ast.Num 3)) [] = [("xxx", (["x"], Ast.Add (Ast.Id "x", Ast.Num 3)))]);
      
        assert (interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]);
        assert (interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) [] = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]);
        assert (interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) [("f", (["x"], Ast.Add (Ast.Id "x", Ast.Num 1)))] = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]);
        assert (interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) [("fff", (["x"], Ast.Add (Ast.Id "x", Ast.Num 1)))] = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("fff", (["x"], Ast.Add (Ast.Id "x", Ast.Num 1)))]);
        assert (interp_def (Ast.FunDef ("f", ["x"; "y"; "z"], Ast.Add (Ast.Id "x", Ast.Sub(Ast.Id "y", Ast.Id "z")))) [("xxxx", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("fff", (["x"], Ast.Add (Ast.Id "x", Ast.Num 1)))] = [("f", (["x"; "y"; "z"], Ast.Add (Ast.Id "x", Ast.Sub(Ast.Id "y", Ast.Id "z")))); ("xxxx", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("fff", (["x"], Ast.Add (Ast.Id "x", Ast.Num 1)))]);
        
      
        assert (interp_expr (Ast.Add (Ast.Call ("f", [Ast.Num 3; Ast.Num 2]), Ast.Id "x")) [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))] [("x", Store.NumV 10)] = Store.NumV 15);
        assert (interp_expr (Ast.Call ("f", [Ast.Num 3; Ast.Num 2])) [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))] [] = Store.NumV 5);
        assert (interp_expr (Ast.Add (Ast.Call ("f1", [Ast.Num 2]), Ast.Id "x")) [("f1", (["x"], Ast.Add (Ast.Id "x", Ast.Num 10)))] [("x", Store.NumV 10)] = Store.NumV 22);
        assert (interp_expr (Ast.Add (Ast.Sub (Ast.Id "x", Ast.Id "y"), Ast.Call ("f1", [Ast.Num 4]))) [("f1", (["x"], Ast.Id "x"))] [("x", Store.NumV 4); ("y", Store.NumV 7)] = Store.NumV 1);
      
      
      
        assert (interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = Store.NumV 4);
        assert (interp_prog (ParserMain.parse "def f1 x y = x + y endef f1(3, 4) + f1(4, 7)") = Store.NumV 18);
        assert (interp_prog (ParserMain.parse "def f1 x y = x + y + 10 endef f1(3, 4)") = Store.NumV 17);
        assert (interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y - f1(1, 2) endef f1(3, 4) + f2(4, 7)") = Store.NumV 1);
        assert (interp_prog (ParserMain.parse "def f1 x = 10 + x endef def f2 x y = x - y + f1(10) endef f1(3) + f2(4, 7)") = Store.NumV 30);
        assert (interp_prog (Ast.Prog ([Ast.FunDef ("f1", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")); Ast.FunDef ("f2", ["x"; "y"], Ast.Sub (Ast.Id "x", Ast.Id "y"))], Ast.Add (Ast.Call ("f1", [Ast.Num 3; Ast.Num 4]), Ast.Call ("f2", [Ast.Num 4; Ast.Num 7])))) = Store.NumV 4);
        
        assert (interp_prog (ParserMain.parse "def f1 x = x endef def f2 x y = x - y + f1(x) endef f1(3) + f2(4, 7)") = Store.NumV 4); 
      
       *)