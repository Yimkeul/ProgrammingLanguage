let rec len lst =
  match lst with
  | [] -> 0
  | _ ::  t ->
    1 + (len t)


let rec rev lst =
  match lst with
  | [] -> []
  | h :: t -> (rev t) @ [h]
  
  
let rec map f lst =
  match lst with
  | [] -> []
  | h :: t -> (f h) :: (map f t)
  

(* let rec fold_right f result lst  =
  match lst with
  | [] -> result
  | h :: t -> f (h ,(fold_right f result t ))
  
   *)

let rec fold_right f result lst =
 match lst with
 | [] ->
     print_string "lst: []";
     print_newline ();
     print_string "result: ";
     print_int result;
     print_newline ();
     result
 | h :: t ->
     let acc = fold_right f result t in
     let res = f (h, acc) in
     print_string "lst: [";
     List.iter (fun x -> print_int x; print_string "; ") lst;
     print_string "]";
     print_newline ();
     print_string "result: ";
     print_int res;
     print_newline ();
     res
  
  

     let rec fold_left f acc lst =
      match lst with
      | [] ->
          print_string "lst: []";
          print_newline ();
          print_string "result: ";
          print_int acc;
          print_newline ();
          acc
      | h :: t ->
          let res = f (acc, h) in
          print_string "lst: [";
          List.iter (fun x -> print_int x; print_string "; ") lst;
          print_string "]";
          print_newline ();
          print_string "result: ";
          print_int res;
          print_newline ();
          fold_left f res t
    
    
    

(* let _ = (fold_right ( fun (x,i) -> x+i) 0 [1;2;3])
let _ = Format.printf("NEXT\n")
let _ = (fold_left ( fun (x,i) -> x+i) 0 [1;2;3]) *)

let _ =
  let r1 = fold_right (fun (x,i) -> x+i) 0 [1;2;3] in
  let () = print_endline "fold_right result:" in
  let () = print_endline (string_of_int r1) in
  let () = print_endline "-------------" in
  let r2 = fold_left (fun (x,i) -> x+i) 0 [1;2;3] in
  let () = print_endline "fold_left result:" in
  let () = print_endline (string_of_int r2) in
  ()


(* let _ = Format.printf "Hello %d\n" (fold_right ( fun (x,i) -> x+i) 0 [1;2;3]) *)

