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
  

let rec fold_right f result lst  =
  match lst with
  | [] -> result
  | h :: t -> f (h ,(fold_right f result t ))
  
  

let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | h :: t -> fold_left f(f (acc ,h)) t
     



