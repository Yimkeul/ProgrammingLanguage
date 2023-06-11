let rec factorial a = 
  if a = 0 then 1
  else if a < 0 then (-1)
  else a * factorial (a - 1)

let rec fib b = 
  if b < 0 then -1
  else if b = 0 then 0
  else if b = 1 then 1
  else fib (b-1) + fib (b-2)
  


let rec acc c =
  if c < 0 then c + acc (c+1)
  else if c = 0 then 0
  else c + acc(c-1)
  


let rec pow d e = 
  if e < 0 then 0
  else if e =0 then 1
  else d * (pow d (e-1))


let _ = Format.printf "test:%d\n" (fib (-100))
let _ = Format.printf "test:%d\n" (fib 0)  
let _ = Format.printf "test:%d\n" (fib 1)
let _ = Format.printf "test:%d\n" (fib 2)  
let _ = Format.printf "test:%d\n" (fib 3)
let _ = Format.printf "test:%d\n" (fib 4)  
let _ = Format.printf "test:%d\n" (fib 5)
let _ = Format.printf "test:%d\n" (fib 6)