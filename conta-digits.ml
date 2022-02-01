(* numeric : char -> bool *)
let numeric c = 
  c >= '0' && c <= '9'

(* conta_digits: string -> int
   conta_digits s = numero di caratteri numerici in s *)
let conta_digits s =
  let max_index = (String.lenght s) -1 in
  (* loop: int -> int *)
  (* loop i = numero di caratteri numerici in s a partire da quello
     in posizione s *)
  let rec loop i =
    if i>max_index then 0
    else if numeric s.[i] then 1 + loop(i+1)
    else loop (i+1)
  in loop 0
