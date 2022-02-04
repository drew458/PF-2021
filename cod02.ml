(* pattern matching e uso di selettori *)
(* sort : 'a * 'a -> 'a * 'a *)
(* sort (x,y) = coppia con x e y ordinati *)
let sort (x,y) = 
  if x<y then (x,y) 
  else (y,x)

let sort pair = 
  if fst pair < snd pair 
  then (fst pair, snd pair)
  else (snd pair, fst pair)

(* ancora una definizione del fattoriale *)
let rec fact = function
    0 -> 1
  | n -> n * fact(n-1)

(* gcd : int -> int -> int *)
(* gcd a b = massimo comun divisore di a e b *)
(* assumendo che almeno uno tra a e b sia diverso da 0 *)
let rec gcd a b =
  if b=0 then (abs a)
  else gcd b (a mod b)

(* o anche *)
let rec gcd a = function
    0 -> abs a
  | b -> gcd b (a mod b)

(* xor: bool * bool -> bool *)
let xor = function
   (true,false) | (false,true) -> true
  | _ -> false

(* dichiarazioni locali *)
(* esempio: int * int * int -> int * int *)
let esempio (n,m,k) = 
  ((n+m)/k, (n+m) mod k)

let esempio(n,m,k) = 
   let somma = n+m 
   in (somma/k, somma mod k)

(* riduzione di una frazione ai minimi termini *)
let fraction (n,d) =
    let com = gcd n d
    in  (n/com, d/com)

(* eccezioni *)
exception NegativeNumber
(* fact: int -> int
   fact n = fattoriale di n, se n non e` negativo,
            altrimenti solleva NegativeNumber *)
let rec fact n =
  if n < 0 then raise NegativeNumber
                 (* viene "sollevata" l'eccezione *)
  else if n=0 then 1
       else n * fact (n-1)

(* conta_digits *)
(* numeric : char -> bool *)
let numeric c =
  c >= '0' && c <= '9'

(* conta_digits: string -> int *)
(* conta_digits s = numero di caratteri numerici in s *)
let conta_digits s =
  let max_index = (String.length s) - 1 in
  (* loop: int -> int *)
  (* loop i = numero di caratteri numerici in s a partire dalla
     posizione i *)
  let rec loop i =
    if i > max_index then 0 
    else if numeric s.[i] then 1 + loop (i+1)
         else loop (i+1) 
   in loop 0

(* uso "sporco" delle eccezioni *)
let conta_digits2 s =
  let rec loop i =
    try if numeric s.[i] then 1 + loop (i+1)
        else loop (i+1)
    with Invalid_argument "index out of bounds" -> 0
  in loop 0

(* ========================== *)
(* problema "evaluate" *)

let primo_non_numerico s =
  let rec loop i = 
      (* loop: int -> int *)
      (* loop i = indice del primo carattere non numerico in s
         a partire da quello in posizione i *)
      if not (numeric s.[i]) then i
      else loop (i+1)
  in loop 0

(* sottoproblema substring *)
(* substring : string -> int -> int -> string *)
(* substring s j k = sottostringa di s che va dalla posizione j 
                           alla posizione k *)
let substring s j k =
  String.sub s j ((k-j)+1)

(* sottoproblema split_string *)
(* split_string : string -> int * char * int
   split_string s = (n,op,m) dove: n e' il primo operando, op l'operatore
                                   e m il secondo operando *)
let split_string s =
  let i = primo_non_numerico s
  in (int_of_string (substring s 0 (i-1)),
      s.[i],
      int_of_string (substring  s (i+1) 
		       ((String.length s)-1)))


(* problema principale *)
(* eccezione da sollevare nel caso in cui il carattere che rappresenta
   l'operatore non e' uno di quelli ammessi *)
exception BadOperation

(* evaluate: string -> int *)
let evaluate s =
  let (n,op,m) = split_string s
  in if op='+' then n+m
     else if op='-' then n-m
          else if op='*' then n*m
               else if op='/' then n/m
                    else raise BadOperation


(* primo non numerico con eccezione non predefinita *)      
(* primo_non_numerico: string -> int *)
let primo_non_numerico str =
  (* aux: int -> int *)
  let rec aux i =
      if not (numeric str.[i]) then i
      else aux (i+1)
  in 
  try aux 0
  with _ -> raise BadOperation

(* split string con eccezione non predefinita *)
exception BadInt

(* split_string : string -> int * char * int *)
let split_string s =
  let i = primo_non_numerico s
  in 
  try (int_of_string (substring s 0 (i-1)),
       s.[i],
       int_of_string (substring  s (i+1) ((String.length s)-1)))
  with _ -> raise BadInt 

(* evalutate con uso di match *)
(* evaluate: string -> int *)
let evaluate s =
  let (n,op,m) = split_string s
  in match op with
    '+' -> n+m
  | '-' -> n-m
  | '*' -> n*m
  | '/' -> n/m
  | _ -> raise BadOperation
