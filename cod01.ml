(* fact: int -> int *)
(* fact n = fattoriale di n
   si assume n non negativo *)
let rec fact n =
  if n=0 then 1
  else n * fact(n-1)

(* double: int -> int *)
(* double n = doppio di n *)
let double n = 2 * n

(* first : 'a * 'b -> 'a *)
(* first x = primo elemento della coppia x *)
let first (x,y) = x

(* id : 'a -> 'a *)
(* funzione identita' *)
let id x = x

(* mult: int*int -> int *)
let mult (m,n) = m * n

(* times : int -> int -> int *)
(* forma currificata di mult *)
(* times n = funzione che moltiplica per n il suo argomento *)
let times n = function m -> n * m

(* pair: 'a -> 'b -> 'a * 'b *)
let pair x y = (x,y)

(* lessthan : 'a -> 'a -> bool *)
let lessthan x y = y < x

(* greaterthan : 'a -> 'a -> bool *)    
let greaterthan x y = y > x
    
(* equal : 'a -> 'a -> bool *)
let equal x y = x=y

(* square : int -> int *)
(* square n = quadrato di n *)
let square n = n* n

(* sommatoria *)
(* sum : (int -> int) -> (int * int) -> int *)
(* sum f (lower,upper) = (f lower) + (f (lower+1)) + .... + (f upper) *)
let rec sum f = function
    (lower,upper) -> if lower > upper then 0
                     else f lower + sum f (lower +1,upper)
(* o anche *)
let rec sum f (lower,upper) = 
  if lower > upper then 0
  else f lower + sum f (lower +1,upper)

(* oppure, forma currificata di sum f *)
let rec sum f  lower upper = 
  if lower > upper then 0
  else f lower + sum f (lower +1) upper

(* sumbetween : int * int -> int *)
(* sumbetween (n,m) = n + (n+1) + ... + m *)
let rec sumbetween (n,m) =
  if n>m then 0 else n + sumbetween (n+1,m)

(* sbt : int -> int -> int *)
(* forma currificata di sumbetween *)
(* sbt n m =  sumbetween (n,m) *)
let rec sbt n m =
  if n>m then 0 else n + sbt (n+1) m

(* composizione di funzioni *)
(* comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
(* comp f g = composizione di f con g *)
let comp f g = function x -> f (g x)
(* o anche *)
let comp f g x = f (g x)

(* composizione definita come operatore infisso *)
let (@@) f g x = f(g x)

(* average : float -> float -> float *)
(* average x y = media aritmetica di x e y *)
let average x y = (x *. y) /. 2.0

(* sign : int -> int *)
(* sign n = 0, 1 o -1 a seconda del "segno" di n *)
let sign n = 
  if n > 0 then 1
  else 
    if n = 0 then 0
    else -1

(* sort : 'a * 'a -> 'a * 'a *)
(* sort (x,y) = coppia con x e y ordinati *)
let sort (x,y) = 
  if x<y then (x,y) 
  else (y,x)

(* necessita' di espressioni "lazy" *)
(* cond : bool * 'a * 'a -> 'a *)
let cond(c,e1,e2) = 
  if c then e1 else e2

let rec fattoriale n =
  cond(n=0, 1, n * fattoriale(n-1))
