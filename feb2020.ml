(** Definizioni di base **)

let rec successori n = function
    [] -> []
  | (x,y)::rest -> if n=x then y::(successori n rest)
                   else successori n rest

let rec vicini nodo = function 
    [] -> []
  | (x,y)::rest -> if x=nodo then y::vicini nodo rest
                   else if y=nodo then x::vicini nodo rest
                        else vicini nodo rest

(*----------------------------------------------------------------------------*)

(* Febbraio 2020 *)

type metro = (int * int * string) list

(** 1 **)

(* mkset: 'a list -> 'a list *)
(* mkset lst = lista con gli elementi di lst rimuovendo tutte le ripetizioni. *)
let rec mkset = function
    [] -> []
  | x::rest -> if List.mem x rest then mkset rest
               else x::mkset rest

(* aux: 'a list -> metro -> 'a list *)
(* aux lst m = (mkset lst)@(line m ln) *)
let line m ln =
  let rec aux lst = function
      [] -> mkset lst
    | (x,y,s)::rest -> if s=ln then aux (x::y::lst) rest
                       else aux lst rest
  in aux [] m

(** 2 **)

let rec vicini stazione = function
    [] -> []
  | (x,y,s)::rest -> if x=stazione then (y,s)::vicini stazione rest
                     else if y=stazione then (x,s)::vicini stazione rest
                          else vicini stazione rest

(** 3 **)

exception NotFound

(* from_node: 'a list -> int -> string -> 'a -> 'a list
   from_list: 'a list -> int -> string -> 'a list -> 'a list *)
(* from_node visited cambi linea s = A partire da una stazione (s) avente
                                     linea precedente (linea) e cambi
                                     disponibili trova un percorso dalla
                                     stazione corrente fino al nodo goal, senza
                                     passare per le stazioni contenute nella
                                     lista visited. Fallisce altrimenti.
   from_list visited cambi linea stazioni = Trova un cammino a partire dalla
                                            lista di coppie (stazione * linea)
                                            fino ad un nodo goal cambiando
                                            massimo "cambi" volte linea e 
                                            considerando "linea" come linea
                                            precedente alle stazioni della
                                            lista. Fallisce altrimenti. *)
let raggiungi m maxc start goal =
  let rec from_node visited cambi linea s =
    if List.mem s visited then raise NotFound
    else if s=goal && cambi=maxc then [s]
         else s::from_list (s::visited) cambi linea (vicini s m)
  and from_list visited cambi linea = function
      [] -> raise NotFound
    | (s,ln)::rest -> try if linea=ln then from_node visited cambi ln s
                          else from_node visited (cambi+1) ln s
                      with NotFound -> from_list visited cambi linea rest
  in from_node [] (-1) "" start