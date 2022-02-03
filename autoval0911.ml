(* Scrivere una funzione ricorsiva e una iterativa di una funzione
   elimina: string -> (string x string) list -> (string x string) list tale che
   elimina s pairlist = lista che si ottiene da pairlist eliminando le coppie
   costituite da due stringhe la cui concatenazione è uguale a s. Le coppie
   possono occorrere in qualsiasi ordine nel risultato.
   Ad esempio, il valore di elimina "abcdef"[("","abcdef");("pi","ppo");
   ("abc","def");("ef","abcd")] sarà la lista [("pi","ppo");("ef","abcd")],
   oppure la lista [("ef","abcd");("pi","ppo")]
   Nota bene: è indispensabile specificare il tipo e dare una descrizione
   dichiarativa di ogni funzione ausiliaria utilizzata (anche locale),
   altrimenti non verrà presa in considerazione. *)


(* -------VERSIONE RICORSIVA----------*)
(* elimina: string -> (string x string) list -> (string x string) list *)
(* elimina s1 lst: elimina le coppie nella stringa la cui concatenazione
   è uguale alla stringa s *)
let rec elimina s1 lst = function
  [] -> []
  |  (x,y)::rest -> if x^y=s then elimina s rest else (x,y)::elimina s rest


(* -------VERSIONE ITERATIVA----------*)
(* elimina: string -> (string x string) list -> (string x string) list *)
(* elimina s1 lst: elimina le coppie nella stringa la cui concatenazione
   è uguale alla stringa s *)
let elimina s1 lista =
  (* aux: (string x string) list -> (string x string) list ->
          (string x string) list *)
  (* aux pairlist newlist: elimina le coppie nella stringa la cui concatenazione
   è uguale alla stringa s *)
  let rec aux pairlist newlist = function
    [] -> newlist
    | (x,y)::rest -> if x^y=s1 then aux rest newlist
      else aux rest (x,y)::rest
  in aux lista []
