(* Sia data la seguente definizione di tipo: type color = Rosso | Verde | Neutro
   Per rappresentare i colori di un insieme di elementi (di tipo α) utilizziamo 
   una lista associativa di tipo (α × color) list, contenente coppie (e,c) dove 
   c può essere soltanto Rosso o Verde: per convenzione, gli elementi a cui non 
   è associato alcun valore nella lista hanno colore Neutro. Scrivere una 
   funzione conta_colori: (α × color) list → α list → (color × int) list
   tale che conta_colori cols lista riporti la lista con 3 coppie 
   [(Rosso,n);(Verde,m);(Neutro,k)] dove n è il numero di elementi rossi in 
   lista, m il numero di quelli verdi e k il numero di quelli neutri. 
   Ad esempio, se cols è la lista [(2,Rosso); (3,Verde); (4,Verde);(6,Verde); 
   (7,Rosso)], allora conta_colori cols [1;2;3;4;5;6;7;8;9;10] riporterà
   la lista [(Rosso, 2); (Verde, 3); (Neutro, 5)]. *)

type color = Rosso | Verde | Neutro

let cols = [(2, Rosso);(3,Verde);(4,Verde);(6,Verde);(7,Rosso);(11,Verde)]

let lista = [1;2;3;4;5;6;7;8;9;10]

(* ---------------------VERSIONE RICORSIVA----------------*)
(* conta_colori: ('a x color) list -> 'a list -> (color x int) list *)
(* conta_colori cols lista: riporta la lista con tre coppie (colore, numero
   di occorrenze del colore) *)
let rec conta cols = function
    [] -> [(Rosso, 0);(Verde, 0); (Neutro, 0]
  | x::rest ->
        let tmp = 
          try List.assoc x cols
          with _ -> Neutro
        in let [(Rosso, r);(Verde, v);(Neutro, n)] = conta cols rest 
        in match tmp with
            Rosso ->[(Rosso,r+1);(Verde,v);(Neutro,n)]
          | Verde -> [(Rosso,r);(Verde,v+1);(Neutro,n)]
          | Neutro -> [(Rosso,r);(Verde,v);(Neutro,n+1)]


(* ---------------------VERSIONE ITERATIVA----------------*)
(* conta_colori: ('a x color) list -> 'a list -> (color x int) list *)
(* conta_colori cols lista: riporta la lista con tre coppie (colore, numero
   di occorrenze del colore) *)
let conta_colori cols lista =
  let rec loop lista r v n =
    match lista with
        []->[(Rossi,r);(Verdi,v);(Neutri,n)]
      | x::rest->
          match trova cols x with
            Rosso -> loop rest (r+1) v n
          | Verde -> loop rest r (v+1) n
          | _ -> loop rest r v (n+1)
  in loop lista 0 0 0
