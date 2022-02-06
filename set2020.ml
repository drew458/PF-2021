(* Settembre 2020 *)

type 'a graph = ('a * 'a) list

exception NotFound

(* from_node: 'a list -> 'a -> 'a list
   from_list: 'a list -> 'a list -> 'a list *)
(* from_node visited n = cammino aciclico che parte dal nodo n fino a target e
                         che passa per il nodo tappa, senza passare per i nodi
                         in visited.
   from_list visited listanodi = cammino aciclico che parte da uno dei nodi in
                                 listanodi fino a target e che passa per il
                                 nodo tappa, senza passare per i nodi in
                                 visited. *)
let percorso graph start tappa target =
  let rec from_node visited n =
    if List.mem n visited then raise NotFound
    else if n=target && (List.mem tappa visited || n=tappa) then [n]
         else n::from_list (n::visited) (successori n graph)
  and from_list visited = function
      [] -> raise NotFound
    | n::rest -> try from_node visited n
                 with NotFound -> from_list visited rest
  in from_node [] start

(*----------------------------------------------------------------------------*)

(* Giugno 2021 *)

(** 1 **)

type 'a graph = ('a * 'a) list

exception NotFound

(* from_node: int -> 'a -> 'a list
   from_list: int -> 'a list -> 'a list *)
(* from_node len n = cammino che parte dal nodo n fino a goal, la cui lunghezza
                     len non supera la profondita' depth.
   from_list len listanodi = cammino che parte da uno dei nodi in listanodi
                             fino a goal, la cui lunghezza len non supera la
                             profondita' depth. *)
let depth_limited g start goal depth =
  let rec from_node len n =
    if len>depth then raise NotFound
    else if n=goal then [n]
         else n::from_list (len+1) (successori n g)
  and from_list len = function
      [] -> raise NotFound
    | n::rest -> try from_node len n
                 with NotFound -> from_list len rest
  in from_node 0 start

(** 2 **)

(* aux: 'a graph -> 'a -> 'a -> int -> 'a list *)
(* aux depth = cammino in depth_limited g start goal depth. Se depth_limited
               fallisce, allora cerco un cammino con depth+1 fino a che
               depth non supera maxdepth. *)
let path g start goal maxdepth =
  let rec aux depth =
    if depth>maxdepth then raise NotFound
    else try depth_limited g start goal depth
         with NotFound -> aux (depth+1)
  in aux 0