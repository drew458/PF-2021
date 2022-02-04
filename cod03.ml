(* ciclo: int -> int -> unit
   ciclo n m: stampa gli interi da n a m (estremi inclusi) *)
let rec ciclo n m =
  if n>m then ()
  else (print_int n;    (* sequenza di comandi *)
	print_newline();
	ciclo (n+1) m)

(* stampa: int -> unit 
   stampa n: stampa degli interi da 0 a n *)
let stampa = ciclo 0

(* conta_digits tail recursive *)
(* numeric : char -> bool *)
let numeric c =
  c >= '0' && c <= '9'
(* conta_digits: string -> int *)
(* conta_digits s = numero di caratteri numerici in s *)
let conta_digits s =
  let rec loop i result =
    try if numeric s.[i] then loop (i+1) (result+1)
        else loop (i+1) result
    with _ -> result
  in loop 0 0

(* somma: unit -> int
   somma () : 
   legge una sequenza di interi terminata da "." e 
   riporta la somma dei numeri letti *)
let rec somma () =
  let s = read_line ()
  in if s="." then 0
  else (int_of_string s) + somma()

(* versione iterativa *)
let somma2 () =
  (* loop: int -> int *)
  (* loop n = n + somma degli interi letti da tastiera *)
  let rec loop result =
    let s = read_line ()
    in if s="." then result
    else loop ((int_of_string s) + result)
  in loop 0

(* uso "sporco" delle eccezioni *)
let somma3 () =
  (* loop: int -> int *)
  (* loop n = n + somma degli interi letti da tastiera *)
  let rec loop result =
    try let n = int_of_string (read_line())
        in loop (n+result)
    with _ -> result
  in loop 0


(* numero_somma: unit -> int * int
   numero_somma () = (n,s) dove n e' il numero degli interi letti
                   da tastiera, e s la loro somma *)
let rec numero_somma () =
  let s = read_line()
  in
  if s="."  (* terminato? *)
  then (0,0) (* nessun numero letto, somma 0 *)
  else (* stringa rappresenta un int, leggi gli altri numeri *)
    let (tot,somma) = numero_somma()
    in (tot+1,somma+(int_of_string s))

(* oppure, usando le eccezioni *)
let rec numero_somma () =
  try let n = int_of_string(read_line())
      in let (tot,somma) = numero_somma()
      in (tot+1,somma+n)
  with _ -> (0,0)

(* numero_somma non e' tail recursive *)
(* versioni iterative *)
let numero_somma_it () =
  (* aux: int -> int -> int * int *)
  let rec aux tot somma = (* due "accumulatori" *)
    let s = read_line ()
    in if s="." then (tot,somma)  (* e non (0,0) *)
    else aux (tot+1) (somma + (int_of_string s))
              (* incremento degli "accumulatori" *)
  in aux 0 0 

(* oppure, usando le eccezioni *)
let numero_somma_it () =
  (* aux: int -> int -> int * int *)
  let rec aux tot somma = (* due "accumulatori" *)
    try
      aux (tot+1) (somma + (int_of_string(read_line())))
    with _ -> (tot,somma)
  in aux 0 0 


(* obiettivo: leggere da file una sequenza di numeri *)
(* interi,  scrivere il numero di interi *)
(* letti, la loro somma, e la media *)
(* media: string -> unit *)
(* media s = legge i numeri dal file di nome s *)

let  media file =
  (* apertura del canale di input *)
  let inchan = open_in file
  in (* "ciclo" di lettura dei numeri *)
     (* riporta il numero di interi letti e la loro somma *)
     (* simile a numero_somma, ma con lettura da file *)
  (* loop: unit -> int * int *)
  let rec loop () =
    try 
      let n = int_of_string(input_line inchan)
                           (* lettura da file *)
      in let (tot,somma) = loop()
      in (tot+1,somma+n)
    with _ -> 
      close_in inchan; (* chiusura del canale di input *)
      (0,0)
   in let (n,somma) = loop ()
  in   (* sequenza di comandi *)
  print_string ("Letti "^(string_of_int n)^
		" interi\nSomma: "^(string_of_int somma)^
		"\nMedia: "^
		(string_of_float ((float_of_int somma)/.(float_of_int n)))
	       ^"\n")
  (* niente overloading *)

(* uso di loop iterativo in media *)
(* media_it: string -> unit *)
(* media_it s = legge dal file di nome s una sequenza di numeri *)
(* interi, terminata dalla stringa ".", e stampa il numero di interi *)
(* letti, la loro somma, e la media *) 
let  media_it file =
  let inchan = open_in file in 
  let rec loop tot somma = (* due accumulatori *)
    try
      loop (tot+1) (somma + (int_of_string(input_line inchan)))
    with _ -> 
      close_in inchan; (* chiusura del canale di input *)
      (tot,somma)
  in let (n,somma) = loop 0 0 (* <=== inizializzazione *)
  in 
  print_string ("Letti "^(string_of_int n)^
		" interi\nSomma: "^(string_of_int somma)^
		"\nMedia: "^
		(string_of_float ((float_of_int somma)/.(float_of_int n)))
	       ^"\n")



(* ----------------------------- *)
(* processi ricorsivi e iterativi *)
let rec fact = function
    0 -> 1
  | n -> n * fact(n-1)

(* versione iterativa *)
let rec fact' n =
  (* aux: int -> int -> int
     il primo argomento e' il "risultato parziale" *)
  (* aux f k = f * fattoriale di k *)
  let rec aux f = function 
      0 -> f   (* il "ciclo" termina *)
    | n -> aux (f*n) (n-1)
  in aux 1 n
