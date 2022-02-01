exception BadOperation

let primo_non_numerico s =
  let rec loop i =
    if not (numeric s.[i]) then i
    else loop(i+1)
  in loop 0

let substring s j k =
  String.sub s j ((k-j)+1)

let split_string s =
  let i = primo_non_numerico s
  in (int_of_string (substring s 0 (i-1)),
      s.[i],
      int_of_string (substring s (i+1)
                       ((String.lenght s)-1)))

let evaluate s =
  let (n,op,m) = split_string s
  in if op='+' then n+m
    else if op='-' then n-m
    else if op='*' then n*m
    else if op='/' then n/m
    else raise BadOperation
