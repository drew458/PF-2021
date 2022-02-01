let rec sb (lower,upper) = 
  if lower>upper then 0
  else sb(lower+1,upper) +lower
