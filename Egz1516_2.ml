let autobusy p a =
  let wynik = ref 0 in
  let n = Array.length p in
  let m = Array.length a in
  let suma_ludzi = ref 0 in
  try
    for i = a.(m - 1) to 
  for i = m - 1 downto 1 do
    for j = a.(i) downto a.(i - 1) + 1 do
      suma_ludzi := !suma_ludzi + p.(j)
    done;
    
    
