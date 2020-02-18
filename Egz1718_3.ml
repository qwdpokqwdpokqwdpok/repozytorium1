(* subset sum problem *)
let energia k t e =
  let n = Array.length t in
  if k > n then [||] else
  let h = Hashtbl.create n in
  let kolejka = Queue.create
  let wynik = ref [||] in
  let flag = ref true in
  let start = (Array.init k (fun i -> i)) in
  
  let zmien_tablice tab s =
    Array.init (Array.length tab - 1)
      (fun i ->
        if i < s then tab.(i) else tab.(i + 1)) in
  let rec check arr suma ile_liczb =
    let n = Array.length arr in
    if ile_liczb = 2 then
      let pocz = ref 0 in
      let kon = ref (n - 1) in
      let znaleziono = ref false in
      while (!kon != !pocz) && not !znaleziono do
        if arr.(!kon) + arr.(!pocz) = suma then
          znaleziono := true; else
        if arr.(!pocz) + arr.(!kon) > suma then
          kon := !kon - 1; else
        pocz := !pocz + 1;
      done;
      if !znaleziono then
        [|!pocz; !kon; -1|] else
      [|-2; -2; -2|] else
    let wyniki =
      Array.init (n - 1)
        (fun i ->
          check (zmien_tablice arr i) (suma - arr.(i)) (ile_liczb - 1)) in
    Array.fold_left (fun 
  
  
  
  
  let check arr =
    (Array.fold_left (fun acc a -> acc + t.(a)) 0 arr) = e in
  Queue.add start kolejka;
  while not (Queue.is_empty kolejka) && !flag do
    let a = Queue.take kolejka in
    for i = 0 to k - 1 do
      for j = 0 to n - 1 do
        let kandydat = Array.copy a in
        kandydat.(i)
        
