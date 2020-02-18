(* subset sum problem *)
let energia k t e =
  let n = Array.length t in
  if k > n then [||] else
  let h = Hashtbl.create n in
  let kolejka = Queue.create
  let wynik = ref [||] in
  let flag = ref true in
  let start = (Array.init k (fun i -> i)) in
  let check arr =
    (Array.fold_left (fun acc a -> acc + t.(a)) 0 arr) = e in
  Queue.add start kolejka;
  while not (Queue.is_empty kolejka) && !flag do
    let a = Queue.take kolejka in
    for i = 0 to k - 1 do
      for j = 0 to n - 1 do
        let kandydat = Array.copy a in
        kandydat.(i)
        
