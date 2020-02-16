(*let dupa = Array.init i (fun _ -> Array.init j (fun _ -> (Array.make k 0))) 
  let odwierty = Array.init n (fun i -> Array.init m (fun j -> if arr.(i).(j) < 0 then 1 else 0))
  let mapa = Array.copy arr *)

let gaz arr =
  let n = Array.length arr in
  let m = Array.length arr.(0) in
  let kolejka = Queue.create () in
  let wynik = ref 0 in
  let flag = ref true in
  while !flag do
    let odwiedzone = Array.make_matrix n m false in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        if arr.(i).(j) < 0 then (
        Queue.add (i,j) kolejka;
        odwiedzone.(i).(j) <- true;
        wynik := !wynik + 1;
        arr.(i).(j) <- arr.(i).(j) + 1;)
      done;
    done;
    if Queue.is_empty kolejka then flag := false;
    while not (Queue.is_empty kolejka) do
      let (x, y) = Queue.take kolejka in
      let check (a, b) =
        arr.(a).(b) > 0 && not odwiedzone.(a).(b) in
      let dodawaj (a, b) =
        Queue.add (a, b);
        odwiedzone.(a).(b) <- true;
        wynik := !wynik + 1;
        arr.(a).(b) <- arr.(a).(b) - 1; in
      if x > 0 then if check (x - 1, y) then dodawaj (x - 1, y);
      if x < n - 1 then if check (x + 1, y) then dodawaj (x + 1, y);
      if y > 0 then if check (x, y - 1) then dodawaj (x, y - 1);
      if y < m - 1 then if check (x, y + 1) then dodawaj (x, y + 1);
    done;
  done;
!wynik;;
