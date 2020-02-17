type 'a heap = Node of 'a heap * 'a * 'a heap | Null;;

let rekonstrukcja arr =
  if Array.length arr = 0 then Null else
  let dodaj_korzen h k =
    Node (h, k, Null) in
  let dodaj_prawe_poddrzewo (Node (h1, k, Null)) h2 =
    Node (h1, k, h2) in
  let nowe_poddrzewo k =
    Node (Null, k, Null) in
  let rec sklejaj ((maxh1,h1)::l) g =
    match l with
      | [] -> (g, dodaj_korzen h1 g)::[]
      | (maxh2,h2)::tl ->
        if g < maxh2 then (g, dodaj_korzen h1 g)::(maxh2,h2)::tl else
        sklejaj ((maxh2, dodaj_prawe_poddrzewo h2 h1)::tl) g in
  let rec sklej_pozostale l =
    match l with
      | ((maxh,h)::[]) -> [(maxh,h)]
      | ((maxh1,h1)::(maxh2,h2)::tl) ->
        sklej_pozostale ((maxh2, dodaj_prawe_poddrzewo h2 h1)::tl) in
  let prawie_wynik =
    Array.fold_left
      (fun ((maxh,h)::l) g ->
        if h = Null then [(g, nowe_poddrzewo g)] else
        if g > maxh then sklejaj ((maxh,h)::l) g else
        ((g, nowe_poddrzewo g)::(maxh,h)::l))
      [(0, Null)]
      arr in
  match sklej_pozostale prawie_wynik with [(maxh,h)] -> h;;
        
