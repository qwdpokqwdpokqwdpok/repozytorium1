type kolor = Zielona | Czerwona


let kulki l =
  let z = List.fold_left (fun acc x -> if x = Zielona then acc + 1 else acc) 0 l in
  let c = List.fold_left (fun acc x -> if x = Zielona then acc + 1 else acc) 0 l in
  let koniec = (min z c) * 2 in
  let kolor_zakonczenia = if z > c then Zielona else Czerwona in
  let ile_zle_zielonych =
    List.fold_left
    (fun (parzystosc, miejsce, licznik) k ->
      if miejsce < koniec then
        if parzystosc = 0 then
          if k = Zielona then (1, miejsce + 1, licznik)
          else (1, miejsce + 1, licznik)
        else if k = Zielona then (0, miejsce + 1, licznik + 1)
          else (0, miejsce + 1, licznik)
      else if k = Zielona && kolor_zakonczenia = Czerwona then (0, miejsce + 1, licznik +1)
        else (0, miejsce + 1, licznik))
    (0, 0, 0)
    l in
  match ile_zle_zielonych with (_,_,licznik) -> licznik;;
