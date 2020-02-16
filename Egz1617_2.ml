 type pora = Wiosna | Lato | Jesien | Zima;;
 let pora arr =
  let nastepna = function
    | Wiosna -> Lato
    | Lato -> Jesien
    | Jesien -> Zima
    | Zima -> Wiosna in
  let n = Array.length arr in
  let wynik = ref (- 1) in
  for i = 0 to n - 1 do
    let licznik = ref 3 in
    let indeks = ref (i + 1) in
    let aktualna_pora = ref arr.(i) in
    while !licznik > 0 && !indeks < n do
      if arr.(!indeks) = nastepna !aktualna_pora then (
        licznik := !licznik - 1;
        aktualna_pora := nastepna !aktualna_pora;
        if !licznik = 0 then wynik :=
         (if !wynik = -1 then !indeks + 1 - i
         else min (!indeks + 1 - i) !wynik;));
      indeks := !indeks + 1;
    done;
  done;
  !wynik;;
  
