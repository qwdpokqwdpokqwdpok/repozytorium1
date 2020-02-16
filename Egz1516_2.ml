exception Nic_nie_jezdzi_a_sa_ludzie;;
exception Ludzie_przyszli_za_pozno;;
let autobusy p a =
  let wynik = ref 0 in
  let n = Array.length p in
  let m = Array.length a in
  let suma_ludzi = ref 0 in
  if n = 0 then 0 else
  if m = 0 then
  try
    for i = 0 to n - 1 do
      if p.(i) > 0 then raise Nic_nie_jezdzi_a_sa_ludzie
    done;
    0;
  with Nic_nie_jezdzi_a_sa_ludzie -> - 1
  else
  try
    for i = a.(m - 1) + 1 to n - 1 do
      if p.(i) > 0 then raise Ludzie_przyszli_za_pozno
    done;
  for i = m - 1 downto 1 do
    for j = a.(i) downto a.(i - 1) + 1 do
      suma_ludzi := !suma_ludzi + p.(j)
    done;
  let wynik_dla_czasu_ai =
    (!suma_ludzi / (m - i)) + (if !suma_ludzi mod (m - i) = 0 then 0 else 1) in
  if wynik_dla_czasu_ai > !wynik then wynik := wynik_dla_czasu_ai;
  done;
  for i = a.(0) downto 0 do
    suma_ludzi := !suma_ludzi + p.(i)
  done;
  let wynik_dla_czasu_a0 =
    (!suma_ludzi / m) + (if !suma_ludzi mod m = 0 then 0 else 1) in
  if wynik_dla_czasu_a0 > !wynik then wynik := wynik_dla_czasu_a0;
  !wynik;
  with Ludzie_przyszli_za_pozno -> - 1;;
