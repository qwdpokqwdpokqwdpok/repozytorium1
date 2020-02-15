let slowa a =
  let wynik = ref 1 in
  let res1 = ref [|'a'|] in
  let res2 = ref [|'b'|] in
  let res1copy = ref (!res1) in
  let x = ref 0 in
  let y = ref 0 in
  let n = Array.length a in
  while !x < n do
    if a.(!x) = !res1.(!y) then (
      if ((Array.length !res1) = (!x + 1)) then (
        wynik := !wynik + 1;
        res1 := Array.append !res1copy !res2;
        res2 := !res1copy;
        res1copy := !res1);
        y := !y + 1);
        x := !x + 1
  done;
  !wynik;
  !res1;;
