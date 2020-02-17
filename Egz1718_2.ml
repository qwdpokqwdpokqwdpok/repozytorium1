(* drzewa zrownowazone -> roznica wysokosci poddrzew ograniczona ->
-> stosunek ilosci wezlow w poddrzewach ograniczony *)
(* binary, roznica wysokosci <= 1 ->
-> max wezlow dla wysokosci n -> 2^n-1
-> min wezlow dla wysokosci n -> 
  r(0)=0; r(1)=1; r(n)=r(n-1)+r(n-2)+1
  0 1 2 4 7 12 20 33 54
