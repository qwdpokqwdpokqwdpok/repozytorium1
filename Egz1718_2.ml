(* drzewa zrownowazone -> roznica wysokosci poddrzew ograniczona ->
-> stosunek ilosci wezlow w poddrzewach ograniczony *)
(* binary, roznica wysokosci <= 1 ->
-> max wezlow dla wysokosci n -> 2^n-1
-> min wezlow dla wysokosci n -> 
  r(0)=0; r(1)=1; r(n)=r(n-1)+r(n-2)+1
  0 1 2 4 7 12 20 33 54
*)
(* idziesz w lewo i patrzysz na relacje elementu w ktorym stoisz i poprzedniego
jest ..., wiekszy, mniejszy -> poprzedni byl takim punktem,
ze wszystko na lewo bylo od niego mniejsze
idziesz dalej i po ilosci ruchow ograniczonej przez
max mozliwa roznice wysokosci natrafiasz na ..., wiekszy, mniejszy
znow cofasz sie o 1
te kilka punktow po drodze moglo byc mniejszych od tego, w ktorym teraz stoisz
reszta na prawo jest wieksza
szukasz w prawej albo lewej polowie
ale co jezeli cos jest mniejsze od 1 i wieksze od 2 punktu
