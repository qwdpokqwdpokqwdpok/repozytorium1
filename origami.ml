(* Autor: Wojciech Jarek *)
(* Code review: Krzysztof Łagodziński *)

(* Punkt na plaszczyznie o wspolrzednych (x, y) *)
type point = float * float

(* Poskladana kartka: ile razy kartke przebije szpilka wbita w danym
punkcie *)
type kartka = point -> int

(* Kontrowanie niedokladnosci floatow. Jesli |a - b| < epsilon to "a = b" *)
let epsilon = 1e-9

(* Zwraca kartke, reprezentujaca domkniety
   prostokat o bokach rownoleglych do osi ukladu wspolrzednych i lewym
   dolnym rogu (x1, y1) a prawym gornym (x2, y2). Punkt (x1, y1) musi byc
   nieostro na lewo i w dol od punktu (x2, y2) *)
let prostokat (x1, y1) (x2, y2) =
  let pomiedzy a b x = a -. epsilon <= x && b +. epsilon >= x in
  if x1 > x2 || y1 > y2 then invalid_arg "Ujemne wymiary prostokata"
  else function (x, y) ->
    if pomiedzy x1 x2 x && pomiedzy y1 y2 y then 1 else 0

(* Zwraca kartke, reprezentujaca domkniete kolko o srodku
   w punkcie (ox, oy) i promieniu [r] *)
let kolko (ox, oy) r =
  if r < 0. then invalid_arg "Ujemny promien kola"
  else function (x, y) ->
    if hypot (x -. ox) (y -. oy) <= r +. epsilon then 1 else 0

(* Przyjmuje punkty p1 p2 p3 i liczy iloczyn wektorowy wektorow p1p2 p1p3 *)
let iloczyn_wektorowy (x1, y1) (x2, y2) (x3, y3) =
  (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1)

(* Oblicza punkt symetryczny punktu (x, y) wzgledem prostej przechodzacej
   przez punkty (x1, y1) i (x2, y2) *)
let symetria (x1, y1) (x2, y2) (x, y) =
  let wi = iloczyn_wektorowy (x1, y1) (x2, y2) (x, y)
  and p1p2 = hypot (x2 -. x1) (y2 -. y1) in
  (x +. (wi *. 2.) /. (p1p2 ** 2.) *. (y2 -. y1),
   y +. (wi *. 2.) /. (p1p2 ** 2.) *. (x1 -. x2))

(* Wartosc bezwzgledna dla typu float *)
let abs_float x = if x < 0. then (-.x) else x

(* Sklada kartke [k] wzdluz prostej przechodzacej
   przez punkty [p1] i [p2]. Papier z prawej strony prostej (patrzac w kierunku
   od [p1] do [p2]) jest przekladany na lewa. Punkty [p1] i [p2] musza byc rozne *)
let zloz p1 p2 k =
  if p1 = p2 then invalid_arg "Te same punkty w zloz"
  else function p ->
    let iw = iloczyn_wektorowy p1 p2 p in
    if abs_float iw < epsilon then k p
    else if iw < 0. then 0
    else k p + k (symetria p1 p2 p)

(* Sklada kartke [k] kolejno wzdluz wszystkich prostych z listy [l] *)
let skladaj l k =
  let pom k (p1, p2) = zloz p1 p2 k in
  List.fold_left pom k l

(*
(* Testy *)

let testy =
  let p = prostokat (0.,0.) (10.,10.) in
  let l = skladaj [((5.,0.),(5.,1.));((0.,0.),(0.,12.))] p in
  assert (l (-5., 1.) = 1);
  assert (l (-3., 10.) = 2);
  assert (l (0.1, 0.) = 0);
  let k = zloz (0.,0.) (1.,1.) (kolko (1.,0.) 0.5) in
  assert (k (0.,1.) = 1);
  assert (k (0.5,0.5) = 0);
  let k2 = zloz (1.,1.) (1.,2.) (kolko (2.,2.) 5.) in
  assert (k2 (1.,3.) = 1);
  assert (k2 (0.5, 2.) = 2);
  assert (k2 (1.5, 2.) = 0);
  let p2 = prostokat (-10., -10.) (10., 10.) in
  let l2 = skladaj [((0.,0.),(0.,1.));((0.,0.),(1.,0.))] p2 in
  assert (l2 (0.,0.) = 1);
  assert (l2 (-5.,5.) = 4)
*)
