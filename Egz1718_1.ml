type symbol =
| Liczba of int
| Plus | Minus | Razy | Podziel
| Nawias_Otw | Nawias_Zam;;

let kalkulator lista =
  let wykonaj a (y, 0, 0, 0, 0) =
    match a with
    | (0, 0, 0, 0, 0) -> (y, 0, 0, 0, 0)
    | (x, 1, 0, 0, 0) -> (x + y, 0, 0, 0, 0)
    | (x, 0, 1, 0, 0) -> (x - y, 0, 0, 0, 0)
    | (x, 0, 0, 1, 0) -> (x * y, 0, 0, 0, 0)
    | (x, 0, 0, 0, 1) -> (x / y, 0, 0, 0, 0) in
  let plus = function
    | (x, 0, 0, 0, 0) -> (x, 1, 0, 0, 0) in
  let minus = function
    | (x, 0, 0, 0, 0) -> (x, 0, 1, 0, 0) in
  let razy = function
    | (x, 0, 0, 0, 0) -> (x, 0, 0, 1, 0) in
  let podz = function
    | (x, 0, 0, 0, 0) -> (x, 0, 0, 0, 1) in
  let otw = function
    | l -> (0,0,0,0,0)::l in
  let zam = function
    | a::b::c -> (wykonaj b a)::c in
  let wynik =
  List.fold_left
    (fun (a::b) s -> match s with
      | Liczba x -> (wykonaj a (x, 0, 0, 0, 0))::b
      | Plus -> (plus a)::b
      | Minus -> (minus a)::b
      | Razy -> (razy a)::b
      | Podziel -> (podz a)::b
      | Nawias_Otw -> otw (a::b)
      | Nawias_Zam -> zam (a::b))
    [(0,0,0,0,0)]
    lista in
  match wynik with 
  

