type 'a heap = Node of 'a heap * 'a * 'a heap | Null;;

let rekonsrukcja arr =
  let dodaj_korzen h k =
    Node of h * k * Null in
  let dodaj_prawe_poddrzewo (Node of h1 * k * Null) h2 =
    Node of h1 * k * h2 in
  let nowe_poddrzewo k =
    Node of Null * h * Null in
  let wynik =
    Array.fold_left
      (fun (
