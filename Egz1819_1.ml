let transpose lista =
  let jadro (l1, l2) x =
    match (l1, l2) with
    | ([], []) -> ([[x]], [])
    | ([], (hd2::tl2)) -> ([(x::hd2)], tl2)
    | ((hd1::tl1), []) -> (([x]::hd1::tl1), [])
    | ((hd1::tl1), (hd2::tl2)) -> (((x::hd2)::hd1::tl1), tl2) in
  let odwrocone =
    (List.fold_left
      (fun acc l ->
        let doklejanie =
          (List.fold_left
            (jadro)
            ([], acc)
            l) in
        match doklejanie with (l, _) ->
          List.rev l)
      []
      lista) in
  List.map (List.rev) odwrocone;;
       
