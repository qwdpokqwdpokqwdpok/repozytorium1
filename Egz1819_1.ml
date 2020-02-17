let transpose lista =
  let odwrocone =
    (List.fold_left
      (fun acc l ->
        let doklejanie =
          (List.fold_left
            (fun ((hd1::tl1), (hd2::tl2)) x -> (((x::hd2)::hd1::tl1), tl2))
            ([[]], acc)
            l) in
        match doklejanie with (l, _) ->
          List.tl (List.rev l))
      []
      lista) in
  List.map (List.rev) odwrocone;;
       
