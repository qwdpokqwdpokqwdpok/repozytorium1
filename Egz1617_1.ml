let ostatki ll =
  List.fold_left
    (fun acc l ->
      if l = [] then acc
      else List.append acc [(List.hd (List.rev l))])
    []
    ll;;
