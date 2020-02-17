let energia k t e =
  let check arr =
    (Array.fold_left
      (fun acc x -> acc + t.(x))
      0
      arr)
      = e in
  
