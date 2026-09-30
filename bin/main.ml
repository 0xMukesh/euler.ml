let () =
  let best_prod, best_info = Solutions.p_011 "./_data/011.txt" in
  match best_info with
  | None -> print_endline "no product found"
  | Some ((row, col), (row_step, col_step)) ->
    Printf.printf
      "found max product\nstart: (%d, %d)\ndirection: (%d, %d)\n"
      row
      col
      row_step
      col_step
;;
