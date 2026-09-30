let p_001 =
  let rec aux curr ~accum ~limit =
    match curr with
    | _ when curr >= limit -> accum
    | _ ->
      if curr mod 3 = 0 || curr mod 5 = 0 then
        aux (curr + 1) ~accum:(accum + curr) ~limit
      else
        aux (curr + 1) ~accum ~limit
  in
  aux 1 ~accum:0 ~limit:1000
;;

let p_002 =
  let rec aux a b ~accum ~limit =
    let next = a + b in
    match next with
    | _ when next >= limit -> accum
    | _ ->
      if next mod 2 = 0 then
        aux b next ~accum:(accum + next) ~limit
      else
        aux b next ~accum ~limit
  in
  aux 1 1 ~accum:0 ~limit:4_000_000
;;

let p_003 num =
  let rec factor n divisor factors =
    if n = 1 then
      factors
    else if divisor * divisor > n then
      n :: factors
    else if n mod divisor = 0 then
      factor (n / divisor) divisor (divisor :: factors)
    else
      factor n (divisor + 1) factors
  in
  let result =
    if num < 2 then
      []
    else
      factor num 2 []
  in
  List.sort_uniq compare result
;;

let p_004 num_max =
  let reversed s = String.init (String.length s) (fun i -> s.[String.length s - 1 - i]) in
  let is_palindrome n =
    let str = string_of_int n in
    str = reversed str
  in
  let advance (a, b) =
    if b = 1 then
      (a - 1, num_max)
    else
      (a, b - 1)
  in
  let rec search (a, b) ~max =
    let prod = a * b in
    let next = advance (a, b) in
    if is_palindrome prod then
      if prod = 1 then
        max
      else if prod > max then
        search next ~max:prod
      else
        search next ~max
    else
      search next ~max
  in
  search (num_max, num_max) ~max:0
;;

let p_005 start_num end_num =
  let nums = List.init (end_num - start_num + 1) (fun i -> start_num + i) in
  let lcm = List.fold_left Utils.lcm 1 nums in
  lcm
;;

let p_006 n =
  let sum = n * (n + 1) / 2 in
  let square_of_sum = sum * sum in
  let sum_of_square = n * (n + 1) * ((2 * n) + 1) / 6 in
  square_of_sum - sum_of_square
;;

let p_007 limit =
  let rec check d primes_found latest =
    if primes_found = limit then
      latest
    else if Utils.is_prime d then
      check (d + 1) (primes_found + 1) d
    else
      check (d + 1) primes_found latest
  in
  match limit with
  | _ when limit < 1 -> failwith "limit must be atleast 1"
  | _ -> check 2 0 0
;;

let p_008 filename window_size =
  let content = In_channel.with_open_text filename In_channel.input_all |> String.trim in
  let rec next_chunk position =
    if position + window_size > String.length content then
      None
    else
      Some (String.sub content position window_size)
  in
  let product_of_digits s =
    String.fold_left (fun product c -> product * (Char.code c - Char.code '0')) 1 s
  in
  let rec search position max_prod max_chunk =
    match next_chunk position with
    | Some chunk -> begin
      let prod = product_of_digits chunk in
      if prod > max_prod then
        search (position + 1) prod chunk
      else
        search (position + 1) max_prod max_chunk
      end
    | None -> (max_prod, max_chunk)
  in
  search 0 0 ""
;;

let p_009 =
  let rec search_a a =
    if a >= 333 then
      failwith "couldn't find a triplet"
    else begin
      let rec search_b b =
        let c = 1000 - a - b in
        if b >= c then
          search_a (a + 1)
        else if (a * a) + (b * b) = c * c then
          a * b * c
        else
          search_b (b + 1)
      in
      search_b (a + 1)
    end
  in
  search_a 1
;;

let p_010 limit =
  let rec aux curr accum =
    if curr > limit then
      accum
    else if Utils.is_prime curr then
      aux (curr + 1) (accum + curr)
    else
      aux (curr + 1) accum
  in
  aux 1 0
;;

let p_011 filename =
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let transform_line s =
    List.map (fun x -> int_of_string x) (String.split_on_char ' ' s) |> Array.of_list
  in
  let grid = List.map transform_line lines |> Array.of_list in

  let n_rows = Array.length grid in
  let n_cols = Array.length grid.(0) in
  let window = 4 in
  let directions = [ (0, 1); (1, 0); (1, 1); (1, -1) ] in

  let rec product_at row col (row_step, col_step) =
    let rec aux offset prod =
      let r = row + (offset * row_step) in
      let c = col + (offset * col_step) in
      if offset >= window then
        prod
      else if r < 0 || r >= n_rows || c < 0 || c >= n_cols then
        0
      else
        aux (offset + 1) (prod * grid.(r).(c))
    in
    aux 0 1
  in

  let advance (i, j) =
    if i = n_rows - 1 then
      None
    else if j = n_cols - 1 then
      Some (i + 1, 0)
    else
      Some (i, j + 1)
  in

  let rec search (i, j) best_prod best_info =
    let best_prod, best_info =
      List.fold_left
        (fun (curr_max, curr_info) direction ->
           let prod = product_at i j direction in
           if prod > curr_max then
             (prod, Some ((i, j), direction))
           else
             (curr_max, curr_info))
        (best_prod, best_info)
        directions
    in

    match advance (i, j) with
    | None -> (best_prod, best_info)
    | Some next_position -> search next_position best_prod best_info
  in

  search (0, 0) 0 None
;;
