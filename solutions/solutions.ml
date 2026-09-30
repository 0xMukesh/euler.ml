let p_001 () =
  let rec aux curr accum limit =
    match curr with
    | _ when curr >= limit -> accum
    | _ ->
      if curr mod 3 = 0 || curr mod 5 = 0 then
        aux (curr + 1) (accum + curr) limit
      else
        aux (curr + 1) accum limit
  in

  aux 1 0 1000
;;

let p_002 () =
  let rec aux a b accum limit =
    let next = a + b in
    match next with
    | _ when next >= limit -> accum
    | _ ->
      if next mod 2 = 0 then
        aux b next (accum + next) limit
      else
        aux b next accum limit
  in

  aux 1 1 0 4_000_000
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

let p_004 max_num =
  let reversed s = String.init (String.length s) (fun i -> s.[String.length s - 1 - i]) in

  let is_palindrome n =
    let str = string_of_int n in
    str = reversed str
  in

  let advance (a, b) =
    if b = 1 then
      (a - 1, max_num)
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

  search (max_num, max_num) ~max:0
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

let p_007 nth =
  let rec check d primes_found latest =
    if primes_found = nth then
      latest
    else if Utils.is_prime d then
      check (d + 1) (primes_found + 1) d
    else
      check (d + 1) primes_found latest
  in

  match nth with
  | _ when nth < 1 -> failwith "nth must be at least 1"
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
    let accum product c = product * (Char.code c - Char.code '0') in
    String.fold_left accum 1 s
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

let p_009 () =
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

let p_010 () =
  let limit = 2_000_000 in

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
    let accum (curr_max, curr_info) direction =
      let prod = product_at i j direction in
      if prod > curr_max then
        (prod, Some ((i, j), direction))
      else
        (curr_max, curr_info)
    in
    let best_prod, best_info = List.fold_left accum (best_prod, best_info) directions in

    match advance (i, j) with
    | None -> (best_prod, best_info)
    | Some next_position -> search next_position best_prod best_info
  in

  search (0, 0) 0 None
;;

let p_012 num_divisors =
  let count_divisors n =
    if n <= 0 then
      invalid_arg "n must be positive";

    let rec loop i count =
      if i * i > n then
        count
      else if n mod i = 0 then
        if i * i = n then
          loop (i + 1) (count + 1)
        else
          loop (i + 1) (count + 2)
      else
        loop (i + 1) count
    in
    loop 1 0
  in

  let rec search i =
    let triangle_number = i * (i + 1) / 2 in
    if count_divisors triangle_number >= num_divisors then
      triangle_number
    else
      search (i + 1)
  in

  search 1
;;

let p_013 filename =
  let nums = In_channel.with_open_text filename In_channel.input_lines in
  let accum total num = Z.add total (Z.of_string num) in
  let sum = List.fold_left accum Z.zero nums in
  let result = Z.to_string sum |> fun s -> String.sub s 0 10 in
  result
;;

let p_014 () =
  let limit = 1_000_000 in

  let rec calc_chain_length num accum =
    if num = 1 then
      accum
    else
      begin if
        num mod 2 = 0
      then
        calc_chain_length (num / 2) (accum + 1)
      else
        calc_chain_length ((3 * num) + 1) (accum + 1)
      end
  in

  let rec search num max_length max_num =
    if num >= limit then
      max_num
    else begin
      let chain_length = calc_chain_length num 1 in
      if chain_length > max_length then
        search (num + 1) chain_length num
      else
        search (num + 1) max_length max_num
    end
  in

  search 1 0 0
;;

(*
  every route consists of N right moves and N down moves.
  therefore, total number of possible routes = 2n_C_n = (2n!)/(n! * n!)
*)
let p_015 n =
  let rec fact n prod =
    if n = Z.one then
      prod
    else
      fact (Z.sub n Z.one) (Z.mul prod n)
  in
  let fact_n = fact (Z.of_int n) Z.one in
  let fact_2n = fact (Z.of_int (2 * n)) Z.one in
  Z.div fact_2n (Z.mul fact_n fact_n) |> Z.to_string
;;

let p_016 () =
  let desired_power = 1000 in
  let known_power = 15 in
  let two_power_fifteen = 32768 in

  let quotient = desired_power / known_power in
  let reminder = desired_power mod known_power in

  let rec pow num n prod =
    if n = Z.zero then
      prod
    else
      pow num (Z.sub n Z.one) (Z.mul prod num)
  in

  let quotient_part = pow (Z.of_int two_power_fifteen) (Z.of_int quotient) Z.one in
  let reminder_part = pow (Z.of_int 2) (Z.of_int reminder) Z.one in
  let result = Z.mul quotient_part reminder_part in

  let sum_of_digits =
    Z.to_string result
    |> String.to_seq
    |> Seq.map (fun c -> Char.code c - Char.code '0')
    |> Seq.fold_left ( + ) 0
  in

  sum_of_digits
;;
