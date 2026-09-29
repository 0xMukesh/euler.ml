let p_001 =
  let rec aux curr ~accum ~limit =
    let gen = aux (curr + 1) ~limit in
    match curr with
    | _ when curr >= limit -> accum
    | _ ->
      if curr mod 3 = 0 || curr mod 5 = 0 then
        gen ~accum:(accum + curr)
      else
        gen ~accum
  in
  aux 1 ~accum:0 ~limit:1000
;;

let p_002 =
  let rec aux a b ~accum ~limit =
    let next = a + b in
    let gen = aux b next ~limit in
    match next with
    | _ when next >= limit -> accum
    | _ ->
      if next mod 2 = 0 then
        gen ~accum:(accum + next)
      else
        gen ~accum
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
      a - 1, num_max
    else
      a, b - 1
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
  let square_of_sum = int_of_float (float_of_int sum ** 2.) in
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
