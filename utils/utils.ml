let print_int (result : int) = Printf.printf "%d\n" result
let print_list (result : int list) = List.iter (fun x -> Printf.printf "%d " x) result

let list_min lst =
  match lst with
  | [] -> invalid_arg "empty list"
  | first :: rest -> List.fold_left min first rest
;;

let list_max lst =
  match lst with
  | [] -> invalid_arg "empty list"
  | first :: rest -> List.fold_left max first rest
;;

let rec gcd a b =
  if b = 0 then
    abs a
  else
    gcd b (a mod b)
;;

let lcm a b =
  if a = 0 || b = 0 then
    0
  else
    (* to avoid integer overflow, it is written in the following way instead of (a * b) / gcd a b *)
    abs (a / gcd a b * b)
;;

let is_prime n =
  let rec check d =
    if d * d > n then
      true
    else if n mod d = 0 then
      false
    else
      check (d + 1)
  in
  if n < 2 then
    false
  else
    check 2
;;
