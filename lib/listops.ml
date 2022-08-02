(*
 * This file contains all exercises related to list operations
 * All functions are implemented tail recursively
 *)

type 'a node = One of 'a | Many of 'a node list
type 'a rle = Uno of 'a | Muchos of int * 'a

let replicate_exception =
  Invalid_argument "replication number must not be negative"

let drop_exception = Invalid_argument "drop stride must be positive"
let index_exception = Invalid_argument "index out of bounds"
let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

let rec nth_helper xs n =
  match (xs, n) with
  | [], _ -> raise index_exception
  | x :: _, 0 -> x
  | _ :: ys, n -> nth_helper ys (n - 1)

let nth xs n = if n < 0 then raise index_exception else nth_helper xs n

let rec length_helper acc = function
  | [] -> acc
  | _ :: xs -> length_helper (acc + 1) xs

(* Value restriction prevents omission of argument `xs` *)
let length xs = length_helper 0 xs

let rec rev_helper acc = function
  | [] -> acc
  | x :: xs -> rev_helper (x :: acc) xs

(* Value restriction prevents omission of argument `xs` *)
let rev xs = rev_helper [] xs
let palindrome xs = rev xs = xs

let rec flatten_helper acc = function
  | [] -> acc
  | One x :: xs -> flatten_helper (x :: acc) xs
  | Many ys :: xs -> flatten_helper (flatten_helper acc ys) xs

let flatten xs = rev (flatten_helper [] xs)

let rec compress_helper acc last = function
  | [] -> last :: acc
  | x :: xs ->
      if last = x then compress_helper acc last xs
      else compress_helper (last :: acc) x xs

let compress = function [] -> [] | x :: xs -> rev (compress_helper [] x xs)

let rec pack_helper acc curr last = function
  | [] -> (last :: curr) :: acc
  | x :: xs ->
      if x = last then pack_helper acc (last :: curr) x xs
      else pack_helper ((last :: curr) :: acc) [] x xs

let pack = function [] -> [] | x :: xs -> rev (pack_helper [] [] x xs)

let encode_generic equals update init return =
  let rec helper acc cnt curr = function
    | [] -> (cnt, return curr) :: acc
    | x :: xs ->
        if equals x curr then helper acc (cnt + 1) (update curr x) xs
        else helper ((cnt, return curr) :: acc) 1 (init x) xs
  in
  function [] -> [] | x :: xs -> rev (helper [] 1 (init x) xs)

let encode xs = encode_generic ( = ) (fun _ x -> x) Fun.id Fun.id xs
let run_to_rle cnt curr = if cnt = 1 then Uno curr else Muchos (cnt, curr)

let rec modified_encode_helper acc cnt curr = function
  | [] -> run_to_rle cnt curr :: acc
  | x :: xs ->
      if x = curr then modified_encode_helper acc (cnt + 1) curr xs
      else modified_encode_helper (run_to_rle cnt curr :: acc) 1 x xs

let modified_encode = function
  | [] -> []
  | x :: xs -> rev (modified_encode_helper [] 1 x xs)

let rec decode_helper acc = function
  | [] -> acc
  | Uno x :: xs -> decode_helper (x :: acc) xs
  | Muchos (n, x) :: xs ->
      decode_helper (x :: acc) (if n = 1 then xs else Muchos (n - 1, x) :: xs)

let decode xs = rev (decode_helper [] xs)

let rec duplicate_helper acc = function
  | [] -> acc
  | x :: xs -> duplicate_helper (x :: x :: acc) xs

let duplicate xs = rev (duplicate_helper [] xs)

let replicate xs n =
  let rec helper acc remaining = function
    | [] -> acc
    | x :: xs ->
        if remaining = 0 then helper acc n xs
        else helper (x :: acc) (remaining - 1) (x :: xs)
  in
  if n < 0 then raise replicate_exception else rev (helper [] n xs)

let drop xs n =
  let rec helper acc n' = function
    | [] -> acc
    | x :: xs ->
        if n' = 1 then helper acc n xs else helper (x :: acc) (n' - 1) xs
  in
  if n <= 0 then raise drop_exception else rev (helper [] n xs)

let rec split_helper acc n = function
  | [] -> (acc, [])
  | x :: xs ->
      if n = 0 then (acc, x :: xs) else split_helper (x :: acc) (n - 1) xs

let split xs n =
  if n < 0 then raise index_exception
  else
    let ys, zs = split_helper [] n xs in
    (rev ys, zs)

let rec dropl xs = function
  | 0 -> xs
  | n -> (
      match xs with [] -> raise index_exception | _ :: ys -> dropl ys (n - 1))

let rec takel_helper acc xs = function
  | 0 -> acc
  | n -> (
      match xs with
      | [] -> raise index_exception
      | y :: ys -> takel_helper (y :: acc) ys (n - 1))

let takel xs n = rev (takel_helper [] xs n)

let slice xs i j =
  if i < 0 || j < i then raise index_exception
  else takel (dropl xs i) (j - i + 1)

let rec append_helper acc = function
  | [] -> acc
  | x :: xs -> append_helper (x :: acc) xs

let append xs ys =
  let rxs = rev xs in
  rev (append_helper rxs ys)

let modulo x y =
  let r = x mod y in
  if r < 0 then r + y else r

let rotate xs i =
  let n = length xs in
  if n = 0 then []
  else
    let i' = modulo i n in
    let ys, zs = split xs i' in
    append zs ys

let remove_at i xs =
  let ys, zs = split xs i in
  match zs with [] -> raise index_exception | _ :: zs' -> append ys zs'

let insert_at x i xs =
  let ys, zs = split xs i in
  append ys (x :: zs)

let rec range_helper acc x y =
  if y < x then acc else range_helper (y :: acc) x (y - 1)

let range x y = if x > y then rev (range_helper [] y x) else range_helper [] x y

let rec unwind acc i xs =
  match (i, xs) with
  | 0, _ -> (acc, xs)
  | _, [] -> raise index_exception
  | i, y :: ys -> unwind (y :: acc) (i - 1) ys

let rec rewind (ys, zs) =
  match ys with [] -> zs | y :: ys' -> rewind (ys', y :: zs)

let replace_at x i xs =
  if i < 0 then raise index_exception
  else
    match unwind [] i xs with
    | _, [] -> raise index_exception
    | acc, _ :: ys -> rewind (acc, x :: ys)

(* The proof that this meets the spec is left as an exercise *)
let rand_select_impl xs n =
  let rec helper acc total = function
    | [] -> acc
    | y :: ys ->
        let total' = total + 1 in
        let r = Random.full_int total' in
        let acc' = if r < n then replace_at y r acc else acc in
        helper acc' total' ys
  in
  match unwind [] n xs with acc, ys -> helper acc n ys

let lotto_select_impl n m =
  if m <= 0 || n < 0 || n > m then raise index_exception
  else rand_select_impl (range 1 m) n

let poll_random first remaining =
  let rec helper selected rest total = function
    | [] -> (selected, rest)
    | y :: ys ->
        let total' = total + 1 in
        if Random.full_int total' = 0 then helper y (selected :: rest) total' ys
        else helper selected (y :: rest) total' ys
  in
  helper first [] 1 remaining

let permutation_impl xs =
  let rec helper acc = function
    | [] -> acc
    | y :: ys -> ( match poll_random y ys with z, zs -> helper (z :: acc) zs)
  in
  helper [] xs

(* let true_rand f = *)
(* let _ = Random.self_init () in *)
(* f () *)

let test_rand f =
  let _ = Random.init 15251 in
  f ()

let rand_select xs n = test_rand (fun _ -> rand_select_impl xs n)
let lotto_select n m = test_rand (fun _ -> lotto_select_impl n m)
let permutation xs = test_rand (fun _ -> permutation_impl xs)

(* This implementation uses Continuation Passing Style *)
let extract n xs =
  let rec helper k acc curr xs = function
    | 0 -> k (curr :: acc)
    | i -> (
        match xs with
        | [] -> k acc
        | y :: ys ->
            let k' acc' = helper k acc' (y :: curr) ys (i - 1) in
            helper k' acc curr ys i)
  in
  if n < 0 then raise index_exception
  else if n = 0 then [ [] ]
  else helper Fun.id [] [] xs n

let extract_with_residue n xs =
  let rec helper k acc curr rest xs = function
    | 0 -> k ((curr, rewind (rest, xs)) :: acc)
    | i -> (
        match xs with
        | [] -> k acc
        | y :: ys ->
            let k' acc' = helper k acc' (y :: curr) ys rest (i - 1) in
            helper k' acc curr (y :: rest) ys i)
  in
  if n < 0 then raise index_exception
  else if n = 0 then [ ([], xs) ]
  else helper Fun.id [] [] [] xs n

let group xs sizes =
  let combine groups combos =
    List.map (fun (chosen, unchosen) -> (unchosen, chosen :: groups)) combos
  in
  let expand size (rest, groups) =
    extract_with_residue size rest |> combine groups
  in
  List.fold_left
    (fun partials size -> List.map (expand size) partials |> List.flatten)
    [ (xs, []) ]
    sizes
  |> List.map (fun (_, groups) -> rev groups)

let insert cmp x =
  let rec helper acc = function
    | [] -> rewind (acc, [ x ])
    | y :: ys ->
        if cmp x y < 0 then rewind (acc, x :: y :: ys) else helper (y :: acc) ys
  in
  helper []

let sort cmp = List.fold_left (fun xs x -> insert cmp x xs) []
let length_sort xs = sort (fun xs ys -> length xs - length ys) xs

let encode_length xs =
  encode_generic
    (fun ps qs ->
      match qs with [] -> false | rs :: _ -> length ps = length rs)
    (fun acc x -> x :: acc)
    (fun x -> [ x ])
    rev xs

let frequency_sort xs =
  length_sort xs |> encode_length
  |> sort (fun (n, _) (m, _) -> n - m)
  |> List.map (fun (_, xs) -> xs)
  |> List.flatten
