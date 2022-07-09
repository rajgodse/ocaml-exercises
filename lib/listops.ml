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

let rec encode_helper acc cnt curr = function
  | [] -> (cnt, curr) :: acc
  | x :: xs ->
      if x = curr then encode_helper acc (cnt + 1) curr xs
      else encode_helper ((cnt, curr) :: acc) 1 x xs

let encode = function [] -> [] | x :: xs -> rev (encode_helper [] 1 x xs)
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

let slice xs i j = if i < 0 || j < i then raise index_exception else takel (dropl xs i) (j - i + 1)