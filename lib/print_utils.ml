(* Printing utils for printing pasteable values *)
let print_list to_string xs =
  (List.map to_string xs |> List.fold_left (fun acc x -> acc ^ x ^ "; ") "[")
  ^ "]"

let print_option to_string = function
  | None -> "None"
  | Some x -> "Some " ^ to_string x

let print_pair (to_string1, to_string2) (x, y) =
  Printf.sprintf "(%s, %s)" (to_string1 x) (to_string2 y)

let print_string = Printf.sprintf "\"%s\""
let print_string_list = print_list print_string
let print_int_list = print_list Int.to_string
let print_string_option = print_option print_string

let print_string_string_option =
  print_option (print_pair (print_string, print_string))

let print_string_list_list = print_list print_string_list
let print_int_string = print_pair (Int.to_string, print_string)
let print_int_string_list = print_list print_int_string
let print_string_list_list_list = print_list print_string_list_list

let print_rle to_string = function
  | Listops.Uno x -> "Uno " ^ to_string x
  | Listops.Muchos (n, x) ->
      "Muchos " ^ print_pair (Int.to_string, to_string) (n, x)

let print_string_rle_list = print_list (print_rle print_string)
let print_string_list_pair = print_pair (print_string_list, print_string_list)