type 'a node =
  | One of 'a 
  | Many of 'a node list

type 'a rle =
  | Uno of 'a
  | Muchos of int * 'a

val index_exception : exn
val replicate_exception : exn
val drop_exception: exn

val last : 'a list -> 'a option
val last_two : 'a list -> ('a * 'a) option
val nth : 'a list -> int -> 'a
val length : 'a list -> int
val rev : 'a list -> 'a list
val palindrome: 'a list -> bool
val flatten: 'a node list -> 'a list
val compress: 'a list -> 'a list
val pack: 'a list -> 'a list list
val encode: 'a list -> (int * 'a) list
val modified_encode: 'a list -> 'a rle list
val decode: 'a rle list -> 'a list
val duplicate: 'a list -> 'a list
val replicate: 'a list -> int -> 'a list
val drop: 'a list -> int -> 'a list
val split: 'a list -> int -> 'a list * 'a list
val slice: 'a list -> int -> int -> 'a list
val rotate: 'a list -> int -> 'a list
val remove_at: int -> 'a list -> 'a list
val insert_at: 'a -> int -> 'a list -> 'a list
val range: int -> int -> int list
val rand_select: 'a list -> int -> 'a list
val lotto_select: int -> int -> int list
val permutation: 'a list -> 'a list
val extract: int -> 'a list -> 'a list list
val group: 'a list -> int list -> 'a list list list
val length_sort: 'a list list -> 'a list list
val frequency_sort: 'a list list -> 'a list list