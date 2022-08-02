(*
 * Test suite for lib/listops.ml
 *)

open OUnit2
open Problems
open Print_utils

let last_tests =
  "last"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal None (Listops.last []) ~printer:print_string_option );
         ( "inhabited" >:: fun _ ->
           assert_equal (Some "d")
             (Listops.last [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_option );
       ]

let last_two_tests =
  "last_two"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal None (Listops.last_two [])
             ~printer:print_string_string_option );
         ( "singleton" >:: fun _ ->
           assert_equal None (Listops.last_two [ "a" ])
             ~printer:print_string_string_option );
         ( "big enough" >:: fun _ ->
           assert_equal
             (Some ("c", "d"))
             (Listops.last_two [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_string_option );
       ]

let nth_tests =
  "nth"
  >::: [
         ( "negative index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.nth [ "a"; "b" ] (-1)) );
         ( "big index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.nth [ "a"; "b" ] 2) );
         ( "first of empty" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ -> Listops.nth [] 0) );
         ( "front" >:: fun _ ->
           assert_equal "a"
             (Listops.nth [ "a"; "b"; "c"; "d"; "e" ] 0)
             ~printer:Fun.id );
         ( "middle" >:: fun _ ->
           assert_equal "c"
             (Listops.nth [ "a"; "b"; "c"; "d"; "e" ] 2)
             ~printer:Fun.id );
         ( "back" >:: fun _ ->
           assert_equal "e"
             (Listops.nth [ "a"; "b"; "c"; "d"; "e" ] 4)
             ~printer:Fun.id );
       ]

let length_tests =
  "length"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal 0 (Listops.length []) ~printer:Int.to_string );
         ( "inhabited" >:: fun _ ->
           assert_equal 4
             (Listops.length [ "a"; "b"; "c"; "d" ])
             ~printer:Int.to_string );
       ]

let rev_tests =
  "rev"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.rev []) ~printer:print_string_list );
         ( "inhabited" >:: fun _ ->
           assert_equal [ "d"; "c"; "b"; "a" ]
             (Listops.rev [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
       ]

let palindrome_tests =
  "palindrome"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal true (Listops.palindrome []) ~printer:Bool.to_string );
         ( "singleton" >:: fun _ ->
           assert_equal true
             (Listops.palindrome [ "a" ])
             ~printer:Bool.to_string );
         ( "odd palindrome" >:: fun _ ->
           assert_equal true
             (Listops.palindrome [ "a"; "b"; "a" ])
             ~printer:Bool.to_string );
         ( "even palindrome" >:: fun _ ->
           assert_equal true
             (Listops.palindrome [ "a"; "b"; "b"; "a" ])
             ~printer:Bool.to_string );
         ( "odd non-palindrome" >:: fun _ ->
           assert_equal false
             (Listops.palindrome [ "a"; "b"; "b" ])
             ~printer:Bool.to_string );
         ( "even non-palindrome" >:: fun _ ->
           assert_equal false
             (Listops.palindrome [ "a"; "b"; "a"; "b" ])
             ~printer:Bool.to_string );
       ]

let flatten_tests =
  "flatten"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.flatten []) ~printer:print_string_list );
         ( "all ones" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.flatten [ One "a"; One "b"; One "c" ])
             ~printer:print_string_list );
         ( "all singletons" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.flatten
                [ Many [ One "a" ]; Many [ One "b" ]; Many [ One "c" ] ])
             ~printer:print_string_list );
         ( "very nested" >:: fun _ ->
           assert_equal [ "a" ]
             (Listops.flatten
                [ Many [ Many [ Many [ Many [ Many [ One "a" ] ] ] ] ] ])
             ~printer:print_string_list );
         ( "mixed" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e" ]
             (Listops.flatten
                [
                  One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ];
                ])
             ~printer:print_string_list );
       ]

let compress_tests =
  "compress"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.compress []) ~printer:print_string_list );
         ( "singleton" >:: fun _ ->
           assert_equal [ "a" ] (Listops.compress [ "a" ])
             ~printer:print_string_list );
         ( "run" >:: fun _ ->
           assert_equal [ "a" ]
             (Listops.compress [ "a"; "a"; "a"; "a"; "a"; "a" ])
             ~printer:print_string_list );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "b"; "a"; "c" ]
             (Listops.compress [ "a"; "b"; "c"; "b"; "a"; "c" ])
             ~printer:print_string_list );
         ( "mixed" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "a"; "d"; "e" ]
             (Listops.compress
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ])
             ~printer:print_string_list );
       ]

let pack_tests =
  "pack"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.pack []) ~printer:print_string_list_list );
         ( "singleton" >:: fun _ ->
           assert_equal [ [ "a" ] ] (Listops.pack [ "a" ])
             ~printer:print_string_list_list );
         ( "run" >:: fun _ ->
           assert_equal
             [ [ "a"; "a"; "a"; "a"; "a"; "a" ] ]
             (Listops.pack [ "a"; "a"; "a"; "a"; "a"; "a" ])
             ~printer:print_string_list_list );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ [ "a" ]; [ "b" ]; [ "c" ]; [ "b" ]; [ "a" ]; [ "c" ] ]
             (Listops.pack [ "a"; "b"; "c"; "b"; "a"; "c" ])
             ~printer:print_string_list_list );
         ( "mixed" >:: fun _ ->
           assert_equal
             [
               [ "a"; "a"; "a"; "a" ];
               [ "b" ];
               [ "c"; "c" ];
               [ "a"; "a" ];
               [ "d"; "d" ];
               [ "e"; "e"; "e"; "e" ];
             ]
             (Listops.pack
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ])
             ~printer:print_string_list_list );
       ]

let encode_tests =
  "encode"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.encode []) ~printer:print_int_string_list );
         ( "singleton" >:: fun _ ->
           assert_equal
             [ (1, "a") ]
             (Listops.encode [ "a" ]) ~printer:print_int_string_list );
         ( "run" >:: fun _ ->
           assert_equal
             [ (6, "a") ]
             (Listops.encode [ "a"; "a"; "a"; "a"; "a"; "a" ])
             ~printer:print_int_string_list );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ (1, "a"); (1, "b"); (1, "c"); (1, "b"); (1, "a"); (1, "c") ]
             (Listops.encode [ "a"; "b"; "c"; "b"; "a"; "c" ])
             ~printer:print_int_string_list );
         ( "mixed" >:: fun _ ->
           assert_equal
             [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
             (Listops.encode
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ])
             ~printer:print_int_string_list );
       ]

let modified_encode_tests =
  "modified encode"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal []
             (Listops.modified_encode [])
             ~printer:print_string_rle_list );
         ( "singleton" >:: fun _ ->
           assert_equal [ Listops.Uno "a" ]
             (Listops.modified_encode [ "a" ])
             ~printer:print_string_rle_list );
         ( "run" >:: fun _ ->
           assert_equal
             [ Listops.Muchos (6, "a") ]
             (Listops.modified_encode [ "a"; "a"; "a"; "a"; "a"; "a" ])
             ~printer:print_string_rle_list );
         ( "no runs" >:: fun _ ->
           assert_equal
             [
               Listops.Uno "a";
               Listops.Uno "b";
               Listops.Uno "c";
               Listops.Uno "b";
               Listops.Uno "a";
               Listops.Uno "c";
             ]
             (Listops.modified_encode [ "a"; "b"; "c"; "b"; "a"; "c" ])
             ~printer:print_string_rle_list );
         ( "mixed" >:: fun _ ->
           assert_equal
             [
               Listops.Muchos (4, "a");
               Listops.Uno "b";
               Listops.Muchos (2, "c");
               Listops.Muchos (2, "a");
               Listops.Uno "d";
               Listops.Muchos (4, "e");
             ]
             (Listops.modified_encode
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ])
             ~printer:print_string_rle_list );
       ]

let decode_tests =
  "decode"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.decode []) ~printer:print_string_list );
         ( "singleton" >:: fun _ ->
           assert_equal [ "a" ]
             (Listops.decode [ Listops.Uno "a" ])
             ~printer:print_string_list );
         ( "run" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "a"; "a"; "a"; "a" ]
             (Listops.decode [ Listops.Muchos (6, "a") ])
             ~printer:print_string_list );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "b"; "a"; "c" ]
             (Listops.decode
                [
                  Listops.Uno "a";
                  Listops.Uno "b";
                  Listops.Uno "c";
                  Listops.Uno "b";
                  Listops.Uno "a";
                  Listops.Uno "c";
                ])
             ~printer:print_string_list );
         ( "mixed" >:: fun _ ->
           assert_equal
             [
               "a";
               "a";
               "a";
               "a";
               "b";
               "c";
               "c";
               "a";
               "a";
               "d";
               "e";
               "e";
               "e";
               "e";
             ]
             (Listops.decode
                [
                  Listops.Muchos (4, "a");
                  Listops.Uno "b";
                  Listops.Muchos (2, "c");
                  Listops.Muchos (2, "a");
                  Listops.Uno "d";
                  Listops.Muchos (4, "e");
                ])
             ~printer:print_string_list );
       ]

let duplicate_tests =
  "duplicate"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.duplicate []) ~printer:print_string_list );
         ( "inhabited" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "b"; "b"; "c"; "c" ]
             (Listops.duplicate [ "a"; "b"; "c" ])
             ~printer:print_string_list );
       ]

let replicate_tests =
  "replicate"
  >::: [
         ( "negative n" >:: fun _ ->
           assert_raises Listops.replicate_exception (fun _ ->
               Listops.replicate [ "a"; "b" ] (-1)) );
         ( "empty zero" >:: fun _ ->
           assert_equal [] (Listops.replicate [] 0) ~printer:print_string_list
         );
         ( "empty double" >:: fun _ ->
           assert_equal [] (Listops.replicate [] 2) ~printer:print_string_list
         );
         ( "inhabited zero" >:: fun _ ->
           assert_equal []
             (Listops.replicate [ "a"; "b"; "c" ] 0)
             ~printer:print_string_list );
         ( "inhabited id" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.replicate [ "a"; "b"; "c" ] 1)
             ~printer:print_string_list );
         ( "inhabited double" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "b"; "b"; "c"; "c" ]
             (Listops.replicate [ "a"; "b"; "c" ] 2)
             ~printer:print_string_list );
         ( "inhabited triple" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
             (Listops.replicate [ "a"; "b"; "c" ] 3)
             ~printer:print_string_list );
       ]

let drop_tests =
  "drop"
  >::: [
         ( "negative n" >:: fun _ ->
           assert_raises Listops.drop_exception (fun _ ->
               Listops.drop [ "a"; "b" ] (-1)) );
         ( "zero n" >:: fun _ ->
           assert_raises Listops.drop_exception (fun _ ->
               Listops.drop [ "a"; "b" ] 0) );
         ( "empty delete" >:: fun _ ->
           assert_equal [] (Listops.drop [] 1) ~printer:print_string_list );
         ( "empty skip" >:: fun _ ->
           assert_equal [] (Listops.drop [] 3) ~printer:print_string_list );
         ( "inhabited delete" >:: fun _ ->
           assert_equal []
             (Listops.drop
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                1)
             ~printer:print_string_list );
         ( "inhabited skip" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
             (Listops.drop
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                3)
             ~printer:print_string_list );
       ]

let split_tests =
  "split"
  >::: [
         ( "negative n" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.split [ "a"; "b"; "c" ] (-1)) );
         ( "empty" >:: fun _ ->
           assert_equal ([], []) (Listops.split [] 5)
             ~printer:print_string_list_pair );
         ( "take none" >:: fun _ ->
           assert_equal
             ([], [ "a"; "b"; "c" ])
             (Listops.split [ "a"; "b"; "c" ] 0)
             ~printer:print_string_list_pair );
         ( "take too much" >:: fun _ ->
           assert_equal
             ([ "a"; "b"; "c" ], [])
             (Listops.split [ "a"; "b"; "c" ] 5)
             ~printer:print_string_list_pair );
         ( "take in between" >:: fun _ ->
           assert_equal
             ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
             (Listops.split
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                3)
             ~printer:print_string_list_pair );
       ]

let slice_tests =
  "slice"
  >::: [
         ( "negative start" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.slice [ "a"; "b"; "c" ] (-1) 2) );
         ( "end less than start" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.slice [ "a"; "b"; "c" ] 2 0) );
         ( "end too big" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.slice [ "a"; "b"; "c" ] 0 3) );
         ( "everything" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.slice [ "a"; "b"; "c" ] 0 2)
             ~printer:print_string_list );
         ( "middle slice" >:: fun _ ->
           assert_equal
             [ "c"; "d"; "e"; "f"; "g" ]
             (Listops.slice
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                2 6)
             ~printer:print_string_list );
       ]

let rotate_tests =
  "rotate"
  >::: [
         ( "empty no rotate" >:: fun _ ->
           assert_equal [] (Listops.rotate [] 0) ~printer:print_string_list );
         ( "empty rotate" >:: fun _ ->
           assert_equal [] (Listops.rotate [] 5) ~printer:print_string_list );
         ( "singleton no rotate" >:: fun _ ->
           assert_equal [ "a" ] (Listops.rotate [ "a" ] 0)
             ~printer:print_string_list );
         ( "singleton rotate" >:: fun _ ->
           assert_equal [ "a" ] (Listops.rotate [ "a" ] 5)
             ~printer:print_string_list );
         ( "big no rotate" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 0)
             ~printer:print_string_list );
         ( "big cycle" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 8)
             ~printer:print_string_list );
         ( "big double cycle" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 16)
             ~printer:print_string_list );
         ( "big reverse cycle" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-8))
             ~printer:print_string_list );
         ( "big partial" >:: fun _ ->
           assert_equal
             [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
             ~printer:print_string_list );
         ( "big reverse partial" >:: fun _ ->
           assert_equal
             [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-5))
             ~printer:print_string_list );
       ]

let remove_at_tests =
  "remove at"
  >::: [
         ( "empty throws" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.remove_at 0 []) );
         ( "negative index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.remove_at (-1) [ "a"; "b"; "c"; "d" ]) );
         ( "end index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.remove_at 4 [ "a"; "b"; "c"; "d" ]) );
         ( "beyond end index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.remove_at 6 [ "a"; "b"; "c"; "d" ]) );
         ( "start index" >:: fun _ ->
           assert_equal [ "b"; "c"; "d" ]
             (Listops.remove_at 0 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
         ( "middle index" >:: fun _ ->
           assert_equal [ "a"; "c"; "d" ]
             (Listops.remove_at 1 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
         ( "last index" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.remove_at 3 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
       ]

let insert_at_tests =
  "insert at"
  >::: [
         ( "negative index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.insert_at "c" (-1) [ "a"; "b" ]) );
         ( "insert at front" >:: fun _ ->
           assert_equal
             [ "alfa"; "a"; "b"; "c"; "d" ]
             (Listops.insert_at "alfa" 0 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
         ( "insert in middle" >:: fun _ ->
           assert_equal
             [ "a"; "alfa"; "b"; "c"; "d" ]
             (Listops.insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
         ( "insert at back" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "alfa" ]
             (Listops.insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
         ( "insert beyond back" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "alfa" ]
             (Listops.insert_at "alfa" 6 [ "a"; "b"; "c"; "d" ])
             ~printer:print_string_list );
       ]

let range_tests =
  "range"
  >::: [
         ( "singleton" >:: fun _ ->
           assert_equal [ 4 ] (Listops.range 4 4) ~printer:print_int_list );
         ( "ascending" >:: fun _ ->
           assert_equal [ 4; 5; 6; 7; 8; 9 ] (Listops.range 4 9)
             ~printer:print_int_list );
         ( "descending" >:: fun _ ->
           assert_equal [ 9; 8; 7; 6; 5; 4 ] (Listops.range 9 4)
             ~printer:print_int_list );
       ]

let rand_select_tests =
  "rand select"
  >::: [
         ( "negative range size" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.rand_select [ "a"; "b"; "c" ] (-1)) );
         ( "range too big" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.rand_select [ "a"; "b"; "c" ] 4) );
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.rand_select [] 0) ~printer:print_string_list
         );
         ( "empty range" >:: fun _ ->
           assert_equal []
             (Listops.rand_select [ "a"; "b"; "c" ] 0)
             ~printer:print_string_list );
         ( "i want it all (may fail correct implementation)" >:: fun _ ->
           assert_equal [ "c"; "b"; "a" ]
             (Listops.rand_select [ "a"; "b"; "c" ] 3)
             ~printer:print_string_list );
         ( "typical (may fail correct implementation)" >:: fun _ ->
           assert_equal [ "e"; "b"; "f" ]
             (Listops.rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
             ~printer:print_string_list );
       ]

let lotto_select_tests =
  "lotto select"
  >::: [
         ( "bad range" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.lotto_select 5 (-1)) );
         ( "negative tickets" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.lotto_select (-1) 5) );
         ( "too many tickets" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.lotto_select 6 5) );
         ( "one lucky winner (may fail correct implementation)" >:: fun _ ->
           assert_equal [ 88 ]
             (Listops.lotto_select 1 100)
             ~printer:print_int_list );
         ( "decimation (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [ 84; 30; 79; 12; 20; 75; 70; 49; 32; 33 ]
             (Listops.lotto_select 10 100)
             ~printer:print_int_list );
         ( "everybody gets a car (may fail correct implementation)" >:: fun _ ->
           assert_equal [ 5; 4; 3; 2; 1 ] (Listops.lotto_select 5 5)
             ~printer:print_int_list );
       ]

let permutation_tests =
  "permutation"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.permutation []) ~printer:print_string_list
         );
         ( "singleton" >:: fun _ ->
           assert_equal [ "a" ]
             (Listops.permutation [ "a" ])
             ~printer:print_string_list );
         ( "many (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [ "f"; "i"; "e"; "d"; "c"; "g"; "h"; "a"; "b"; "j" ]
             (Listops.permutation
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
             ~printer:print_string_list );
       ]

let extract_tests =
  "extract"
  >::: [
         ( "choose negative" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.extract (-1) [ "a"; "b"; "c" ]) );
         ( "zero choose zero" >:: fun _ ->
           assert_equal [ [] ] (Listops.extract 0 [])
             ~printer:print_string_list_list );
         ( "zero choose three" >:: fun _ ->
           assert_equal [] (Listops.extract 3 [])
             ~printer:print_string_list_list );
         ( "six choose zero" >:: fun _ ->
           assert_equal [ [] ]
             (Listops.extract 0 [ "a"; "b"; "c"; "d"; "e"; "f" ])
             ~printer:print_string_list_list );
         ( "six choose three (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [
               [ "c"; "b"; "a" ];
               [ "d"; "b"; "a" ];
               [ "e"; "b"; "a" ];
               [ "f"; "b"; "a" ];
               [ "d"; "c"; "a" ];
               [ "e"; "c"; "a" ];
               [ "f"; "c"; "a" ];
               [ "e"; "d"; "a" ];
               [ "f"; "d"; "a" ];
               [ "f"; "e"; "a" ];
               [ "d"; "c"; "b" ];
               [ "e"; "c"; "b" ];
               [ "f"; "c"; "b" ];
               [ "e"; "d"; "b" ];
               [ "f"; "d"; "b" ];
               [ "f"; "e"; "b" ];
               [ "e"; "d"; "c" ];
               [ "f"; "d"; "c" ];
               [ "f"; "e"; "c" ];
               [ "f"; "e"; "d" ];
             ]
             (Listops.extract 3 [ "a"; "b"; "c"; "d"; "e"; "f" ])
             ~printer:print_string_list_list );
         ( "six choose six (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [ [ "f"; "e"; "d"; "c"; "b"; "a" ] ]
             (Listops.extract 6 [ "a"; "b"; "c"; "d"; "e"; "f" ])
             ~printer:print_string_list_list );
         ( "six choose nine" >:: fun _ ->
           assert_equal []
             (Listops.extract 9 [ "a"; "b"; "c"; "d"; "e"; "f" ])
             ~printer:print_string_list_list );
       ]

let group_tests =
  "group"
  >::: [
         ( "negative index" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.group [ "a"; "b"; "c"; "d" ] [ -1 ]) );
         ( "negative index later" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.group [ "a"; "b"; "c"; "d" ] [ 2; 1; -1 ]) );
         ( "empty null" >:: fun _ ->
           assert_equal [ [] ] (Listops.group [] [])
             ~printer:print_string_list_list_list );
         ( "empty zero" >:: fun _ ->
           assert_equal [ [ [] ] ] (Listops.group [] [ 0 ])
             ~printer:print_string_list_list_list );
         ( "empty multi-zero" >:: fun _ ->
           assert_equal
             [ [ []; []; [] ] ]
             (Listops.group [] [ 0; 0; 0 ])
             ~printer:print_string_list_list_list );
         ( "empty too much" >:: fun _ ->
           assert_equal [] (Listops.group [] [ 5 ])
             ~printer:print_string_list_list_list );
         ( "empty multi-too much" >:: fun _ ->
           assert_equal []
             (Listops.group [] [ 2; 1; 2 ])
             ~printer:print_string_list_list_list );
         ( "inhabited null" >:: fun _ ->
           assert_equal [ [] ]
             (Listops.group [ "a"; "b"; "c"; "d" ] [])
             ~printer:print_string_list_list_list );
         ( "inhabited zero" >:: fun _ ->
           assert_equal [ [ [] ] ]
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 0 ])
             ~printer:print_string_list_list_list );
         ( "inhabited multi-zero" >:: fun _ ->
           assert_equal
             [ [ []; []; [] ] ]
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 0; 0; 0 ])
             ~printer:print_string_list_list_list );
         ( "inhabited too much" >:: fun _ ->
           assert_equal []
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 5 ])
             ~printer:print_string_list_list_list );
         ( "inhabited multi-too much" >:: fun _ ->
           assert_equal []
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 2; 1; 2 ])
             ~printer:print_string_list_list_list );
         ( "inhabited singleton" >:: fun _ ->
           assert_equal
             [ [ [ "a" ] ]; [ [ "b" ] ]; [ [ "c" ] ]; [ [ "d" ] ] ]
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 1 ])
             ~printer:print_string_list_list_list );
         ( "inhabited many at once (may fail correct implementation)"
         >:: fun _ ->
           assert_equal
             [
               [ [ "a"; "b" ] ];
               [ [ "b"; "c" ] ];
               [ [ "a"; "c" ] ];
               [ [ "c"; "d" ] ];
               [ [ "b"; "d" ] ];
               [ [ "a"; "d" ] ];
             ]
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 2 ])
             ~printer:print_string_list_list_list );
         ( "inhabited canonical (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [
               [ [ "a"; "b" ]; [ "c" ] ];
               [ [ "a"; "b" ]; [ "d" ] ];
               [ [ "b"; "c" ]; [ "a" ] ];
               [ [ "b"; "c" ]; [ "d" ] ];
               [ [ "a"; "c" ]; [ "b" ] ];
               [ [ "a"; "c" ]; [ "d" ] ];
               [ [ "c"; "d" ]; [ "a" ] ];
               [ [ "c"; "d" ]; [ "b" ] ];
               [ [ "b"; "d" ]; [ "a" ] ];
               [ [ "b"; "d" ]; [ "c" ] ];
               [ [ "a"; "d" ]; [ "b" ] ];
               [ [ "a"; "d" ]; [ "c" ] ];
             ]
             (Listops.group [ "a"; "b"; "c"; "d" ] [ 2; 1 ])
             ~printer:print_string_list_list_list );
         ( "partition (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [
               [ [ "a" ]; [ "e"; "d" ]; []; [ "c"; "b" ] ];
               [ [ "a" ]; [ "d"; "c" ]; []; [ "e"; "b" ] ];
               [ [ "a" ]; [ "e"; "c" ]; []; [ "d"; "b" ] ];
               [ [ "a" ]; [ "c"; "b" ]; []; [ "e"; "d" ] ];
               [ [ "a" ]; [ "d"; "b" ]; []; [ "e"; "c" ] ];
               [ [ "a" ]; [ "e"; "b" ]; []; [ "d"; "c" ] ];
               [ [ "b" ]; [ "e"; "d" ]; []; [ "c"; "a" ] ];
               [ [ "b" ]; [ "d"; "c" ]; []; [ "e"; "a" ] ];
               [ [ "b" ]; [ "e"; "c" ]; []; [ "d"; "a" ] ];
               [ [ "b" ]; [ "c"; "a" ]; []; [ "e"; "d" ] ];
               [ [ "b" ]; [ "d"; "a" ]; []; [ "e"; "c" ] ];
               [ [ "b" ]; [ "e"; "a" ]; []; [ "d"; "c" ] ];
               [ [ "c" ]; [ "e"; "d" ]; []; [ "b"; "a" ] ];
               [ [ "c" ]; [ "d"; "b" ]; []; [ "e"; "a" ] ];
               [ [ "c" ]; [ "e"; "b" ]; []; [ "d"; "a" ] ];
               [ [ "c" ]; [ "b"; "a" ]; []; [ "e"; "d" ] ];
               [ [ "c" ]; [ "d"; "a" ]; []; [ "e"; "b" ] ];
               [ [ "c" ]; [ "e"; "a" ]; []; [ "d"; "b" ] ];
               [ [ "d" ]; [ "e"; "c" ]; []; [ "b"; "a" ] ];
               [ [ "d" ]; [ "c"; "b" ]; []; [ "e"; "a" ] ];
               [ [ "d" ]; [ "e"; "b" ]; []; [ "c"; "a" ] ];
               [ [ "d" ]; [ "b"; "a" ]; []; [ "e"; "c" ] ];
               [ [ "d" ]; [ "c"; "a" ]; []; [ "e"; "b" ] ];
               [ [ "d" ]; [ "e"; "a" ]; []; [ "c"; "b" ] ];
               [ [ "e" ]; [ "d"; "c" ]; []; [ "b"; "a" ] ];
               [ [ "e" ]; [ "c"; "b" ]; []; [ "d"; "a" ] ];
               [ [ "e" ]; [ "d"; "b" ]; []; [ "c"; "a" ] ];
               [ [ "e" ]; [ "b"; "a" ]; []; [ "d"; "c" ] ];
               [ [ "e" ]; [ "c"; "a" ]; []; [ "d"; "b" ] ];
               [ [ "e" ]; [ "d"; "a" ]; []; [ "c"; "b" ] ];
             ]
             (Listops.group [ "a"; "b"; "c"; "d"; "e" ] [ 1; 2; 0; 2 ])
             ~printer:print_string_list_list_list );
       ]

let length_sort_tests =
  "length sort"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Listops.length_sort [])
             ~printer:print_string_list_list );
         ( "empties" >:: fun _ ->
           assert_equal [ []; []; [] ]
             (Listops.length_sort [ []; []; [] ])
             ~printer:print_string_list_list );
         ( "canonical (assumes sorting in-place)" >:: fun _ ->
           assert_equal
             [
               [ "o" ];
               [ "d"; "e" ];
               [ "d"; "e" ];
               [ "m"; "n" ];
               [ "a"; "b"; "c" ];
               [ "f"; "g"; "h" ];
               [ "i"; "j"; "k"; "l" ];
             ]
             (Listops.length_sort
                [
                  [ "a"; "b"; "c" ];
                  [ "d"; "e" ];
                  [ "f"; "g"; "h" ];
                  [ "d"; "e" ];
                  [ "i"; "j"; "k"; "l" ];
                  [ "m"; "n" ];
                  [ "o" ];
                ])
             ~printer:print_string_list_list );
         ( "canonical with empties (assumes sorting in-place)" >:: fun _ ->
           assert_equal
             [
               [];
               [];
               [];
               [ "o" ];
               [ "d"; "e" ];
               [ "d"; "e" ];
               [ "m"; "n" ];
               [ "a"; "b"; "c" ];
               [ "f"; "g"; "h" ];
               [ "i"; "j"; "k"; "l" ];
             ]
             (Listops.length_sort
                [
                  [ "a"; "b"; "c" ];
                  [ "d"; "e" ];
                  [];
                  [ "f"; "g"; "h" ];
                  [ "d"; "e" ];
                  [ "i"; "j"; "k"; "l" ];
                  [];
                  [ "m"; "n" ];
                  [ "o" ];
                  [];
                ])
             ~printer:print_string_list_list );
       ]

let frequency_sort_tests =
  "frequency sort"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal []
             (Listops.frequency_sort [])
             ~printer:print_string_list_list );
         ( "empties" >:: fun _ ->
           assert_equal [ []; []; [] ]
             (Listops.frequency_sort [ []; []; [] ])
             ~printer:print_string_list_list );
         ( "canonical (may fail correct implementation)" >:: fun _ ->
           assert_equal
             [
               [ "o" ];
               [ "i"; "j"; "k"; "l" ];
               [ "a"; "b"; "c" ];
               [ "f"; "g"; "h" ];
               [ "d"; "e" ];
               [ "d"; "e" ];
               [ "m"; "n" ];
             ]
             (Listops.frequency_sort
                [
                  [ "a"; "b"; "c" ];
                  [ "d"; "e" ];
                  [ "f"; "g"; "h" ];
                  [ "d"; "e" ];
                  [ "i"; "j"; "k"; "l" ];
                  [ "m"; "n" ];
                  [ "o" ];
                ])
             ~printer:print_string_list_list );
         ( "canonical with empties (may fail correct implementation)"
         >:: fun _ ->
           assert_equal
             [
               [ "o" ];
               [ "i"; "j"; "k"; "l" ];
               [ "a"; "b"; "c" ];
               [ "f"; "g"; "h" ];
               [ "d"; "e" ];
               [ "d"; "e" ];
               [ "m"; "n" ];
               [];
               [];
               [];
               [];
             ]
             (Listops.frequency_sort
                [
                  [];
                  [ "a"; "b"; "c" ];
                  [ "d"; "e" ];
                  [];
                  [ "f"; "g"; "h" ];
                  [ "d"; "e" ];
                  [ "i"; "j"; "k"; "l" ];
                  [];
                  [ "m"; "n" ];
                  [ "o" ];
                  [];
                ])
             ~printer:print_string_list_list );
       ]

let () = run_test_tt_main last_tests
let () = run_test_tt_main last_two_tests
let () = run_test_tt_main nth_tests
let () = run_test_tt_main length_tests
let () = run_test_tt_main rev_tests
let () = run_test_tt_main palindrome_tests
let () = run_test_tt_main flatten_tests
let () = run_test_tt_main compress_tests
let () = run_test_tt_main pack_tests
let () = run_test_tt_main encode_tests
let () = run_test_tt_main modified_encode_tests
let () = run_test_tt_main decode_tests
let () = run_test_tt_main duplicate_tests
let () = run_test_tt_main replicate_tests
let () = run_test_tt_main drop_tests
let () = run_test_tt_main split_tests
let () = run_test_tt_main slice_tests
let () = run_test_tt_main rotate_tests
let () = run_test_tt_main remove_at_tests
let () = run_test_tt_main insert_at_tests
let () = run_test_tt_main range_tests

(* YMMV with these tests owing to implementation differences and weak spec *)
let () = run_test_tt_main rand_select_tests
let () = run_test_tt_main lotto_select_tests
let () = run_test_tt_main permutation_tests
let () = run_test_tt_main extract_tests
let () = run_test_tt_main group_tests
let () = run_test_tt_main length_sort_tests
let () = run_test_tt_main frequency_sort_tests
