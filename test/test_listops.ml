(*
 * Test suite for lib/listops.ml
 *)

open OUnit2
open Problems

let last_tests =
  "last"
  >::: [
         ("empty" >:: fun _ -> assert_equal None (Listops.last []));
         ( "inhabited" >:: fun _ ->
           assert_equal (Some "d") (Listops.last [ "a"; "b"; "c"; "d" ]) );
       ]

let last_two_tests =
  "last_two"
  >::: [
         ("empty" >:: fun _ -> assert_equal None (Listops.last_two []));
         ("singleton" >:: fun _ -> assert_equal None (Listops.last_two [ "a" ]));
         ( "big enough" >:: fun _ ->
           assert_equal
             (Some ("c", "d"))
             (Listops.last_two [ "a"; "b"; "c"; "d" ]) );
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
           assert_equal "a" (Listops.nth [ "a"; "b"; "c"; "d"; "e" ] 0) );
         ( "middle" >:: fun _ ->
           assert_equal "c" (Listops.nth [ "a"; "b"; "c"; "d"; "e" ] 2) );
         ( "back" >:: fun _ ->
           assert_equal "e" (Listops.nth [ "a"; "b"; "c"; "d"; "e" ] 4) );
       ]

let length_tests =
  "length"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (Listops.length []));
         ( "inhabited" >:: fun _ ->
           assert_equal 4 (Listops.length [ "a"; "b"; "c"; "d" ]) );
       ]

let rev_tests =
  "rev"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.rev []));
         ( "inhabited" >:: fun _ ->
           assert_equal [ "d"; "c"; "b"; "a" ]
             (Listops.rev [ "a"; "b"; "c"; "d" ]) );
       ]

let palindrome_tests =
  "palindrome"
  >::: [
         ("empty" >:: fun _ -> assert_equal true (Listops.palindrome []));
         ( "singleton" >:: fun _ ->
           assert_equal true (Listops.palindrome [ "a" ]) );
         ( "odd palindrome" >:: fun _ ->
           assert_equal true (Listops.palindrome [ "a"; "b"; "a" ]) );
         ( "even palindrome" >:: fun _ ->
           assert_equal true (Listops.palindrome [ "a"; "b"; "b"; "a" ]) );
         ( "odd non-palindrome" >:: fun _ ->
           assert_equal false (Listops.palindrome [ "a"; "b"; "b" ]) );
         ( "even non-palindrome" >:: fun _ ->
           assert_equal false (Listops.palindrome [ "a"; "b"; "a"; "b" ]) );
       ]

let flatten_tests =
  "flatten"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.flatten []));
         ( "all ones" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.flatten [ One "a"; One "b"; One "c" ]) );
         ( "all singletons" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.flatten
                [ Many [ One "a" ]; Many [ One "b" ]; Many [ One "c" ] ]) );
         ( "very nested" >:: fun _ ->
           assert_equal [ "a" ]
             (Listops.flatten
                [ Many [ Many [ Many [ Many [ Many [ One "a" ] ] ] ] ] ]) );
         ( "mixed" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e" ]
             (Listops.flatten
                [
                  One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ];
                ]) );
       ]

let compress_tests =
  "compress"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.compress []));
         ( "singleton" >:: fun _ ->
           assert_equal [ "a" ] (Listops.compress [ "a" ]) );
         ( "run" >:: fun _ ->
           assert_equal [ "a" ]
             (Listops.compress [ "a"; "a"; "a"; "a"; "a"; "a" ]) );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "b"; "a"; "c" ]
             (Listops.compress [ "a"; "b"; "c"; "b"; "a"; "c" ]) );
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
                ]) );
       ]

let pack_tests =
  "pack"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.pack []));
         ( "singleton" >:: fun _ ->
           assert_equal [ [ "a" ] ] (Listops.pack [ "a" ]) );
         ( "run" >:: fun _ ->
           assert_equal
             [ [ "a"; "a"; "a"; "a"; "a"; "a" ] ]
             (Listops.pack [ "a"; "a"; "a"; "a"; "a"; "a" ]) );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ [ "a" ]; [ "b" ]; [ "c" ]; [ "b" ]; [ "a" ]; [ "c" ] ]
             (Listops.pack [ "a"; "b"; "c"; "b"; "a"; "c" ]) );
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
                ]) );
       ]

let encode_tests =
  "encode"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.encode []));
         ( "singleton" >:: fun _ ->
           assert_equal [ (1, "a") ] (Listops.encode [ "a" ]) );
         ( "run" >:: fun _ ->
           assert_equal
             [ (6, "a") ]
             (Listops.encode [ "a"; "a"; "a"; "a"; "a"; "a" ]) );
         ( "no runs" >:: fun _ ->
           assert_equal
             [ (1, "a"); (1, "b"); (1, "c"); (1, "b"); (1, "a"); (1, "c") ]
             (Listops.encode [ "a"; "b"; "c"; "b"; "a"; "c" ]) );
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
                ]) );
       ]

let modified_encode_tests =
  "modified encode"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.modified_encode []));
         ( "singleton" >:: fun _ ->
           assert_equal [ Listops.Uno "a" ] (Listops.modified_encode [ "a" ]) );
         ( "run" >:: fun _ ->
           assert_equal
             [ Listops.Muchos (6, "a") ]
             (Listops.modified_encode [ "a"; "a"; "a"; "a"; "a"; "a" ]) );
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
             (Listops.modified_encode [ "a"; "b"; "c"; "b"; "a"; "c" ]) );
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
                ]) );
       ]

let decode_tests =
  "decode"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.decode []));
         ( "singleton" >:: fun _ ->
           assert_equal [ "a" ] (Listops.decode [ Listops.Uno "a" ]) );
         ( "run" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "a"; "a"; "a"; "a" ]
             (Listops.decode [ Listops.Muchos (6, "a") ]) );
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
                ]) );
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
                ]) );
       ]

let duplicate_tests =
  "duplicate"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Listops.duplicate []));
         ( "inhabited" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "b"; "b"; "c"; "c" ]
             (Listops.duplicate [ "a"; "b"; "c" ]) );
       ]

let replicate_tests =
  "replicate"
  >::: [
         ( "negative n" >:: fun _ ->
           assert_raises Listops.replicate_exception (fun _ ->
               Listops.replicate [ "a"; "b" ] (-1)) );
         ("empty zero" >:: fun _ -> assert_equal [] (Listops.replicate [] 0));
         ("empty double" >:: fun _ -> assert_equal [] (Listops.replicate [] 2));
         ( "inhabited zero" >:: fun _ ->
           assert_equal [] (Listops.replicate [ "a"; "b"; "c" ] 0) );
         ( "inhabited id" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.replicate [ "a"; "b"; "c" ] 1) );
         ( "inhabited double" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "b"; "b"; "c"; "c" ]
             (Listops.replicate [ "a"; "b"; "c" ] 2) );
         ( "inhabited triple" >:: fun _ ->
           assert_equal
             [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
             (Listops.replicate [ "a"; "b"; "c" ] 3) );
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
         ("empty delete" >:: fun _ -> assert_equal [] (Listops.drop [] 1));
         ("empty skip" >:: fun _ -> assert_equal [] (Listops.drop [] 3));
         ( "inhabited delete" >:: fun _ ->
           assert_equal []
             (Listops.drop
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                1) );
         ( "inhabited skip" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
             (Listops.drop
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                3) );
       ]

let split_tests =
  "split"
  >::: [
         ( "negative n" >:: fun _ ->
           assert_raises Listops.index_exception (fun _ ->
               Listops.split [ "a"; "b"; "c" ] (-1)) );
         ("empty" >:: fun _ -> assert_equal ([], []) (Listops.split [] 5));
         ( "take none" >:: fun _ ->
           assert_equal
             ([], [ "a"; "b"; "c" ])
             (Listops.split [ "a"; "b"; "c" ] 0) );
         ( "take too much" >:: fun _ ->
           assert_equal
             ([ "a"; "b"; "c" ], [])
             (Listops.split [ "a"; "b"; "c" ] 5) );
         ( "take in between" >:: fun _ ->
           assert_equal
             ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
             (Listops.split
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                3) );
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
           assert_equal [ "a"; "b"; "c" ] (Listops.slice [ "a"; "b"; "c" ] 0 2)
         );
         ( "middle slice" >:: fun _ ->
           assert_equal
             [ "c"; "d"; "e"; "f"; "g" ]
             (Listops.slice
                [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
                2 6) );
       ]

let rotate_tests =
  "rotate"
  >::: [
         ("empty no rotate" >:: fun _ -> assert_equal [] (Listops.rotate [] 0));
         ("empty rotate" >:: fun _ -> assert_equal [] (Listops.rotate [] 5));
         ( "singleton no rotate" >:: fun _ ->
           assert_equal [ "a" ] (Listops.rotate [ "a" ] 0) );
         ( "singleton rotate" >:: fun _ ->
           assert_equal [ "a" ] (Listops.rotate [ "a" ] 5) );
         ( "big no rotate" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 0) );
         ( "big cycle" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 8) );
         ( "big double cycle" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 16) );
         ( "big reverse cycle" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-8)) );
         ( "big partial" >:: fun _ ->
           assert_equal
             [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3) );
         ( "big reverse partial" >:: fun _ ->
           assert_equal
             [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
             (Listops.rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-5)) );
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
             (Listops.remove_at 0 [ "a"; "b"; "c"; "d" ]) );
         ( "middle index" >:: fun _ ->
           assert_equal [ "a"; "c"; "d" ]
             (Listops.remove_at 1 [ "a"; "b"; "c"; "d" ]) );
         ( "last index" >:: fun _ ->
           assert_equal [ "a"; "b"; "c" ]
             (Listops.remove_at 3 [ "a"; "b"; "c"; "d" ]) );
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
             (Listops.insert_at "alfa" 0 [ "a"; "b"; "c"; "d" ]) );
         ( "insert in middle" >:: fun _ ->
           assert_equal
             [ "a"; "alfa"; "b"; "c"; "d" ]
             (Listops.insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ]) );
         ( "insert at back" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "alfa" ]
             (Listops.insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ]) );
         ( "insert beyond back" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "alfa" ]
             (Listops.insert_at "alfa" 6 [ "a"; "b"; "c"; "d" ]) );
       ]

let range_tests =
  "range"
  >::: [
         ("singleton" >:: fun _ -> assert_equal [ 4 ] (Listops.range 4 4));
         ( "ascending" >:: fun _ ->
           assert_equal [ 4; 5; 6; 7; 8; 9 ] (Listops.range 4 9) );
         ( "descending" >:: fun _ ->
           assert_equal [ 9; 8; 7; 6; 5; 4 ] (Listops.range 9 4) );
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
(* Excluding tests for rand_select because results depend on implementation *)