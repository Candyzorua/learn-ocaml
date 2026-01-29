open OUnit2

let suite =
  "chapter3 tests" >::: [
    "empty list for product" >:: (fun _ -> assert_equal 1 (Cs3110.Chapter3.product []));
    "normal case for product" >:: (fun _ -> assert_equal 6 (Cs3110.Chapter3.product [1;2;3]));

    "negative case for bigred" >:: (fun _ -> assert_equal false (Cs3110.Chapter3.first_bigred []));
    "positive single-element case for bigred" >:: (fun _ -> assert_equal true (Cs3110.Chapter3.first_bigred ["bigred"]));
    "positive multi-element case for bigred" >:: (fun _ -> assert_equal true (Cs3110.Chapter3.first_bigred ["bigred"; "blue"]));
  ]

let () =
  run_test_tt_main suite
