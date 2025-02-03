open OUnit2
open Project1.Basics

let test_abs _ = 
  assert_equal 1 (abs 1) ~msg:"test_abs (1)"

let test_rev_tup _ =
  assert_equal (1, 2, 3) (rev_tup (3, 2, 1))   ~msg:"rev_tup (1)"

let test_is_even _ =
  assert_equal false (is_even 1) ~msg:"is_even (1)"

let test_area _ =
  assert_equal 1 (area (1, 1) (2, 2))   ~msg:"area (1)"

let test_zip _ = 
  assert_equal [(1, 2, 7, 8); (3, 4, 9, 10); (5, 6, 11, 12)] (zip [(1, 2); (3, 4); (5, 6)] [(7, 8); (9, 10); (11, 12)]) ~msg:"zip (1)"

let test_fibonacci _ = 
  assert_equal 1 (fibonacci 1) ~msg:"fibonacci (1)"

let test_pow _ =
  assert_equal 2 (pow 2 1) ~msg:"pow (1)"

let test_log _ =
  assert_equal 1 (log 4 4) ~msg:"log (1)"

let test_gcf _ =
  assert_equal 0 (gcf 0 0) ~msg:"gcf (1)"

let test_reverse _ =
  assert_equal [1] (reverse [1]) ~msg:"reverse (1)"

let test_merge _ =
  assert_equal [1; 2] (merge [1] [2]) ~msg:"merge (1)"

let test_is_present _ =
  assert_equal false (is_present [] "") ~msg:"is_present (1)"

let test_every_nth _ = 
  let a = [1;2;3;4;5;6;7] in
  assert_equal [2;4;6] (every_nth 2 a) ~msg:"every_nth (1)"

let test_jumping_tuples _ = 
  assert_equal [8; 3; 12; 1; 10; 5] (jumping_tuples [(1, 2); (3, 4); (5, 6)] [(7, 8); (9, 10); (11, 12)]) ~msg:"jumping tuples (1)"

let test_max_func_chain _ = 
  assert_equal 8 (max_func_chain 2 [(fun x -> x + 6)]) ~msg:"max_func_chain (1)"

let test_is_there _ =
  let a = [] in
  assert_equal false (is_there a 3) ~msg:"is_present (1)"

let test_count_occ _ =
  let y = ["a";"a";"b";"a"] in
  assert_equal 3 (count_occ y "a") ~msg:"count_occ (1)"

let test_uniq _ =
  let y = ["a";"a";"b";"a"] in
  assert_equal ["a";"b"] @@ List.sort compare (uniq y)

let test_every_xth _ = 
  let a = [1;2;3;4;5;6;7] in
  assert_equal [2;4;6] (every_xth 2 a) ~msg:"every_xth (1)"

let suite =
  "student" >::: [
    "abs" >:: test_abs;
    "rev_tup" >:: test_rev_tup;
    "is_even" >:: test_is_even;
    "area" >:: test_area;
    "zip" >:: test_zip;
    "fibonacci" >:: test_fibonacci; 
    "pow" >:: test_pow; 
    "log" >:: test_log; 
    "gcf" >:: test_gcf; 
    "zip" >:: test_zip;
    "reverse" >:: test_reverse; 
    "merge" >:: test_merge; 
    "is_present" >:: test_is_present; 
    "every_nth" >:: test_every_nth; 
    "jumping_tuples" >:: test_jumping_tuples; 
    "max_func_chain" >:: test_max_func_chain; 
    "is_there" >:: test_is_there; 
    "count_occ" >:: test_count_occ; 
    "uniq" >:: test_uniq; 
    "every_xth" >:: test_every_xth; 
    ]

let _ = run_test_tt_main suite
