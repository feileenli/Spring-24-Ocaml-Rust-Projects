open OUnit2
open Project1.Basics

let test_abs _ = 
  assert_equal 1 (abs 1) ~msg:"test_abs (1)";
  assert_equal 2 (abs 2) ~msg:"test_abs (2)";
  assert_equal 1 (abs (-1)) ~msg:"test_abs (3)";
  assert_equal 2 (abs (-2)) ~msg:"test_abs (4)";
  assert_equal 0 (abs (0)) ~msg:"test_abs (5)"

let test_rev_tup _ =
  assert_equal (1, 2, 3)   (rev_tup (3, 2, 1))   ~msg:"rev_tup (1)";
  assert_equal (3, 2, 1)   (rev_tup (1, 2, 3))   ~msg:"rev_tup (2)";
  assert_equal (3, 1, 1)   (rev_tup (1, 1, 3))   ~msg:"rev_tup (3)";
  assert_equal (1, 1, 1)   (rev_tup (1, 1, 1))   ~msg:"rev_tup (4)";
  assert_equal ("l",0,"i") (rev_tup ("i",0,"l")) ~msg:"rev_tup (5)"

let test_is_even _ =
  assert_equal false (is_even 1) ~msg:"is_even (1)";
  assert_equal true (is_even 4) ~msg:"is_even (2)";
  assert_equal false (is_even (-5)) ~msg:"is_even (3)";
  assert_equal true (is_even (0)) ~msg:"is_even (4)"

let test_area _ =
  assert_equal 1 (area (1, 1) (2, 2))   ~msg:"area (1)";
  assert_equal 2 (area (1, 1) (2, 3))   ~msg:"area (2)";
  assert_equal 2 (area (1, 1) (3, 2))   ~msg:"area (3)";
  assert_equal 4 (area (1, 1) (3, 3))   ~msg:"area (4)";
  assert_equal 9 (area ((-1), (-2)) (2, 1)) ~msg:"area (5)";
  assert_equal 0 (area (1,1) (1,1)) ~msg:"area (6)";
  assert_equal 49 (area (7,7) (0,0)) ~msg:"area (7)";
  assert_equal 36 (area (1,(4)) (5,(-5))) ~msg:"area (8)"

let test_zip _ = 
  assert_equal [(1, 2, 7, 8); (3, 4, 9, 10); (5, 6, 11, 12)] (zip [(1, 2); (3, 4); (5, 6)] [(7, 8); (9, 10); (11, 12)]) ~msg:"zip (1)";
  assert_equal [] (zip [] []) ~msg: "zip (2)";
  assert_equal [] (zip [(1, 4)] []) ~msg: "zip (3)";
  assert_equal [(1, 2, 7, 8)] (zip [(1, 2); (3, 4)] [(7, 8)]) ~msg: "zip (4)";
  assert_equal [("4",5,1,"G")] (zip [("4",5)] [(1,"G")]) ~msg: "zip (5)"


let test_fibonacci _ = 
  assert_equal 1   (fibonacci 1)    ~msg:"fibonacci (1)";
  assert_equal 1   (fibonacci 2)    ~msg:"fibonacci (2)";
  assert_equal 8   (fibonacci 6)    ~msg:"fibonacci (3)";
  assert_equal 144 (fibonacci 12)   ~msg:"fibonacci (4)";
  assert_equal 34  (fibonacci 9)    ~msg:"fibonacci (5)"

let test_pow _ =
  assert_equal 2 (pow 2 1)        ~msg:"pow (1)";
  assert_equal 4 (pow 2 2)        ~msg:"pow (2)";
  assert_equal 3 (pow 3 1)        ~msg:"pow (3)";
  assert_equal 27 (pow 3 3)       ~msg:"pow (4)";
  assert_equal 625 (pow 5 4)      ~msg:"pow (5)";
  assert_equal (-27) (pow (-3) 3) ~msg:"pow (6)";
  assert_equal 1 (pow 7 0)      ~msg: "pow (7)";
  assert_equal 1 (pow (-7) 0)      ~msg: "pow (8)";
  assert_equal (81) (pow (-3) 4) ~msg: "pow (9)"

let test_log _ =
  assert_equal 1 (log 4 4)   ~msg:"log (1)";
  assert_equal 2 (log 4 16)  ~msg:"log (2)";
  assert_equal 1 (log 4 15)  ~msg:"log (3)";
  assert_equal 3 (log 4 64)  ~msg:"log (4)";
  assert_equal 3 (log 5 125) ~msg:"log (5)";
  assert_equal 4 (log 3 81)  ~msg:"log (6)"

let test_gcf _ =
  assert_equal 0 (gcf 0 0) ~msg:"gcf (1)";
  assert_equal 3 (gcf 3 0) ~msg:"gcf (2)";
  assert_equal 4 (gcf 12 8) ~msg:"gcf (3)";
  assert_equal 6 (gcf 24 6) ~msg:"gcf (4)";
  assert_equal 1 (gcf 27 10) ~msg:"gcf (5)";
  assert_equal 13 (gcf 13 13) ~msg:"gcf (6)";
  assert_equal 32 (gcf 128 96) ~msg:"gcf (7)"

let test_reverse _ =
  assert_equal [1] (reverse [1]) ~msg:"reverse (1)";
  assert_equal [3; 2; 1] (reverse [1; 2; 3]) ~msg:"reverse (2)";
  assert_equal [] (reverse []) ~msg:"reverse (3)";
  assert_equal ["c"; "b"; "a"] (reverse ["a"; "b"; "c"]) ~msg:"reverse (4)";
  assert_equal [1; (-1)] (reverse [(-1);1]) ~msg:"reverse (5)";
  assert_equal [(-1);(-1)] (reverse [(-1);(-1)]) ~msg:"reverse (6)"

let test_merge _ =
  assert_equal [1; 2] (merge [1] [2]) ~msg:"merge (1)";
  assert_equal [] (merge [] []) ~msg:"merge (2)";
  assert_equal [1; 2; 3; 4] (merge [1; 4] [2; 3]) ~msg:"merge (3)";
  assert_equal [0; 1] (merge [1] [0]) ~msg:"merge (4)";
  assert_equal [1; 2; 3; 4; 5; 6; 7; 8; 9] (merge [1; 4; 5] [2; 3; 6; 7; 8; 9]) ~msg:"merge (5)";
  assert_equal [-1;1] (merge [-1] [1]) ~msg:"merge (6)";
  assert_equal [-7;-7;-4;-1;1;4;7;7] (merge [-7;-1;4;7] [-7;-4;1;7]) ~msg:"merge (7)";
  assert_equal [-5;-4;-1;-1;1;1;4;5] (merge [-5;1;1;4] [-4;-1;-1;5]) ~msg:"merge (8)" 

let test_is_present _ =
  let a = [] in
  let b = ["w";"x";"y";"z"] in
  let c = [14;20;42;1;81] in
  let d = ["mittens";"meowww";"chicken";"butt";"fart";"club"] in

  assert_equal false (is_present a 3) ~msg:"is_present (1)";
  assert_equal true (is_present b "w") ~msg:"is_present (2)";
  assert_equal false (is_present b "") ~msg:"is_present (3)";
  assert_equal false (is_present b "wd") ~msg:"is_present (4)";
  assert_equal true (is_present c 20) ~msg:"is_present (5)";
  assert_equal false (is_present c (-20)) ~msg:"is_present (6)";
  assert_equal true (is_present d "mittens") ~msg:"is_present (7)";
  assert_equal false (is_present d "fart club") ~msg:"is_present (8)";
  assert_equal false (is_present d "") ~msg:"is_present (9)"

let test_every_nth _ = 
  let a = [1;2;3;4;5;6;7] in
  let b = ["w";"x";"y";"z"] in 

  assert_equal [2;4;6] (every_nth 2 a) ~msg:"every_nth (1)";
  assert_equal [1;2;3;4;5;6;7] (every_nth 1 a) ~msg:"every_nth (2)";
  assert_equal ["w";"x";"y";"z"] (every_nth 1 b) ~msg:"every_nth (3)";
  assert_equal ["x";"z"] (every_nth 2 b) ~msg:"every_nth (4)"


let test_jumping_tuples _ = 
  assert_equal [8; 3; 12; 1; 10; 5] (jumping_tuples [(1, 2); (3, 4); (5, 6)] [(7, 8); (9, 10); (11, 12)]) ~msg:"jumping tuples (1)";
  assert_equal [false; false; true; true] (jumping_tuples [(true,"a"); (false,"b")] [(100, false); (428, true)]) ~msg:"jumping tuples (2)";
  assert_equal ["sixth"; "third"; "first"; "eighth"] (jumping_tuples [("first", "second"); ("third", "fourth")] [("fifth", "sixth"); ("seventh", "eighth")]) ~msg:"jumping tuples (3)";
  assert_equal [] (jumping_tuples [] []) ~msg:"jumping tuples (4)"

let test_max_func_chain _ = 
  assert_equal 8 (max_func_chain 2 [(fun x -> x + 6)]) ~msg:"max_func_chain (1)";
  assert_equal 24 (max_func_chain 2 [(fun x -> x + 4); (fun x -> x * 4)]) ~msg:"max_func_chain (2)";
  assert_equal (-1) (max_func_chain (-4) [(fun x -> x * 4); (fun x -> x + 3)]) ~msg:"max_func_chain (3)";
  assert_equal 14 (max_func_chain 4 [(fun x -> x - 2); (fun x -> x + 10)]) ~msg:"max_func_chain (4)";
  assert_equal 501 (max_func_chain 0 [(fun x -> x - 1); (fun x -> x * -500); (fun x -> x + 1)]) ~msg:"max_func_chain (5)"



let test_is_there _ =
  let a = [] in
  let b = ["w";"x";"y";"z"] in
  let c = [14;20;42;1;81] in
  let d = ["mittens";"meowww";"chicken";"butt";"fart";"club"] in

  assert_equal false (is_there a 3) ~msg:"is_present (1)";
  assert_equal true (is_there b "w") ~msg:"is_present (2)";
  assert_equal false (is_there b "") ~msg:"is_present (3)";
  assert_equal false (is_there b "wd") ~msg:"is_present (4)";
  assert_equal true (is_there c 20) ~msg:"is_present (5)";
  assert_equal false (is_there c (-20)) ~msg:"is_present (6)";
  assert_equal true (is_there d "mittens") ~msg:"is_present (7)";
  assert_equal false (is_there d "fart club") ~msg:"is_present (8)";
  assert_equal false (is_there d "") ~msg:"is_present (9)"

let test_count_occ _ =
  let y = ["a";"a";"b";"a"] in
  let z = [1;7;7;1;5;2;7;7] in
  let a = [true;false;false;true;false;false;false] in
  let b = [] in

  assert_equal 3 (count_occ y "a") ~msg:"count_occ (1)";
  assert_equal 1 (count_occ y "b") ~msg:"count_occ (2)";
  assert_equal 0 (count_occ y "") ~msg:"count_occ (3)";
  assert_equal 2 (count_occ z 1) ~msg:"count_occ (4)";
  assert_equal 4 (count_occ z 7) ~msg:"count_occ (5)";
  assert_equal 1 (count_occ z 5) ~msg:"count_occ (6)";
  assert_equal 1 (count_occ z 2) ~msg:"count_occ (7)";
  assert_equal 2 (count_occ a true) ~msg:"count_occ (8)";
  assert_equal 5 (count_occ a false) ~msg:"count_occ (9)";
  assert_equal 0 (count_occ b "a") ~msg:"count_occ (10)";
  assert_equal 0 (count_occ b 2) ~msg:"count_occ (11)"

let test_uniq _ =
  let y = ["a";"a";"b";"a"] in
  let z = [1;7;7;1;5;2;7;7] in
  let a = [true;false;false;true;false;false;false] in
  let b = [] in

  assert_equal ["a";"b"] @@ List.sort compare (uniq y);
  assert_equal [1;2;5;7] @@ List.sort compare (uniq z);
  assert_equal [false;true] @@ List.sort compare (uniq a);
  assert_equal [] @@ uniq b

let test_every_xth _ = 
  let a = [1;2;3;4;5;6;7] in
  let b = ["w";"x";"y";"z"] in 

  assert_equal [2;4;6] (every_xth 2 a) ~msg:"every_xth (1)";
  assert_equal [1;2;3;4;5;6;7] (every_xth 1 a) ~msg:"every_xth (2)";
  assert_equal ["w";"x";"y";"z"] (every_xth 1 b) ~msg:"every_xth (3)";
  assert_equal ["x";"z"] (every_xth 2 b) ~msg:"every_xth (4)"

let suite =
  "public" >::: [
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