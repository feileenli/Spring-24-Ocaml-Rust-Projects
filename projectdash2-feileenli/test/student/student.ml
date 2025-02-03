open OUnit2
open List
open P2.Db
open P2.Ntree

let student_test1 _ = 
  assert_equal 42 42 ~msg:"student_test1 (1)"
;;

let suite = "student" >::: [ 
  "student_test1" >:: student_test1 
]

let _ = run_test_tt_main suite
