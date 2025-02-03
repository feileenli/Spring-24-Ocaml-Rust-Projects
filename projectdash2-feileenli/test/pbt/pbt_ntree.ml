open OUnit2
open QCheck
open P2.Ntree

let rec ntree_gen =
  let leaf _ = Node (0, []) in
  let node v children = Node (v, children) in
  fun depth ->
    Gen.(
      let cnt = Gen.generate1 (Gen.int_range 0 10) in
      match depth with
      | 0 -> map leaf nat
      | _ ->
          frequency
            [
              (1, map leaf nat);
              ( 2,
                map2 node (return cnt)
                  (Gen.list_size (return cnt) (ntree_gen (depth - 1))) );
            ])

let test_encode_decode =
  Test.make ~name:"test_encode+decode"
    ~count:1000 (* number of tests. change this number to generate more tests *)
    (make (ntree_gen (Gen.generate1 (Gen.int_range 0 10))))
    (fun x -> decode (encode x) = x)

let suite = "pbt" >::: [ QCheck_runner.to_ounit2_test test_encode_decode ]
let _ = run_test_tt_main suite
