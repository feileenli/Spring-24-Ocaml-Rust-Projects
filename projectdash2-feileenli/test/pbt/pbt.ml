open OUnit2
open QCheck
open P2.Ntree

let leaf _ = Leaf
let node l v r = BiNode (l, v, r)

let tree_gen =
  Gen.(
    sized
    @@ fix (fun self n ->
           match n with
           | 0 -> map leaf nat
           | n ->
               frequency
                 [
                   (1, map leaf nat);
                   (2, map3 node (self (n / 2)) Gen.nat (self (n / 2)));
                 ]))

let test_flatten_unflatten =
  Test.make ~name:"test_flatten_unflatten"
    ~count:1000 (* number of tests. change this number to generate more tests *)
    (make tree_gen) (fun x -> unflatten (flatten x) = x)

let suite = "pbt" >::: [ QCheck_runner.to_ounit2_test test_flatten_unflatten ]
let _ = run_test_tt_main suite
