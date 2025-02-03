open QCheck
open Ntree

let gen_tree () =
  let leaf _ = Leaf in
  let node l v r = BiNode (l, v, r) in
  let generate =
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
  in
  Gen.generate1 generate

let gen_ntree d =
  let leaf _ = Node (0, []) in
  let node v children = Node (v, children) in
  let rec ntree depth =
    Gen.(
      let cnt = Gen.generate1 (Gen.int_range 0 5) in
      match depth with
      | 0 -> map leaf nat
      | _ ->
          frequency
            [
              (1, map leaf nat);
              ( 2,
                map2 node (return cnt)
                  (Gen.list_size (return cnt) (ntree (depth - 1))) );
            ])
  in
  Gen.generate1 (ntree d)
