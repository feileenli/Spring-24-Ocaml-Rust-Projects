open OUnit2
open List

open P2.Db
open P2.Ntree

(* PART 1: DATABASE *)

let person1 = { name = "Alice"; age = 23; hobbies = [ "Skiing"; "golfing" ] }

let person2 =
  { name = "Bob"; age = 42; hobbies = [ "Skiing"; "Cooking"; "Legos" ] }

let person3 = { name = "Clyff"; age = 98; hobbies = [ "Legos"; "Gaming" ] }
let person4 = { name = "Duncan"; age = 13; hobbies = [ "Gaming"; "DnD" ] }
let condition_empty = True
let condition_name = Name (fun name -> name = "Clyff")
let condition_age = Age (fun age -> age > 30)
let condition_hobbies = Hobbies (fun hobbies -> List.mem "Gaming" hobbies)
let condition_and = And (Age (fun age -> age > 41), Age (fun age -> age < 43))

let condition_or =
  Or
    ( Hobbies (fun hobbies -> List.mem "DnD" hobbies),
      Name (fun name -> String.length name < 4) )

let condition_not = Not condition_name
let condition_if = If (condition_age, condition_name, condition_not)

let comparator_age = (fun p1 p2 -> 
  if p1.age < p2.age then 
    -1 
  else if p1.age = p2.age then 
    0 
  else 
    1)

let change_name person =
  { name = "Luke"; age = person.age; hobbies = person.hobbies }

let db1 = insert person1 newDatabase

let db2 =
  insert person4 (insert person3 (insert person2 (insert person1 newDatabase)))

let public_insert1 _ =
  let applied = query condition_empty db1 in

  assert_equal 1 (List.length applied) ~msg:"db1 not empty after inserting person1";
  assert_equal true (List.mem person1 applied) ~msg:"db1 contains person1"

let public_remove1 _ =
  let db1 = remove person1.name db1 in
  let applied = query condition_empty db1 in

  assert_equal 0 (List.length applied) ~msg:"db1 empty after removing person1"

let public_remove2 _ =
  let db2 = remove person2.name db2 in
  let db2 = remove person4.name db2 in
  let applied = query condition_empty db2 in

  assert_equal 2 (List.length applied) ~msg:"db2 has 2 people after removing person2 and person4";
  assert_equal true (List.mem person1 applied) ~msg:"db2 contains person1";
  assert_equal false (List.mem person2 applied) ~msg:"db2 doesn't contain person2";
  assert_equal true (List.mem person3 applied) ~msg:"db2 contains person3"
  
let public_query1 _ = 
  let applied = query condition_name db2 in
  
  assert_equal 1 (List.length applied) ~msg:"condition_name applied to db2 has one element";
  assert_equal true (List.mem person3 applied) ~msg:"condition_name applied to db2 contains person3";

  let applied = query condition_age db2 in
  assert_equal 2 (List.length applied) ~msg:"condition_age applied to db2 has 2 elements";
  assert_equal true (List.mem person2 applied) ~msg:"condition_age applied to db2 contains person2";
  assert_equal true (List.mem person3 applied) ~msg:"condition_age applied to db2 contains person3";

  let applied = query condition_hobbies db2 in
  assert_equal 2 (List.length applied) ~msg:"condition_hobbies applied to db2 has 2 elements";
  assert_equal true (List.mem person3 applied) ~msg:"condition_hobbies applied to db2 contains person3";
  assert_equal true (List.mem person4 applied) ~msg:"condition_hobbies applied to db2 contains person4"

let public_query2 _ =
  let applied = query condition_and db2 in

  assert_equal 1 (List.length applied) ~msg:"condition_and applied to db2 has 1 element";
  assert_equal true (List.mem person2 applied) ~msg:"condition_and applied to db2 contains person2";

  let applied = query condition_or db2 in

  assert_equal 2 (List.length applied) ~msg:"condition_or applied to db2 has 2 elements";
  assert_equal true (List.mem person2 applied) ~msg:"condition_or applied to db2 contains person2";
  assert_equal true (List.mem person4 applied) ~msg:"condition_or applied to db2 contains person4";

  let applied = query condition_not db2 in

  assert_equal 3 (List.length applied) ~msg:"condition_not applied to db2 has 3 elements";
  assert_equal true (List.mem person1 applied) ~msg:"condition_not applied to db2 contains person1";
  assert_equal true (List.mem person2 applied) ~msg:"condition_not applied to db2 contains person2";
  assert_equal true (List.mem person4 applied) ~msg:"condition_not applied to db2 contains person4";

  let applied = query condition_if db2 in

  assert_equal 3 (List.length applied) ~msg:"condition_if applied to db2 has 3 elements";
  assert_equal true (List.mem person1 applied) ~msg:"condition_if applied to db2 contains person1";
  assert_equal false (List.mem person2 applied) ~msg:"condition_if applied to db2 does not contain person2";
  assert_equal true (List.mem person3 applied) ~msg:"condition_if applied to db2 contains person3";
  assert_equal true (List.mem person4 applied) ~msg:"condition_if applied to db2 contains person4"

let public_sort _ =  
  let sorted = sort comparator_age db2 in

  assert_equal true (List.equal (fun a b -> a = b) [person4; person1; person2; person3] sorted) ~msg:"sort db2 by ascending age"

let public_queryby _ =   
  let applied = queryBy condition_age db2 comparator_age in

  assert_equal true (List.equal (fun a b -> a = b) [person2; person3] applied) ~msg:"applying condition_age, sorted by ascending age, returns [person2; person3]"

let public_update _ =
  let updated = update condition_name db2 change_name in
  let applied = query condition_empty updated in 
  
  assert_equal 4 (List.length applied) ~msg:"update db2 with condition_name and change_name has length 4";
  assert_equal true (List.mem {name = "Luke"; age = 98; hobbies = ["Legos";"Gaming"]} applied) ~msg:"updated db2 contains person3 with name changed to 'Luke'";
  assert_equal true (List.mem person1 applied) ~msg:"updated db2 contains person1";
  assert_equal true (List.mem person2 applied) ~msg:"updated db2 contains person2";
  assert_equal true (List.mem person4 applied) ~msg:"updated db2 contains person4"

let public_deleteall _ =
  let applied = deleteAll condition_age db2 in
  let applied = query condition_empty applied in

  assert_equal 2 (List.length applied) ~msg:"deleting db2 with condition_age has 2 elements";
  assert_equal true (List.mem person1 applied) ~msg:"deleted db2 with condition_age contains person1";
  assert_equal true (List.mem person4 applied) ~msg:"deleted db2 with condition_age contains person4"

(* PART 2/3: BINARY TREE*)

let btree1 = BiNode(BiNode(BiNode(Leaf, "D", Leaf), "B", BiNode(Leaf, "E", BiNode(Leaf, "F", Leaf))), "A", BiNode(Leaf, "C", BiNode(BiNode(Leaf, "H", Leaf), "G", Leaf))) 
let btree2 = BiNode(BiNode(Leaf, 2, Leaf), 1, BiNode(Leaf, 3, Leaf)) 
let btree3 = BiNode(BiNode(BiNode(Leaf, (2, "c"), Leaf), (1, "b"), Leaf), (0, "a"), Leaf)

let public_flatten _ = 
  assert_equal (flatten btree1) [Lf; Lf; Nd "D"; Lf; Lf; Lf; Nd "F"; Nd "E"; Nd "B"; Lf; Lf; Lf; Nd "H"; Lf;
  Nd "G"; Nd "C"; Nd "A"];
  assert_equal (flatten btree2) [Lf; Lf; Nd 2; Lf; Lf; Nd 3; Nd 1];
  assert_equal (flatten btree3) [Lf; Lf; Nd (2, "c"); Lf; Nd (1, "b"); Lf; Nd (0, "a")]

let public_unflatten _ =
  assert_equal (unflatten (flatten btree1)) btree1;
  assert_equal (unflatten (flatten btree2)) btree2;
  assert_equal (unflatten (flatten btree3)) btree3;

  assert_equal (unflatten (flatten (unflatten (flatten btree3)))) btree3

(* PART 3/3: N_TREE*)


let t1 =
  Node("a", 
    [Node("b", 
      [Node ("c", []); Node ("d", [ Node ("f", []) ]); Node ("e", [])]);
    Node ("g", 
      [Node ("x", [])])
    ])

let t2 = 
  Node(1, 
    [Node(2, []); 
     Node(3, [
        Node(4, [Node(5, [])]); Node(6, []); Node(7, [Node(8, []); Node(9, [])])
     ])
    ])

let l1 = [("a", 2); ("b", 3); ("c", 0); ("d", 1); ("f", 0); ("e", 0); ("g", 1); ("x", 0)]
let l2 = [(1,2); (2,0); (3,3); (4,1); (5,0); (6,0); (7,2); (8,0); (9,0)]

let rec check_equal tree1 tree2 = 
  match tree1, tree2 with
  | Node(a, t1), Node(b, t2) -> (a = b) && (List.fold_left2 (fun acc c d -> acc && (check_equal c d)) true t1 t2)

let public_encode1 _ =
  let lst1 = encode t1 in

  assert_equal 8 (List.length lst1) ~msg:"encoded t1 has 8 elements";
  assert_equal true (List.equal (fun (a,b) (c, d) -> (a = c) && (b = d)) lst1 l1) ~msg:"check that all 8 elements match for encode t1"

let public_encode2 _ =
  let lst2 = encode t2 in

  assert_equal 9 (List.length lst2) ~msg:"encoded t2 has 9 elements";
  assert_equal true (List.equal (fun (a,b) (c, d) -> (a = c) && (b = d)) lst2 l2) ~msg:"check that all 8 elements match for encode t1"

let public_decode _ = 
  let tree1 = decode l1 in 
  let tree2 = decode l2 in

  assert_equal true (check_equal tree1 t1) ~msg:"decode l1 gives you t1";
  assert_equal true (check_equal tree2 t2) ~msg:"decode l2 gives you t2"

let suite =
  "public"
  >::: [
          "public_insert1" >:: public_insert1;
          "public_remove1" >:: public_remove1;
          "public_remove2" >:: public_remove2;
          "public_query1" >:: public_query1;
          "public_query2" >:: public_query2;
          "public_sort" >:: public_sort;
          "public_queryBy" >:: public_queryby;
          "public_update" >:: public_update;
          "public_deleteAll" >:: public_deleteall;
          "public_encode1" >:: public_encode1;
          "public_encode2" >:: public_encode2;
          "public_decode" >:: public_decode;
          "public_flatten" >:: public_flatten;
          "public_unflatten" >:: public_unflatten;
        ]

let _ = run_test_tt_main suite