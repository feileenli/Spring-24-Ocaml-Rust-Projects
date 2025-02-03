open Funs 

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let abs x = if x < 0 then -x else x

let rev_tup tup = let (a,b,c) = tup in (c,b,a)

let is_even x = if (x mod 2 == 0) then true else false 

let area point1 point2 = 
    match (point1, point2) with 
    | ((a,b), (c,d)) -> abs((a-c) * (b-d)) 

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  match n with 
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci(n-1) + fibonacci(n-2)

let rec pow x p = 
  match p with 
  | 0 -> 1
  | _ -> x * pow x (p-1)

let rec log x y = if (y/x) >= 1 then 1 + log x (y/x) else 0 

let rec gcf x y = if y = 0 then x else gcf y (x mod y)

(*****************)
(* Part 3: Lists *)
(*****************)

let prepend a x = x::a 

let reverse lst = fold prepend [] lst 

let rec zip lst1 lst2 = 
    match (lst1, lst2) with 
    |[],_ -> [] 
    |_,[]->[]
    |((a,b)::t1, (c,d)::t2) -> (a,b,c,d)::zip t1 t2

let rec merge lst1 lst2 = 
    match (lst1, lst2) with 
    |x,[] -> x
    |[], y -> y 
    |a::t1, b::t2 -> if a < b then a::merge t1 lst2 else b::merge lst1 t2

let rec is_present lst v = 
    match lst with 
    |[]->false 
    |h::t -> if v = h then true else is_present t v

let every_nth n lst = 
    let rec aux_nth count lst = 
        match lst with 
        |[] -> [] 
        |h::t -> if count = n then h::aux_nth 1 t else aux_nth (count+1) t in aux_nth 1 lst 

let jumping_tuples lst1 lst2 = 
    let lst = zip lst1 lst2 in
    let result = 
    fold (fun (index, acc1, acc2) (a,b,c,d) -> if index mod 2 = 0 then (index + 1, d::acc1, a::acc2) else (index+1, a::acc1, d::acc2)) (0,[],[]) lst 
    in 
    match result with 
    |_, acc1, acc2 -> (reverse acc1) @ (reverse acc2)

let rec max_func_chain init funcs = 
    match init, funcs with 
    |init, [] -> init 
    |init, h::t -> max (max_func_chain (h init) t) (max_func_chain init t)

(*****************)
(* Part 4: HOF *)
(*****************)

let is_there lst x = 
  fold (fun acc y -> acc || y = x) false lst

let count_occ lst target =
  fold (fun acc y -> if y = target then acc + 1 else acc) 0 lst

let uniq lst = 
  fold(fun acc x -> if is_there acc x then acc else x::acc ) [] lst

let every_xth x lst = 
  let result = 
  fold(fun (count, acc) y -> if count mod x = 0 then (count + 1, y::acc) else (count + 1, acc)) (1,[]) lst 
  in 
  match result with 
  |(_, acc) -> reverse acc 



