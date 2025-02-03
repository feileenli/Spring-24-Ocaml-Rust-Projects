type 'a tree = BiNode of 'a tree * 'a * 'a tree | Leaf
type 'a flat = Lf | Nd of 'a
type 'a n_tree = Node of 'a * 'a n_tree list

(* TODO: Implement the functions below *)

let rec flatten (input : 'a tree) : 'a flat list = 
  match input with 
  |Leaf -> [Lf]
  |BiNode (lt, r, rt) -> flatten lt@flatten rt@[Nd r]

let rec aux input = 
  match input with 
  |Lf::xs -> (xs, Leaf) 
  |Nd x::xs -> 
  let (newlst, rtree) = aux xs in 
  let (lst, ltree) = aux (newlst) in 
  (lst, BiNode(ltree, x, rtree))
  |_-> ([], Leaf)

let unflatten (input : 'a flat list) : 'a tree = 
  let input = List.rev input in 
  let root = aux input in 
  match root with 
  |(lst, t) -> t
  |_, _ -> Leaf 
  
let rec encode (input : 'a n_tree) : ('a * int) list = 
  match input with 
  |Node(x, children) -> (x, List.length children)::List.concat (List.map encode children) 

let push x stack = x::stack 

let rec pop n stack = 
  if n <= 0 then ([], stack) else 
    match stack with 
    |[] -> failwith("empty stack")
    |top::rest -> let popped, rem_stack = pop (n-1) rest in 
    top::popped, rem_stack 
  
let rec build_tree input stack =
    match input with 
    |[]-> stack 
    |(root,num)::tail -> if num = 0 then let stack = push (Node(root, [])) stack in build_tree tail stack
    else let children, rem_stack = pop num stack in let stack = push (Node(root, children)) rem_stack in build_tree tail stack 

let decode (input : ('a * int) list) : 'a n_tree = let lst = 
  let input = List.rev input in build_tree input [] in
  match lst with 
  |x::xs -> x 
  |[] -> failwith("empty tree")