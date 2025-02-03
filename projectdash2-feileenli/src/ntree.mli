type 'a tree = BiNode of 'a tree * 'a * 'a tree | Leaf
type 'a flat = Lf | Nd of 'a
type 'a n_tree = Node of 'a * 'a n_tree list

val flatten : 'a tree -> 'a flat list
val unflatten : 'a flat list -> 'a tree
val encode : 'a n_tree -> ('a * int) list
val decode : ('a * int) list -> 'a n_tree
