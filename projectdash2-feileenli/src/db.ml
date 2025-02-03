type person = { name : string; age : int; hobbies : string list }
type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

(* TODO: Implement functions below *)

type db = person list 

let newDatabase = [] 

(*add person to start of list*)
let insert person db = 
  person :: db 

let remove name db = 
  List.filter (fun person -> if person.name <> name then true else false) db 

let sort comparator db = List.sort comparator db 

let rec result condition person = 
  match condition with 
  | True -> true
  | False -> false 
  | Age f -> f person.age 
  | Name f -> f person.name 
  | Hobbies f -> f person.hobbies
  | And (c1, c2) -> result c1 person && result c2 person 
  | Or (c1,c2) -> result c1 person || result c2 person 
  | Not c1 -> not (result c1 person)
  | If (c1, c2, c3) -> if result c1 person then result c2 person else result c3 person 

  (*filter returns all elements of the db that satisfy the function. fun takes in a person and returns a bool*)
let query condition db = List.filter (fun person -> result condition person) db 

let queryBy condition db comparator = let newdb = query condition db in sort comparator newdb 

let rec update condition db change = 
  match db with 
  |[] -> []
  |h::t -> if result condition h then change h::update condition t change else h::update condition t change
 
let deleteAll condition db = List.filter (fun person -> not (result condition person)) db

