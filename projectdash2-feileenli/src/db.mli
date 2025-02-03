type person = { name : string; age : int; hobbies : string list }
type db

val newDatabase : db
val insert : person -> db -> db
val remove : string -> db -> db

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

val query : condition -> db -> person list

type comparator = person -> person -> int

val sort : comparator -> db -> person list
val queryBy : condition -> db -> comparator -> person list
val update : condition -> db -> (person -> person) -> db
val deleteAll : condition -> db -> db
