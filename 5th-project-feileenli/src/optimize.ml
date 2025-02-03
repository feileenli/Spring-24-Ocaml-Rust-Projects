open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> None
  | (var, value) :: t -> if x = var then Some value else lookup t x

let rec optimize env e = 
  match e with 
  | ID x -> 
    (match (lookup env x) with 
      | None -> ID x
      | Some v1-> v1)
  | Bool b -> Bool b
  | Int i -> Int i
  | Not e1 -> 
    (match (optimize env e1) with 
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "expected type bool"))
  | Binop (op, e1, e2) -> 
    (match op with 
      | Add -> 
        (match (optimize env e1, optimize env e2) with 
          | (e1, Int 0) -> e1
          | (Int 0, e2) -> e2
          | (Int n1, Int n2) -> Int (n1 + n2)
          | s1, s2 -> Binop (Add, s1, s2)
          | _ -> raise (TypeError "expected type int"))
      | Sub -> 
        (match (optimize env e1, optimize env e2) with 
        | (e1, Int 0) -> e1
        | (Int n1, Int n2) -> Int (n1 - n2)
        | s1, s2 -> Binop (Sub, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | Mult -> 
        (match (optimize env e1, optimize env e2) with 
        | (e1, Int 1) -> e1
        | (Int 1, e2) -> e2
        | (e1, Int 0) -> Int (0)
        | (Int 0, e2) -> Int (0)
        | (Int n1, Int n2) -> Int (n1 * n2)
        | s1, s2 -> Binop (Mult, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | Div -> 
        (match (optimize env e1, optimize env e2) with 
        | (e1, Int 1) -> e1
        | (e1, Int 0) -> raise (DivByZeroError)
        | (Int 0, e2) -> Int (0)
        | (Int n1, Int n2) -> if n2 = 0 then raise (DivByZeroError) else Int (n1 / n2)
        | s1, s2 -> Binop (Div, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | Greater -> 
        (match (optimize env e1, optimize env e2) with 
        | (Int n1, Int n2) -> Bool (n1 > n2)
        | s1, s2 -> Binop (Greater, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | Less -> 
        (match (optimize env e1, optimize env e2) with 
        | (Int n1, Int n2) -> Bool (n1 < n2)
        | s1, s2 -> Binop (Less, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | GreaterEqual -> 
       (match (optimize env e1, optimize env e2) with 
        | (Int n1, Int n2) -> Bool (n1 >= n2)
        | s1, s2 -> Binop (GreaterEqual, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | LessEqual -> 
        (match (optimize env e1, optimize env e2) with 
        | (Int n1, Int n2) -> Bool (n1 <= n2)
        | s1, s2 -> Binop (LessEqual, s1, s2)
        | _ -> raise (TypeError "expected type int"))
      | Equal -> 
        (match (optimize env e1, optimize env e2) with 
        | (Int n1, Int n2) -> Bool (n1 = n2)
        | (Bool b1, Bool b2) -> Bool (b1 = b2)
        | s1, s2 -> Binop (Equal, s1, s2)
        | _ -> raise (TypeError "cannot compare types"))
      | NotEqual -> 
        (match (optimize env e1, optimize env e2) with 
        | (Int n1, Int n2) -> Bool (n1 <> n2)
        | (Bool b1, Bool b2) -> Bool (b1 <> b2)
        | s1, s2 -> Binop (NotEqual, s1, s2)
        | _ -> raise (TypeError "cannot compare types"))
      | Or -> 
        (match (optimize env e1, optimize env e2) with 
        | (Bool true, ID x) -> Bool (true)
        | (ID x, Bool true) -> Bool (true)
        | (Bool b1, Bool b2) -> Bool (b1 || b2)
        | s1, s2 -> Binop (Or, s1, s2)
        | _ -> raise (TypeError "expected type Bool"))
      | And -> 
        (match (optimize env e1, optimize env e2) with 
        | (Bool false, ID x) -> Bool (false)
        | (ID x, Bool false) -> Bool (false)
        | (Bool b1, Bool b2) -> Bool (b1 && b2)
        | s1, s2 -> Binop (And, s1, s2)
        | _ -> raise (TypeError "expected type Bool"))
      | _ -> raise (TypeError "invalid op input"))
    | If (e1, e2, e3) -> 
      (match (optimize env e1) with 
      | Bool true -> optimize env e2
      | Bool false -> optimize env e3
      | _ -> If (optimize env e1, optimize env e2, optimize env e3)
      | _ -> raise (TypeError "expected bool value in e1"))
    | Fun (x, t, e1) -> 
      let new_env = List.remove_assoc x env in 
      Fun (x, t, optimize new_env e1)
    | App (e1, e2) -> 
      (match optimize env e1 with 
      | Fun (x, t, e) -> 
        let v1 = optimize env e2 in 
        let env' = extend env x v1 in 
        let v = optimize env' e in v
      | _ -> App (optimize env e1, optimize env e2)
      | _ -> raise (TypeError "not a function"))
    | Let (x, e1, e2) -> 
        let v1 = optimize env e1 in 
        let env' = extend env x v1 in 
        let v2 = optimize env' e2 in v2  
    | LetRec (x, t, e1, e2) -> 
        LetRec (x, t, optimize env e1, optimize env e2)
    | Record lst -> 
      let new_lst = List.map (fun (a, b) -> (a, optimize env b)) lst in 
      Record new_lst 
    | Select (Lab x, e) ->
      (match optimize env e with 
      | Record lst -> (let rec search l v = 
        match l with
          | [] -> raise (SelectError "no value in record")
          | (Lab var, value) :: t -> if x = var then value else search t v in search lst x)
          | _ -> raise (TypeError "not a record select"))
    | _ -> raise (TypeError "invalid input")