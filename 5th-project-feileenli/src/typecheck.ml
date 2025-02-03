open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound type for " ^ x))
  | (var, value) :: t -> if x = var then value else lookup t x

let rec find_type_by_label label types =
  match types with
  | [] -> None
  | (l, t) :: rest -> if l = label then Some t else find_type_by_label label rest

let rec is_subtype t1 t2 =
  match (t1, t2) with
  | (TInt, TInt) | (TBool, TBool) -> true
  | (TArrow (s1, s2), TArrow (t1, t2)) ->
      is_subtype t1 s1 && is_subtype s2 t2  
  | (TRec fields1, TRec fields2) ->
      List.for_all (fun (label2, type2) ->
        match find_type_by_label label2 fields1 with
        | Some type1 -> is_subtype type1 type2
        | None -> false
      ) fields2
  | _ -> false

let rec typecheck gamma e = 
  match e with 
  | ID x -> 
    (match (lookup gamma x) with 
      | v1 -> v1
      | _ -> raise (DeclareError "ID type not declared"))
  | Bool b -> TBool
  | Int i -> TInt
  | Not e1 -> 
    (match (typecheck gamma e1) with 
    | TBool -> TBool
    | _ -> raise (TypeError "expected type bool"))
  | Binop (op, e1, e2) -> 
    (match op with 
      | Add -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
          | TInt, TInt -> TInt 
          | _ -> raise (TypeError "expected type int"))
      | Sub -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TInt 
        | _ -> raise (TypeError "expected type int"))
      | Mult -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TInt 
        | _ -> raise (TypeError "expected type int"))
      | Div -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TInt 
        | _ -> raise (TypeError "expected type int"))
      | Greater -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TBool 
        | _ -> raise (TypeError "expected type int"))
      | Less -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TBool 
        | _ -> raise (TypeError "expected type int"))
      | GreaterEqual -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TBool 
        | _ -> raise (TypeError "expected type int"))
      | LessEqual -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TInt, TInt -> TBool 
        | _ -> raise (TypeError "expected type int"))
      | Equal -> 
        if (typecheck gamma e1) = (typecheck gamma e2) then TBool 
        else raise (TypeError "expected same type")
      | NotEqual -> 
        if (typecheck gamma e1) = (typecheck gamma e2) then TBool 
        else raise (TypeError "expected same type")
      | Or -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TBool, TBool -> TBool 
        | _ -> raise (TypeError "expected type Bool"))
      | And -> 
        (match (typecheck gamma e1, typecheck gamma e2) with 
        | TBool, TBool -> TBool 
        | _ -> raise (TypeError "expected type Bool"))
      | _ -> raise (TypeError "invalid input"))
  | If (e1, e2, e3) -> 
    (match (typecheck gamma e1) with 
    | TBool -> if (typecheck gamma e2 = typecheck gamma e3) then 
      typecheck gamma e2 else raise (TypeError "invalid input")
    | _ -> raise (TypeError "invalid input"))
  | Fun (x, t, e1) -> 
    let gamma' = extend gamma x t in 
    TArrow(t, typecheck gamma' e1)
  | App (e1, e2) -> 
    (match (typecheck gamma e1) with 
    | TArrow(t1, t2) -> if (is_subtype (typecheck gamma e2) t1) then 
      t2 else raise (TypeError "invalid input")
    | _ -> raise (TypeError "invalid input"))
  | Record lst -> 
    let new_lst = List.map (fun (a, b) -> (a, typecheck gamma b)) lst in 
    TRec new_lst 
  | Select (Lab x, e) ->
    (match typecheck gamma e with 
    | TRec lst -> (let rec search l v = 
      match l with
        | [] -> raise (SelectError "no value in record")
        | (Lab var, value) :: t -> if x = var then value else search t v in search lst x)
        | _ -> raise (TypeError "not a record select"))
  | Let (x, e1, e2) -> 
      let t1 = typecheck gamma e1 in 
      let gamma' = extend gamma x t1 in 
      let t2 = typecheck gamma' e2 in
      t2 
  | LetRec (x, t, e1, e2) -> 
    let gamma' = extend gamma x t in 
    let t1 = typecheck gamma' e1 in 
    if t = t1 then 
    let t2 = typecheck gamma' e2 in
    t2 else raise (TypeError "invalid input") 
  | _ -> raise (TypeError "invalid input") 