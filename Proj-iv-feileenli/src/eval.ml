open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | ID x -> lookup env x 
  | Bool b -> Bool b
  | Int i -> Int i
  | String s -> String s
  | Not e1 -> 
    (match (eval_expr env e1) with 
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "expected type bool"))
  | Binop (op, e1, e2) -> 
    (match op with 
      | Add -> 
        (match (eval_expr env e1, eval_expr env e2) with 
          | (Int n1, Int n2) -> Int (n1 + n2)
          | _ -> raise (TypeError "expected type int"))
      | Sub -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Int (n1 - n2)
        | _ -> raise (TypeError "expected type int"))
      | Mult -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Int (n1 * n2)
        | _ -> raise (TypeError "expected type int"))
      | Div -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> if n2 = 0 then raise (DivByZeroError) else Int (n1 / n2)
        | _ -> raise (TypeError "expected type int"))
      | Greater -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Bool (n1 > n2)
        | _ -> raise (TypeError "expected type int"))
      | Less -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Bool (n1 < n2)
        | _ -> raise (TypeError "expected type int"))
      | GreaterEqual -> 
       (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Bool (n1 >= 2)
        | _ -> raise (TypeError "expected type int"))
      | LessEqual -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Bool (n1 <= n2)
        | _ -> raise (TypeError "expected type int"))
      | Concat -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (String s1, String s2) -> String (s1 ^ s2)
        | _ -> raise (TypeError "expected type string"))
      | Equal -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Bool (n1 = n2)
        | (Bool b1, Bool b2) -> Bool (b1 = b2)
        | (String s1, String s2) -> Bool (s1 = s2)
        | _ -> raise (TypeError "cannot compare types"))
      | NotEqual -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Int n1, Int n2) -> Bool (n1 <> n2)
        | (Bool b1, Bool b2) -> Bool (b1 <> b2)
        | (String s1, String s2) -> Bool (s1 <> s2)
        | _ -> raise (TypeError "cannot compare types"))
      | Or -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Bool b1, Bool b2) -> Bool (b1 || b2)
        | _ -> raise (TypeError "expected type Bool"))
      | And -> 
        (match (eval_expr env e1, eval_expr env e2) with 
        | (Bool b1, Bool b2) -> Bool (b1 && b2)
        | _ -> raise (TypeError "expected type Bool"))
      | _ -> raise (TypeError "invalid op input"))
    | If (e1, e2, e3) -> 
      (match (eval_expr env e1) with 
      | Bool true -> eval_expr env e2
      | Bool false -> eval_expr env e3
      | _ -> raise (TypeError "expected bool value in e1"))
   | Let (x, r, e1, e2) -> 
      (match r with 
      | false ->       
        let v1 = eval_expr env e1 in 
        let env' = extend env x v1 in 
        let v2 = eval_expr env' e2 in v2 
      | true -> 
        let env' = extend_tmp env x in
        let v1 = eval_expr env' e1 in 
        let smth = update env' x v1 in 
        let v2 = eval_expr env' e2 in v2 
      | _ -> raise (TypeError "expected bool value"))
    | Fun (x, e1) -> Closure (env, x, e1)
    | App (e1, e2) -> 
      (match eval_expr env e1 with 
      | Closure (env', x, e) -> 
        let v1 = eval_expr env e2 in 
        let env' = extend env' x v1 in 
        let v = eval_expr env' e in v
      | _ -> raise (TypeError "not a function"))
    | Record lst -> Record lst 
    | Select (Lab x, e) -> 
      (match eval_expr env e with 
        | Record lst -> (let rec search l v = 
          match l with
            | [] -> raise (SelectError "no value in record")
            | (Lab var, value) :: t -> if x = var then value else search t v in search lst x)
        | _ -> raise (TypeError "not a record select"))
    | _ -> raise (TypeError "invalid input")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | Def (vr, e) -> 
    let env' = extend_tmp env vr in 
    let ex = eval_expr env' e in 
    let smth = update env' vr ex in (env', Some ex )
  | Expr e -> let e1 = eval_expr env e in (env, Some e1)
  | NoOp -> (env, None)
  | _ -> raise (TypeError "invalid input")
