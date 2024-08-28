open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token and returns the rest of the list *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with (* returns the next token in the token list as an option *)
    (* expr -> LetExpr *)
    |Some Tok_Let -> parse_LetExpr toks 
    (* expr -> IfExpr *)
    |Some Tok_If -> parse_IfExpr toks 
    (* expr -> FunctionExpr *)
    |Some Tok_Fun -> parse_FunExpr toks 
    (* expr -> OrExpr *)
    |_ -> parse_OrExpr toks 
  
    and parse_LetExpr toks = 
      match lookahead toks with 
      | Some Tok_Let -> (* LetExpr -> let Recursion Tok_ID = Expr in Expr *)
        let t1 = match_token toks Tok_Let in 
        let t2, e1 = parse_RecursionExpr t1 in 
        (match lookahead t2 with 
          | Some Tok_ID c -> 
            let t3 = match_many t2 [Tok_ID c; Tok_Equal] in 
            let t4, e2 = parse_expr t3 in 
            let t5 = match_token t4 Tok_In in 
            let t6, e3 = parse_expr t5 in 
            (t6, Let (c, e1, e2, e3))
          | _ -> raise (InvalidInputException "invalid letexpr1 input"))
      | _ -> raise (InvalidInputException "invalid letexpr input")
      
  and parse_RecursionExpr toks = 
    match lookahead toks with 
    | Some Tok_Rec -> (* Recursion -> rec | ε *)
      let t1 = match_token toks Tok_Rec in (t1, true)
    | _ -> (toks, false)    

  and parse_IfExpr toks = 
    match lookahead toks with 
    | Some Tok_If -> (* IfExpr -> if Expr then Expr else Expr *)
      let t1 = match_token toks Tok_If in 
      let t2, e1 = parse_expr t1 in 
      let t3 = match_token t2 Tok_Then in 
      let t4, e2 = parse_expr t3 in 
      let t5 = match_token t4 Tok_Else in 
      let t6, e3 = parse_expr t5 in 
      (t6, If (e1, e2, e3))
    | _ -> raise (InvalidInputException "invalid ifexpr input")

      and parse_FunExpr toks = 
        match lookahead toks with 
        | Some Tok_Fun -> (* FunctionExpr -> fun Tok_ID -> Expr *)
          let t1 = match_token toks Tok_Fun in 
          (match lookahead t1 with 
            | Some Tok_ID c -> 
              let t2 = match_many t1 [Tok_ID c; Tok_Arrow] in 
              let t3, e1 = parse_expr t2 in 
              (t3, Fun (c, e1))
            | _ -> raise (InvalidInputException "invalid funexpr1 input"))
        | _ -> raise (InvalidInputException "invalid funexpr input")

    and parse_OrExpr toks = 
      let t1, e1 = parse_AndExpr toks in 
        match lookahead t1 with 
        | Some Tok_Or -> (* OrExpr -> AndExpr || OrExpr *)
          let t2 = match_token t1 Tok_Or in 
          let t3, e2 = parse_OrExpr t2 in 
          (t3, Binop (Or, e1, e2)) 
        | _ -> (t1, e1) (* OrExpr -> AndExpr *)
    
  and parse_AndExpr toks = 
    let t1, e1 = parse_EqualityExpr toks in 
      match lookahead t1 with 
      | Some Tok_And -> (* AndExpr -> EqualityExpr && AndExpr *)
        let t2 = match_token t1 Tok_And in 
        let t3, e2 = parse_AndExpr t2 in 
        (t3, Binop (And, e1, e2)) 
      | _ -> (t1, e1) (* AndExpr -> EqualityExpr *)

  and parse_EqualityExpr toks = 
    let t1, e1 = parse_RelationalExpr toks in 
      match lookahead t1 with 
      | Some Tok_Equal -> (* EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr *)
        let t2 = match_token t1 Tok_Equal in 
        let t3, e2 = parse_EqualityExpr t2 in 
        (t3, Binop (Equal, e1, e2)) 
      | Some Tok_NotEqual -> (* EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr *)
        let t2 = match_token t1 Tok_NotEqual in 
        let t3, e2 = parse_EqualityExpr t2 in 
        (t3, Binop(NotEqual, e1, e2)) 
      | _ -> (t1, e1) (* EqualityExpr -> RelationalExpr *)
  
  and parse_RelationalExpr toks = 
    let t1, e1 = parse_AdditiveExpr toks in 
      match lookahead t1 with 
      | Some Tok_Less -> (* RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr *)
        let t2 = match_token t1 Tok_Less in 
        let t3, e2 = parse_RelationalExpr t2 in 
        (t3, Binop(Less, e1, e2)) 
      | Some Tok_Greater -> (* RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr *)
        let t2 = match_token t1 Tok_Greater in 
        let t3, e2 = parse_RelationalExpr t2 in 
        (t3, Binop(Greater, e1, e2)) 
      | Some Tok_LessEqual -> (* RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr *)
        let t2 = match_token t1 Tok_LessEqual in 
        let t3, e2 = parse_RelationalExpr t2 in 
        (t3, Binop (LessEqual, e1, e2)) 
      | Some Tok_GreaterEqual -> (* RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr *)
        let t2 = match_token t1 Tok_GreaterEqual in 
        let t3, e2 = parse_RelationalExpr t2 in 
        (t3, Binop (GreaterEqual, e1, e2)) 
      | _ -> (t1, e1) (* RelationalExpr ->  AdditiveExpr *)

 and parse_AdditiveExpr toks = 
    let t1, e1 = parse_MultiplicativeExpr toks in 
      match lookahead t1 with 
      | Some Tok_Add -> (* AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr *)
        let t2 = match_token t1 Tok_Add in 
        let t3, e2 = parse_AdditiveExpr t2 in 
        (t3, Binop (Add, e1, e2)) 
      | Some Tok_Sub -> (* AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr *)
        let t2 = match_token t1 Tok_Sub in 
        let t3, e2 = parse_AdditiveExpr t2 in 
        (t3, Binop (Sub, e1, e2)) 
      | _ -> (t1, e1) (* AdditiveExpr -> MultiplicativeExpr *)

  and parse_MultiplicativeExpr toks = 
    let t1, e1 = parse_ConcatExpr toks in 
      match lookahead t1 with 
      | Some Tok_Mult -> (* MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr *)
        let t2 = match_token t1 Tok_Mult in 
        let t3, e2 = parse_MultiplicativeExpr t2 in 
        (t3, Binop (Mult, e1, e2)) 
      | Some Tok_Div -> (* MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr *)
        let t2 = match_token t1 Tok_Div in 
        let t3, e2 = parse_MultiplicativeExpr t2 in 
        (t3, Binop (Div, e1, e2)) 
      | _ -> (t1, e1) (* MultiplicativeExpr -> ConcatExpr *)
  
  and parse_ConcatExpr toks = 
    let t1, e1 = parse_UnaryExpr toks in 
      match lookahead t1 with 
      | Some Tok_Concat -> (* ConcatExpr -> UnaryExpr ^ ConcatExpr *)
        let t2 = match_token t1 Tok_Concat in 
        let t3, e2 = parse_ConcatExpr t2 in 
        (t3, Binop (Concat, e1, e2)) 
      | _ -> (t1, e1) (* ConcatExpr -> UnaryExpr *)

 and parse_UnaryExpr toks = 
      match lookahead toks with 
      | Some Tok_Not -> (* UnaryExpr -> not UnaryExpr *)
        let t1 = match_token toks Tok_Not in 
        let t2, e1 = parse_UnaryExpr t1 in
        (t2, Not (e1))
      | _ -> parse_AppExpr toks (* UnaryExpr -> AppExpr *)

 and parse_AppExpr toks = 
    let t1, e1 = parse_SelectExpr toks in 
      match lookahead t1 with 
      | Some Tok_Int c -> (* AppExpr -> SelectExpr PrimaryExpr *)
        let t2, e2 = parse_PrimaryExpr t1 in 
        (t2, App (e1, e2))
      | Some Tok_Bool c  -> (* AppExpr -> SelectExpr PrimaryExpr *)
        let t2, e2 = parse_PrimaryExpr t1 in 
        (t2, App (e1, e2))
      | Some Tok_String c -> (* AppExpr -> SelectExpr PrimaryExpr *)
        let t2, e2 = parse_PrimaryExpr t1 in 
        (t2, App (e1, e2))
      | Some Tok_ID c -> (* AppExpr -> SelectExpr PrimaryExpr *)
        let t2, e2 = parse_PrimaryExpr t1 in 
        (t2, App (e1, e2))
      | Some Tok_LParen -> (* AppExpr -> SelectExpr PrimaryExpr *)
        let t2, e2 = parse_PrimaryExpr t1 in 
        (t2, App (e1, e2))
      |Some Tok_LCurly -> (* AppExpr -> SelectExpr PrimaryExpr *)
        let t2, e2 = parse_RecordExpr t1 in 
        (t2, App (e1, e2))
      | _ -> (t1, e1)

  and parse_SelectExpr toks = 
    let t1, e1 = parse_PrimaryExpr toks in 
    match lookahead t1 with (* SelectExpr -> PrimaryExpr . Tok_ID | PrimaryExpr *)
    | Some Tok_Dot -> 
      let t2 = match_token t1 Tok_Dot in 
      (match lookahead t2 with 
      | Some Tok_ID c -> 
        let t3 = match_token t2 (Tok_ID c) in 
        (t3, Select (Lab (c), e1))
      | Some _ -> raise (InvalidInputException "invalid SELECT1 input")
      | None -> raise (InvalidInputException "invalid select2 input"))
    | _ -> (t1, e1)


and parse_PrimaryExpr toks = (* PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr )| RecordExpr *)
      match lookahead toks with 
      | Some Tok_Int c -> 
        let t1 = match_token toks (Tok_Int c) in 
        (t1, Int c)
      | Some Tok_Bool c  -> 
        let t1 = match_token toks (Tok_Bool c) in 
        (t1, Bool c)
      | Some Tok_String c -> 
        let t1 = match_token toks (Tok_String c) in 
        (t1, String c)
      | Some Tok_ID c -> 
        let t1 = match_token toks (Tok_ID c) in 
        (t1, ID c)
      | Some Tok_LParen ->
        let t1 = match_token toks Tok_LParen in
        let t2, e2 = parse_expr t1 in 
        let t3 = match_token t2 Tok_RParen in 
        (t3, (e2))
      | Some Tok_LCurly -> parse_RecordExpr toks (* PrimaryExpr -> RecordExpr *)
      | _ -> raise (InvalidInputException "invalid primary 2 input")

  and parse_RecordExpr toks = (* RecordExpr -> { RecordBodyExpr } *)
    match lookahead toks with 
    | Some Tok_LCurly -> 
      let t1 = match_token toks Tok_LCurly in 
      let t2, e1 = parse_RecordBodyExpr t1 in  
      let t3 = match_token t2 Tok_RCurly in (t3, Record e1)
    | _ -> raise (InvalidInputException "invalid recrdexp2 input")

and parse_RecordBodyExpr toks = (* RecordBodyExpr -> Tok_ID = Expr ; RecordBodyExpr | Tok_ID = Expr *)
    match lookahead toks with 
    | Some Tok_ID c -> 
      let t1 = match_token toks (Tok_ID c) in 
      let t2 = match_token t1 Tok_Equal in 
      let t3, e1 = parse_expr t2 in 
      (match lookahead t3 with 
      | Some Tok_Semi ->
        let t4 = match_token t3 Tok_Semi in 
        let t5, e2 = parse_RecordBodyExpr t4 in 
        (t5, (Lab c, e1)::e2)
      | Some Tok_RCurly -> (t3, [(Lab c, e1)])
      | _ -> raise (InvalidInputException "invalid recrdbodyexpr1 input")) 
    | Some Tok_RCurly -> (toks, [])
    | _ -> raise (InvalidInputException "invalid recrdbodyexpr input")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = (* Mutop -> DefMutop | ExprMutop | ;; *)
  match lookahead toks with 
    (* Mutop -> DefMutop *)
    |Some Tok_Def -> parse_DefMutop toks 
    (* Mutop -> ;; *)
    |Some Tok_DoubleSemi -> 
      let t1 = match_token toks Tok_DoubleSemi in 
      (t1, NoOp)
    (* Mutop -> ExprMutop *)
    |_ -> parse_ExprMutop toks 

and parse_DefMutop toks = (* DefMutop -> def Tok_ID = Expr ;; *)
  let t1 = match_token toks Tok_Def in 
  match lookahead t1 with 
  | Some Tok_ID c -> 
    let t2 = match_token t1 (Tok_ID c) in 
    let t3 = match_token t2 Tok_Equal in 
    let t4, e1 = parse_expr t3 in 
    let t5 = match_token t4 Tok_DoubleSemi in 
    (t5, Def (c, e1))
  | _ -> raise (InvalidInputException "invalid defmutop input")

and parse_ExprMutop toks = (* ExprMutop -> Expr ;; *)
  let t1, e1 = parse_expr toks in 
  let t2 = match_token t1 Tok_DoubleSemi in 
  (t2, Expr (e1))
  





