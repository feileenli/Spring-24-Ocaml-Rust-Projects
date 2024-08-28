open Types 

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"

let re_lcurl = Str.regexp "{"
let re_rcurl = Str.regexp "}"

let re_dot = Str.regexp "\\."

let re_eq = Str.regexp "="
let re_noteq = Str.regexp "<>"

let re_gre = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greq = Str.regexp ">="
let re_leq = Str.regexp "<="

let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "!"

let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"

let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "/"

let re_cat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_rec = Str.regexp "rec"
let re_in = Str.regexp "in"
let re_def = Str.regexp "def"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"

let re_posint = Str.regexp "[0-9]+"
let re_negint = Str.regexp "(-[0-9]+)"
let re_bool = Str.regexp "true\\|false"
let re_str = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"

let re_dbsemi = Str.regexp ";;"
let re_semi = Str.regexp ";"

let re_space = Str.regexp "[ \t\n]+"
let re_next = Str.regexp "[a-zA-Z0-9]"

let tokenize input = 
  let length = String.length input in 
  let rec tok pos = 
      if pos >= length then []
      (* longest match rule *)
      else if Str.string_match re_space input pos then 
        let str = Str.matched_string input in 
        let len = String.length str in 
        tok(pos + len)
      (* Tok_ID of string starts with a letter and can be followed by an number of letters or numbers. Can contain keywords*)
      else if Str.string_match re_id input pos then 
        let str = Str.matched_string input in 
        let len = String.length str in
        (* check if matched string is a specific token or if it's really an id *)
        if str = "if" then Tok_If::(tok (pos + 2))
        else if str = "then" then Tok_Then::(tok (pos + 4))
        else if str = "else" then Tok_Else::(tok (pos + 4))
        else if str = "let" then Tok_Let::(tok (pos + 3))
        else if str = "rec" then Tok_Rec::(tok (pos + 3))
        else if str = "in" then Tok_In::(tok (pos + 2))
        else if str = "def" then Tok_Def::(tok (pos + 3))
        else if str = "fun" then Tok_Fun::(tok (pos + 3)) 
        else if str = "true" then Tok_Bool true::(tok(pos + 4))
        else if str = "false" then Tok_Bool false::(tok(pos + 5))
        else if str = "not" then Tok_Not::(tok (pos + 3))
        else Tok_ID(str)::(tok (pos + len))
      (* strings *)
      else if Str.string_match re_str input pos then 
        let str = Str.matched_string input in 
        let len = String.length str in 
        let sanitized = String.sub str 1 (len - 2) in
        Tok_String(sanitized)::(tok (pos + len))
      (* neg integers need to remove () *)
      else if Str.string_match re_negint input pos then 
        let str = Str.matched_string input in 
        let len = String.length str in 
        let num = String.sub str 1 (len-2) in
        Tok_Int(int_of_string num)::(tok (pos + len))
      (* pos integers  *)
      else if Str.string_match re_posint input pos then 
        let str = Str.matched_string input in 
        let len = String.length str in 
        Tok_Int(int_of_string str)::(tok (pos + len))
      (* 2 character tokens *)
      else if Str.string_match re_arrow input pos then Tok_Arrow::(tok (pos + 2))
      else if Str.string_match re_dbsemi input pos then Tok_DoubleSemi::(tok (pos + 2))
      else if Str.string_match re_noteq input pos then Tok_NotEqual::(tok (pos + 2))
      else if Str.string_match re_greq input pos then Tok_GreaterEqual::(tok (pos + 2))
      else if Str.string_match re_leq input pos then Tok_LessEqual::(tok (pos + 2))


      else if Str.string_match re_lparen input pos then Tok_LParen::(tok (pos + 1))
      else if Str.string_match re_rparen input pos then Tok_RParen::(tok (pos + 1))
      else if Str.string_match re_lcurl input pos then Tok_LCurly::(tok (pos + 1))
      else if Str.string_match re_rcurl input pos then Tok_RCurly::(tok (pos + 1))
      else if Str.string_match re_dot input pos then Tok_Dot::(tok (pos + 1))
      else if Str.string_match re_eq input pos then Tok_Equal::(tok (pos + 1))
      else if Str.string_match re_gre input pos then Tok_Greater::(tok (pos + 1))
      else if Str.string_match re_less input pos then Tok_Less::(tok (pos + 1))
      else if Str.string_match re_add input pos then Tok_Add::(tok (pos + 1))
      else if Str.string_match re_sub input pos then Tok_Sub::(tok (pos + 1))
      else if Str.string_match re_mult input pos then Tok_Mult::(tok (pos + 1))
      else if Str.string_match re_div input pos then Tok_Div::(tok (pos + 1))
      else if Str.string_match re_cat input pos then Tok_Concat::(tok (pos + 1))
      else if Str.string_match re_semi input pos then Tok_Semi::(tok (pos + 1))
      else if Str.string_match re_or input pos then Tok_Or::(tok (pos + 2))
      else if Str.string_match re_and input pos then Tok_And::(tok (pos + 2))

      else failwith "lexing error"
      
    in tok 0;;
      
      


      
      
      


