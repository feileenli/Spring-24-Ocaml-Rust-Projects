open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
let rec aux_move nfa qs s = 
  match s with 
  |Some x -> if not (elem x nfa.sigma) then [] else 
    let transitions = List.filter (fun (b, sym, e) -> elem b qs && s = sym) nfa.delta 
    in List.map (fun (b,letter, e) -> e) transitions
  |None -> let transitions = List.filter (fun (b, sym, e) -> elem b qs && sym = None) nfa.delta 
in List.map (fun (b,letter, e) -> e) transitions

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  let result = aux_move nfa qs s in insert_all result [] 
  
let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  if eq (insert_all (move nfa qs None) qs) qs then qs else 
    e_closure nfa (insert_all (move nfa qs None) qs)

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let final = List.sort_uniq Stdlib.compare (List.fold_left 
  (fun state elem -> e_closure nfa (move nfa state (Some elem))) (e_closure nfa [nfa.q0]) (explode s)) 
in ((intersection final nfa.fs) <> []);; 


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.map (fun s -> e_closure nfa (move nfa qs (Some s))) nfa.sigma 

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map (fun s -> let states_lst = e_closure nfa (move nfa qs (Some s)) in (qs, Some s, states_lst)) nfa.sigma 

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let bool_lst = List.map (fun s -> elem s nfa.fs) qs in if elem true bool_lst then [qs] else [] 

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
    match work with 
    |x::xs -> 
    let nt = union (new_trans nfa x) (dfa.delta) in 
    let ns = union ([x]) (dfa.qs) in 
    let nfs = union (new_finals nfa x) (dfa.fs) in 
    let new_dfa =  
    {
      sigma = dfa.sigma;
      qs = ns;
      q0 = dfa.q0;
      fs = nfs;
      delta = nt; 
    } in nfa_to_dfa_step nfa new_dfa (diff (insert_all (new_states nfa x) work) new_dfa.qs)
   |[] -> dfa 

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start = e_closure nfa [nfa.q0] in  
  let dfa = {
    sigma = nfa.sigma;
    qs = [start];
    q0 = start;
    fs = new_finals nfa start;
    delta = new_trans nfa [nfa.q0]; 
  } in 
  nfa_to_dfa_step nfa dfa [start] 
