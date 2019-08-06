open List
open Sets

let rec map f xs = match xs with
 [] -> []
| x :: xt -> (f x)::(map f xt)
;;
let rec fold f a xs = match xs with
 [] -> a
| x :: xt -> fold f (f a x) xt
;;
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
;;
(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let func a (x, y, z) = if ((List.mem x qs) && y = s && (not (List.mem z a))) then z::a else a in
  fold func [] nfa.delta
;;


let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let func a (x, y, z) = if (((List.mem x qs) || (List.mem x a)) && y = None && (not (List.mem z a))) then z::a else a in
  fold func qs nfa.delta
  
;;




(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let nlist = map (fun x -> Some x) nfa.sigma in
  let sts = e_closure nfa qs in
  let func a l = let ns = move nfa sts l in (e_closure nfa ns)::a in
  fold func [] nlist
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =

    let nlist = map (fun x -> Some x) nfa.sigma in
    let sts = e_closure nfa qs in
    let func a l = let ns = move nfa sts l in (qs, l, e_closure nfa ns)::a in
    fold func [] nlist
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let func a x = if List.mem x qs then qs::a else a in
  fold func [] nfa.fs  
;;

(* let tester b = fold (fun a x -> if (not (List.mem x a)) && (List.length x > 0) then x::a else a) [[0]] (new_states m1 b) ;; *)
let fs nfa b = fold (fun a x -> (fold (fun c d -> if not(List.mem d c) then d::c else c) [] (new_finals nfa x))@a) [] b ;; 
let fs_in x nfa = fold (fun c d -> if not(List.mem d c) then d::c else c) [] (new_finals nfa x) ;; 

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = failwith "unimplemented"
  

let rec helper (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : 'q list list = 
  
    let sigma = nfa.sigma in
    let q0 = dfa.q0 in
    let delta = dfa.delta in
    let fs = dfa.fs in
    let qs = dfa.qs in
    match qs with
    
    | [b] -> if not(List.mem b work) then 
      let new_qs = fold (fun a x -> if (not (List.mem x a)) && (List.length x > 0) then x::a else a) qs (new_states nfa b) in
      helper nfa {sigma; qs=new_qs; q0; fs; delta} (b::work) 
    else
      work
    | h::t -> if not(List.mem h work) then 
      let new_qs = fold (fun a x -> if (not (List.mem x a)) && (List.length x > 0) then x::a else a) qs (new_states nfa h) in
      helper nfa {sigma; qs=new_qs; q0; fs; delta} (h::work) 
    else
      helper nfa {sigma; qs=t; q0; fs; delta} work
    | [] -> work
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let r0 = e_closure nfa [nfa.q0] in
  let q0 = r0 in
  let qs = [r0] in
  let sigma = nfa.sigma in
  let fs = [] in
  let delta = [] in
  
  let tqs = helper nfa {sigma; qs; q0; fs; delta} [] in

  let qs = tqs in

  let delta = fold (fun a x -> (fold (fun c d -> if not(List.mem d c) then d::c else c) [] (new_trans nfa x))@a) [] qs in
  (* fold (fun a x -> fold (fun c d -> if not(List.mem d a) then d::a else a) [] (new_trans nfa x)) [] qs *) 
  
  let fs = fold (fun a x -> (fold (fun c d -> if not(List.mem d c) then d::c else c) [] (new_finals nfa x))@a) [] qs in

  {sigma; qs; q0; fs; delta}

;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let chars = explode s in
  let dfa = nfa_to_dfa nfa in
  let init = dfa.q0 in
  let delta = dfa.delta in

  let fs = dfa.fs in
  let rec func (lst: char list) (a: 'q list) (dlist: ('q list, char) transition list) : bool = 
    (match lst with
    [] -> if (List.mem init fs) then true else false
    | [b] -> (match dlist with
                    [] -> false
                    | (x, y, z)::tl -> print_int 0; if x = a && y = Some b && (List.length z > 0) && (List.mem z fs) then true else func lst a tl)
    | h::t -> (match dlist with
                    [] -> false
                    | (x, y, z)::tl -> if x = a && y = Some h && (List.length z > 0) then func t z delta else func lst a tl)) in
  func chars init delta

;;




