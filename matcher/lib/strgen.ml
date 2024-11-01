
open Regex
open Formatter
open Core
open Globals
(* 
let rec strgen (r: regex) (acc: char list) (max_depth: int) (curr_depth: int)=
  Printf.printf "strgen with %s and accumulator %s\n" (display_regex r) (c_str acc);
  if curr_depth > max_depth then [acc] else
  let downstream = (match r with
    | Zero | One -> []
    | Char c -> strgen (der r c) (acc @ [c]) max_depth (curr_depth + 1)
    | Alt(r1, r2) -> (strgen r1 acc max_depth (curr_depth + 1)) @ (strgen r2 acc max_depth (curr_depth + 1))
    | Seq(r1, r2) -> 
        let results_r1 = strgen r1 acc max_depth (curr_depth + 1) in
        List.concat (List.map (fun (cl: char list) -> strgen r2 cl max_depth (curr_depth + 2)) results_r1)
    | Star(rs) -> strgen (Seq(rs, r)) acc max_depth (curr_depth + 1)) in
  if nullable(r) then acc :: downstream else downstream *)

module CharListSet = Set.Make(struct
  type t = char list
  let compare = compare
end)

let rec strgen (r: regex) (acc: char list) (max_depth: int) (curr_depth: int) =
  if !verbose then Printf.printf "strgen with %s and accumulator %s\n" (display_regex r) (c_str acc);
  if curr_depth > max_depth && nullable r then CharListSet.singleton acc else
  let gen r = strgen r acc max_depth (curr_depth + 1) in
  let downstream = (match r with
    | Zero | One -> CharListSet.empty
    | Char c -> strgen (der r c) (acc @ [c]) max_depth (curr_depth + 1)
    | Any(cset) -> 
      CharSet.fold (fun c ac -> 
        CharListSet.union (strgen (der r c) (acc @ [c]) max_depth (curr_depth + 1)) ac
      ) cset CharListSet.empty
    | Plus(r1) -> gen (Seq(r1, (Star r1)))
    | Opt(r1) -> gen (Alt(One, r1))
    | Alt(r1, r2) -> 
        let res1 = gen r1 in
        let res2 = gen r2 in
        CharListSet.union res1 res2
    | Seq(r1, r2) -> 
        let results_r1 = gen r1 in
        CharListSet.fold (fun cl acc_set ->
          CharListSet.union (strgen r2 cl max_depth (curr_depth + 2)) acc_set
        ) results_r1 CharListSet.empty
    | Star(rs) -> gen (Seq(rs, r))
    | NTimes(rs, n) -> gen (Seq(rs, NTimes(rs, n - 1)))
    | Recd(_, rs) -> gen rs
    | _ -> failwith "Not configured for strgen"
    ) in
  if nullable r then CharListSet.add acc downstream else downstream 

let () = 
  let r = Alt(Star(Alt(Char 'a', Char 'b')), Star(Seq(Char 'c', Char 'b'))) in 
  List.iter (fun (st: char list) -> Printf.printf "%s\n" (c_str st)) (CharListSet.elements (strgen r [] 10 0))
