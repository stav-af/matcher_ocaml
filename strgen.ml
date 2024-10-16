type regex = 
  | Zero
  | One
  | Char of char
  | Seq of regex * regex
  | Alt of regex * regex
  | Star of regex
  | NTimes of regex * int


(* Syntactic sugar for our expressions *)
let ( ** ): regex -> regex -> regex = fun r1 r2 -> Seq (r1, r2)
let ( ||| ): regex -> regex -> regex = fun r1 r2 -> Alt (r1, r2)
let star: regex -> regex = fun r -> Star r

let c_str (cs: char list) = 
  String.concat "" (List.map (String.make 1) cs)

let s_str (ss: string list) =
  String.concat "" ss

let rec nullable(r: regex) = 
  match r with
  | Zero | Char(_) -> false
  | One  | Star(_) -> true
  | Seq(r1, r2) -> nullable r1 && nullable r2
  | Alt(r1, r2) -> nullable r1 || nullable r2
  | NTimes(r, n) -> n = 0 || nullable(r)

let rec display_regex r =
  match r with
  | Zero -> "0"
  | One -> "1"
  | Char c -> Printf.sprintf "%c" c
  | Seq (r1, r2) -> 
      Printf.sprintf "(%s ** %s)" (display_regex r1) (display_regex r2)
  | Alt (r1, r2) -> 
      Printf.sprintf "(%s ||| %s)" (display_regex r1) (display_regex r2)
  | Star r -> Printf.sprintf "(%s)*" (display_regex r)
  | NTimes (r, n) -> Printf.sprintf "(%s{%d})" (display_regex r) n


let rec der(r: regex) (c: char) =
  match r with
  | Zero | One -> Zero
  | Char(x) -> if x = c then One else Zero
  | Seq(r1, r2) -> 
    if nullable r1
    then (der r1 c ** r2) ||| der r2 c
    else der r1 c ** r2
  | Alt(r1, r2) -> der r1 c ||| der r2 c
  | Star(r) -> der r c ** Star(r)
  | NTimes(r, n) -> if n = 0 then Zero else (der r c ** NTimes(r, n-1))

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
  Printf.printf "strgen with %s and accumulator %s\n" (display_regex r) (c_str acc);
  if curr_depth > max_depth && nullable r then CharListSet.singleton acc else
  let downstream = (match r with
    | Zero | One -> CharListSet.empty
    | Char c -> strgen (der r c) (acc @ [c]) max_depth (curr_depth + 1)
    | Alt(r1, r2) -> 
        let res1 = strgen r1 acc max_depth (curr_depth + 1) in
        let res2 = strgen r2 acc max_depth (curr_depth + 1) in
        CharListSet.union res1 res2
    | Seq(r1, r2) -> 
        let results_r1 = strgen r1 acc max_depth (curr_depth + 1) in
        CharListSet.fold (fun cl acc_set ->
          CharListSet.union (strgen r2 cl max_depth (curr_depth + 2)) acc_set
        ) results_r1 CharListSet.empty
    | Star(rs) -> strgen (Seq(rs, r)) acc max_depth (curr_depth + 1)) in
  if nullable r then CharListSet.add acc downstream else downstream 

let () = 
  let r = Alt(Star(Alt(Char 'a', Char 'b')), Star(Seq(Char 'c', Char 'b'))) in 
  List.iter (fun (st: char list) -> Printf.printf "%s\n" (c_str st)) (CharListSet.elements (strgen r [] 10 0))
