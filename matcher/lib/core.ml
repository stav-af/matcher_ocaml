open Regex
open Formatter
open Globals


let ( ** ): regex -> regex -> regex = fun r1 r2 -> Seq (r1, r2)
let ( ||| ): regex -> regex -> regex = fun r1 r2 -> Alt (r1, r2)
let star: regex -> regex = fun r -> Star r
let clts: char list -> CharSet.t = fun chars -> List.fold_left (fun set c -> CharSet.add c set) CharSet.empty chars


let rec nullable(r: regex) = 
  match r with
  | Zero | Char(_) | Any(_) -> false
  | One  | Star(_) | Opt(_) -> true
  | Seq(r1, r2) -> nullable r1 && nullable r2
  | Alt(r1, r2) -> nullable r1 || nullable r2
  | NTimes(r, n) -> n = 0 || nullable(r)
  | Recd(_, r) | Plus (r) -> nullable(r)


let rec der(r: regex) (c: char) =
  if !verbose then Printf.printf "Derivative being calculated for %s wrt. %s\n" (display_regex r) (String.make 1 c);
  match r with
  | Zero | One -> Zero
  | Char(x) -> if x = c then One else Zero
  | Any(cset) -> if CharSet.mem c cset then One else Zero
  | Seq(r1, r2) -> 
    if nullable r1
    then (der r1 c ** r2) ||| der r2 c
    else der r1 c ** r2
  | Alt(r1, r2) -> der r1 c ||| der r2 c
  | Star(r) -> der r c ** Star(r)
  | NTimes(r, n) -> if n = 0 then Zero else (der r c ** NTimes(r, n-1))
  | Opt(r) -> der r c
  | Plus(r) -> der r c ** Star(r) 
  | Recd(_, r1) -> der r1 c


let rec simp(r: regex) =
  match r with
  | Seq(r1, r2) -> (match (simp r1, simp r2) with
    | One, rs | rs, One -> rs
    | Zero, _ | _, Zero -> Zero
    | r1s, r2s -> r1s ** r2s)
  | Alt(r1, r2) -> (match (simp r1, simp r2) with
    | Zero, r1s | r1s, Zero -> r1s
    | r1s, r2s -> r1s ||| r2s)
  | Star(One) | Star(Zero) -> One
  | NTimes(One, _) -> One
  | NTimes(Zero, _) -> Zero
  | Any(cset) when CharSet.is_empty cset -> Zero
  | r -> r


let rec matches (r: regex) (cs: char list) =
  match cs with
  | [] -> nullable r
  | c::s -> matches (simp (der r c)) s


let rec mkEps (r: regex) =
  if !verbose then Printf.printf "mkEps recieved: %s\n" (display_regex r);
  match r with 
  | Star(_) -> StarVal([])
  | Seq(r1, r2) -> Pair(mkEps r1, mkEps r2)
  | Alt(r1, _) when nullable r1 -> Left(mkEps r1)
  | Alt(_, r2) when nullable r2 -> Right(mkEps r2)
  | One -> Empty
  | Recd(x, r1) -> Rec(x, mkEps r1)
  | _ -> failwith (Printf.sprintf "Mkeps recieved an odd regex: %s\n" (display_regex r))


let rec inj (r: regex) (c: char) (v: value) = 
  if !verbose then Printf.printf "Inj recieved r: %s, for c: %s, and v: %s \n" (display_regex r) (String.make 1 c) (display_value v);
  match (r, v) with
  | (Seq(r1, r2), v) -> (match v with
    | Pair(v1, v2) -> Pair(inj r1 c v1, v2)
    | Left(Pair(v1, v2)) -> Pair(inj r1 c v1, v2)
    | Right(v2) -> Pair(mkEps(r1), inj r2 c v2)
    | _ -> failwith @@ Printf.sprintf "Val not injectable at Regex: %s with Val: %s" (display_regex r) (display_value v))
  | (Alt(r1, r2), v) -> (match v with
    | Left(v1) -> Left(inj r1 c v1)
    | Right(v2) -> Right(inj r2 c v2)
    | _ -> failwith @@ Printf.sprintf "Val not injectable at Regex: %s with Val: %s" (display_regex r) (display_value v))
  | (Char(ch), Empty) when ch = c -> Literal(ch)
  | (Any(cset), Empty) when CharSet.mem c cset -> Literal(c)
  | (Star(r1), Pair(v1, StarVal(sv))) -> StarVal(inj r1 c v1::sv)
  | (Recd(x, r1), _) -> Rec(x, inj r1 c v)
  | _ -> v


let rec flatten v =
  match v with
  | Empty | StarVal([]) -> [""]
  | StarVal(x :: xs) -> List.append (flatten x) (flatten (StarVal xs))
  | Literal l -> [String.make 1 l]
  | Pair(v1, v2) -> flatten v1 @ flatten v2
  | Left(v1) -> flatten v1
  | Right v1 -> flatten v1
  | Rec(_, v) -> flatten v


let rec env v =
  match v with
  | Empty | Literal(_) | StarVal([])-> []
  | StarVal(x :: xs) -> List.append (env x) (env (StarVal xs))
  | Pair(v1, v2) -> env v1 @ env v2
  | Left(v1) -> env v1
  | Right(v1) -> env v1
  | Rec(x, v) -> List.append [(x, (s_str (flatten v)))] (env v)


let rec lex (r: regex) (s: char list) : value =
  if !verbose then Printf.printf "Lex function atching r: %s on the string %s\n" (display_regex r) (c_str s);
  match s with
  | [] -> if nullable r then mkEps r else failwith (Printf.sprintf "Matcher found no matches for the string %s with the regex %s" (c_str s) (display_regex r))
  | c::cs -> inj r c (lex (der r c) cs)

let parse (r: regex) (s: char list) : (string * string) list = 
  env (lex r s)
