open Regex
open Formatter
open Globals

let der_printed = ref false

let rec regex_equal r1 r2 =
  match (r1, r2) with
  | (Zero, Zero) | (One, One) -> true
  | (Char c1, Char c2) -> c1 = c2
  | (Any cs1, Any cs2) -> CharSet.equal cs1 cs2
  | (CFun (id1, _), CFun (id2, _)) -> id1 = id2
  | (Seq (l1, r1), Seq (l2, r2)) -> regex_equal l1 l2 && regex_equal r1 r2
  | (Alt (l1, r1), Alt (l2, r2)) -> regex_equal l1 l2 && regex_equal r1 r2
  | (Star r1, Star r2) -> regex_equal r1 r2
  | (NTimes (r1, n1), NTimes (r2, n2)) -> n1 = n2 && regex_equal r1 r2
  | (Opt r1, Opt r2) -> regex_equal r1 r2
  | (Plus r1, Plus r2) -> regex_equal r1 r2
  | (Recd (s1, r1), Recd (s2, r2)) -> s1 = s2 && regex_equal r1 r2
  | _ -> false

let rec nullable(r: regex) = 
  match r with
  | Zero | Char(_) | Any(_) | CFun(_, _) -> false
  | One  | Star(_) | Opt(_) -> true
  | Seq(r1, r2) -> nullable r1 && nullable r2
  | Alt(r1, r2) -> nullable r1 || nullable r2
  | NTimes(r, n) -> n = 0 || nullable(r)
  | Recd(_, r) | Plus (r) -> nullable(r)


let rec der(r: regex) (c: char) =
  match r with
  | Zero | One -> Zero
  | Char(x) -> if x = c then One else Zero
  | Any(cset) -> if CharSet.mem c cset then One else Zero
  | CFun(_, f) -> if f c then One else Zero
  | Seq(r1, r2) -> 
    if nullable r1
    then (der r1 c ** r2) ||| der r2 c
    else der r1 c ** r2
  | Alt(r1, r2) -> der r1 c ||| der r2 c
  | Star(r1) -> der r1 c ** Star(r1)
  | NTimes(_, n) when n <= 0 -> Zero
  | NTimes(r1, n) -> der r1 c ** NTimes(r1, n - 1)
  | Opt(r1) -> der r1 c
  | Plus(r1) -> der r1 c ** Star(r1) 
  | Recd(_, r1) -> der r1 c


let f_id (v: value) : value = v

let f_right (f: value -> value) : value -> value =
  fun v -> Right(f v)

let f_left (f: value -> value) : value -> value =
  fun v -> Left(f v)

let f_alt (f1: value -> value) (f2: value -> value) : value -> value =
  fun v -> match v with
  | Right(vp) -> Right(f2 vp)
  | Left(vp) -> Left(f1 vp)
  | _ -> failwith "f_alt received non-alt value"

let f_seq (f1: value -> value) (f2: value -> value) : value -> value =
  fun v -> match v with
  | VSeq(v1, v2) -> VSeq(f1 v1, f2 v2)
  | _ -> failwith "f_seq received non-seq value"

let f_seq_empty1 (f1: value -> value) (f2: value -> value) : value -> value =
  fun v -> VSeq(f1 Empty, f2 v)

let f_seq_empty2 (f1: value -> value) (f2: value -> value) : value -> value =
  fun v -> VSeq(f1 v, f2 Empty)

let f_error (_: value) : value =
  failwith "Error in rectification function"


let rec simp(r: regex): regex * (value -> value) =
  incr simp_calls;   
  match r with
  | Seq(r1, r2) -> (match (simp r1, simp r2) with
    | (Zero, _), (_, _) -> (Zero, f_error)
    | (_, _), (Zero, _) -> (Zero, f_error)
    | (One, f1), (r2s, f2) -> (r2s, f_seq_empty1 f1 f2)
    | (r1s, f1), (One, f2) -> (r1s, f_seq_empty2 f1 f2)
    | (r1s, f1), (r2s, f2) -> (r1s ** r2s, f_seq f1 f2))
  | Alt(r1, r2) -> (match (simp r1, simp r2) with
    | (Zero, _), (r1s, f2) -> (r1s, f_right f2)
    | (r1s, f1), (Zero, _) -> (r1s, f_left f1)
    | (r1s, f1), (r2s, _) when regex_equal r1s r2s -> (r1s, f_left f1)
    | (r1s, f1), (r2s, f2) -> (r1s ||| r2s, f_alt f1 f2))
  | Star(Star(r1)) -> (Star(r1), f_id)
  | r -> (r, f_id)


let rec simp2(r: regex): regex * (value -> value) =
  incr simp2_calls;
  match r with
  | Seq(r1, r2) -> (match (simp2 r1) with
    | (Zero, _) -> (Zero, f_error)
    | (One, f1) -> (r2, f_seq_empty1 f1 f_id)
    | (r1s, f1) -> (r1s ** r2, f_seq f1 f_id))
  | Alt(r1, r2) -> (match (simp2 r1, simp2 r2) with
    | (Zero, _), (r1s, f2) -> (r1s, f_right f2)
    | (r1s, f1), (Zero, _) -> (r1s, f_left f1)
    | (r1s, f1), (r2s, _) when regex_equal r1s r2s -> (r1s, f_left f1)
    | (r1s, f1), (r2s, f2) -> (r1s ||| r2s, f_alt f1 f2))
  | Star(Star(r1)) -> (Star(r1), f_id)
  | r -> (r, f_id)


let rec mkEps (r: regex) =
  match r with 
  | Star(_) -> StarVal([])
  | Seq(r1, r2) -> VSeq(mkEps r1, mkEps r2)
  | Alt(r1, _) when nullable r1 -> Left(mkEps r1)
  | Alt(_, r2) when nullable r2 -> Right(mkEps r2)
  | NTimes(_, n) when (n = 0) -> Empty
  | NTimes(r1, n) -> NTVal(mkEps r1, n)
  | Plus(r1) -> VSeq(mkEps r1, StarVal([]))
  | Opt(r1) -> if nullable r1 then mkEps r1 else Empty
  | One -> Empty
  | Recd(x, r1) -> Rec(x, mkEps r1)
  | _ -> failwith (Printf.sprintf "Mkeps recieved an odd regex: %s\n" (display_regex r))


let rec inj (r: regex) (c: char) (v: value) = 
  match (r, v) with
  | (Seq(r1, r2), v) -> (match v with
    | VSeq(v1, v2) -> VSeq(inj r1 c v1, v2)
    | Left(VSeq(v1, v2)) -> VSeq(inj r1 c v1, v2)
    | Right(v2) -> VSeq(mkEps(r1), inj r2 c v2)
    | _ -> failwith @@ Printf.sprintf "Val not injectable at Regex: %s with Val: %s" (display_regex r) (display_value v))
  | (Alt(r1, r2), v) -> (match v with
    | Left(v1) -> Left(inj r1 c v1)
    | Right(v2) -> Right(inj r2 c v2)
    | _ -> failwith @@ Printf.sprintf "Val not injectable at Regex: %s with Val: %s" (display_regex r) (display_value v))
  | (NTimes(r1, _), VSeq(v1, v2)) -> VSeq(inj r1 c v1, v2)
  | (Char(ch), Empty) when ch = c -> Literal(ch)
  | (Any(cset), Empty) when CharSet.mem c cset -> Literal(c)
  | (CFun(_, f), Empty) when f c -> Literal(c)
  | (Plus(r1), VSeq(v1, v2)) -> VSeq(inj r1 c v1, v2)
  | (Opt(r1), v1) -> inj r1 c v1
  | (Star(r1), VSeq(v1, StarVal(sv))) -> StarVal(inj r1 c v1::sv)
  | (Recd(x, r1), _) -> Rec(x, inj r1 c v)
  | _ -> v


let rec flatten v =
  match v with
  | Empty | StarVal([]) -> [""]
  | StarVal(x :: xs) -> List.append (flatten x) (flatten (StarVal xs))
  | Literal l -> [String.make 1 l]
  | VSeq(v1, v2) -> flatten v1 @ flatten v2
  | Left(v1) -> flatten v1
  | Right v1 -> flatten v1
  | NTVal(_, 0) -> []
  | NTVal(v1, n) -> flatten v1 @ (flatten (NTVal(v1, (n - 1))))
  | Rec(_, v) -> flatten v


let rec env v =
  match v with
  | Empty | Literal(_) | StarVal([])-> []
  | StarVal(x :: xs) -> List.append (env x) (env (StarVal xs))
  | VSeq(v1, v2) -> env v1 @ env v2
  | Left(v1) -> env v1
  | Right(v1) -> env v1
  | Rec(x, v) -> List.append [(x, (s_str (flatten v)))] (env v)
  | NTVal(v, n) -> env v @ (env (NTVal(v, n - 1)))

let rec lex (r: regex) (s: char list) : value =
  if !verbose then Printf.printf "Lex function atching r: %s on the string %s\n" (display_regex r) (c_str s);
  match s with
  | [] -> if nullable r then mkEps r else failwith (Printf.sprintf "Matcher found no matches for the string %s with the regex %s" (c_str s) (display_regex r))
  | c::cs -> inj r c (lex (der r c) cs)

let rec lex_simp (r: regex) (cl: char list) : value = 
  if !verbose then Printf.printf "Lex function atching r: %s on the string %s\n" (display_regex r) (c_str cl);
  match cl with
  | [] -> if nullable r then mkEps r else failwith (Printf.sprintf "Matcher found no matches for the string %s with the regex %s" (c_str cl) (display_regex r))
  | c::cs -> 
    let (r_simp, f_complicate) = simp(der r c) in 
    inj r c (f_complicate(lex_simp r_simp cs))
    (* Printf.printf "\n\n\n differentiated for: %c\n\n\n Simplified: %s\n\n\n to: %s\n---------------------" c (display_regex r) (display_regex r_simp); *)

let rec lex_simp2 (r: regex) (cl: char list) : value = 
  if !verbose then Printf.printf "Lex function atching r: %s on the string %s\n" (display_regex r) (c_str cl);
  match cl with
  | [] -> if nullable r then mkEps r else failwith (Printf.sprintf "Matcher found no matches for the string %s with the regex %s" (c_str cl) (display_regex r))
  | c::cs -> 
    let (r_simp, f_complicate) = simp2(der r c) in 
    inj r c (f_complicate(lex_simp2 r_simp cs))

let parse (r: regex) (s: char list) : (string * string) list = 
  env (lex_simp r s)
