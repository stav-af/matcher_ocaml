type regex = 
  | Zero
  | One
  | Char of char
  | Seq of regex * regex
  | Alt of regex * regex
  | Star of regex
  | NTimes of regex * int
  | Recd of string * regex
  (* | Range of regex * int * int *)

type value =
  | StarVal of value list
  | Empty
  | Literal of char
  | Pair of value * value
  | Left of value
  | Right of value
  | Rec of string * value

(* Syntactic sugar for our expressions *)
let ( ** ): regex -> regex -> regex = fun r1 r2 -> Seq (r1, r2)
let ( ||| ): regex -> regex -> regex = fun r1 r2 -> Alt (r1, r2)
let star: regex -> regex = fun r -> Star r

(* Pretty printing *)
let rec display_value v =
  match v with
  | StarVal(_) -> "StarValue()"
  | Empty -> "Empty"
  | Literal c -> "Literal('" ^ String.make 1 c ^ "')"
  | Pair (v1, v2) -> "Pair(" ^ display_value v1 ^ ", " ^ display_value v2 ^ ")"
  | Left v -> "Left(" ^ display_value v ^ ")"
  | Right v -> "Right(" ^ display_value v ^ ")"
  | Rec (s, v) -> Printf.sprintf "Record(%s: %s)" s (display_value v) 

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
  | Recd (s, r) -> Printf.sprintf "Record(%s: %s)" s (display_regex r) 
  | NTimes (r, n) -> Printf.sprintf "(%s{%d})" (display_regex r) n

let display_env ev = 
  List.iter (fun (x, s) -> Printf.printf "Found submatch (%s: %s)" x s) ev


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
  (* | Range(r, lb, ub) -> n == 0 || nullable(r) *)


let rec der(r: regex) (c: char) =
  Printf.printf "Derivative being calculated for %s wrt. %s\n" (display_regex r) (String.make 1 c);
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
  | Recd(_, r1) -> der r1 c
  (* | Range(r, lb, ub) -> if lb = 0 then Zero else (der r c ** Range(r, lb - 1, ub - 1)) *)


let rec simp(r: regex) =
  match r with
  | Seq(r1, r2) -> (match (simp r1, simp r2) with
    | One, rs | rs, One -> rs
    | Zero, rs| rs, Zero -> Zero
    | r1s, r2s -> r1s ** r2s)
  | Alt(r1, r2) -> (match (simp r1, simp r2) with
    | Zero, r1s | r1s, Zero -> r1s
    | r1s, r2s -> r1s ||| r2s)
  | Star(One) | Star(Zero) -> One
  | NTimes(One, _) -> One
  | NTimes(Zero, _) -> Zero
  (* | Range() *)
  | r -> r


let rec matches (r: regex) (cs: char list) =
  match cs with
  | [] -> nullable r
  | c::s -> matches (simp (der r c)) s


let rec mkEps (r: regex) =
  Printf.printf "mkEps recieved: %s\n" (display_regex r);
  match r with 
  | Star(r) -> StarVal([])
  | Seq(r1, r2) -> Pair(mkEps r1, mkEps r2)
  | Alt(r1, r2) when nullable r1 -> Left(mkEps r1)
  | Alt(r1, r2) when nullable r2 -> Right(mkEps r2)
  | One -> Empty
  | Recd(x, r1) -> Rec(x, mkEps r1)
  | _ -> failwith (Printf.sprintf "Mkeps recieved an odd regex: %s\n" (display_regex r))


let rec inj (r: regex) (c: char) (v: value) = 
  Printf.printf "Inj recieved r: %s, for c: %s, and v: %s \n" (display_regex r) (String.make 1 c) (display_value v);
  match (r, v) with
  | (Seq(r1, r2), v) -> (match v with
    | Pair(v1, v2) -> Pair(inj r1 c v1, v2)
    | Left(Pair(v1, v2)) -> Pair(inj r1 c v1, v2)
    | Right(v2) -> Pair(mkEps(r1), inj r2 c v2))
  | (Alt(r1, r2), v) -> (match v with
    | Left(v1) -> Left(inj r1 c v1)
    | Right(v2) -> Right(inj r2 c v2))
  | (Char(ch), Empty) when ch = c -> Literal(ch)
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
  | Rec(x, v) -> flatten v

let rec env v =
  match v with
  | Empty | Literal(_) | StarVal([])-> []
  | StarVal(x :: xs) -> List.append (env x) (env (StarVal xs))
  | Pair(v1, v2) -> env v1 @ env v2
  | Left(v1) -> env v1
  | Right(v1) -> env v1
  | Rec(x, v) -> List.append [(x, (s_str (flatten v)))] (env v)


let rec lex (r: regex) (s: char list) : value =
  Printf.printf "Lex function atching r: %s on the string %s\n" (display_regex r) (c_str s);
  match s with
  | [] -> if nullable r then mkEps r else failwith (Printf.sprintf "Matcher found no matches for the string %s with the regex %s" (c_str s) (display_regex r))
  | c::cs -> inj r c (lex (der r c) cs)

let parse (r: regex) (s: char list) : (string * string) list = 
  env (lex r s)
  

let regex_test_cases : (regex * char list * bool) list = [
  (Char 'a',                        ['a'], true);
  (Char 'a' ** Char 'b',            ['a'; 'b'], true);
  (star (Char 'a') ** Char 'b',     ['a'; 'a'; 'b'], true);
  (Char 'a' ||| Char 'b',           ['a'], true);
  (Char 'a' ||| Char 'b',           ['c'], false);
  (Char 'a' ** star (Char 'b'),     ['a'; 'b'; 'b'], true);
  (Char 'a' ** Char 'b',            ['a'; 'b'; 'b'], false);
  (Char 'a' ||| Zero ** Char 'b',   ['a'], true);
  (NTimes (Char 'a', 5),             ['a';'a';'a';'a';'a'], true);
  (NTimes (Char 'a', 4),             ['a';'a';'a';'a';'a'], false);
  (NTimes (Char 'a', 6),                 ['a';'a';'a';'a';'a'], false);
  (NTimes ((Char 'a' ** Char 'b'), 6),  ['a';'b';'a';'b';'b'], false);
  (NTimes (Char 'a' ** Char 'b', 6),    ['a';'b';'a';'b';'b';'a'], false);
  (NTimes (Char 'a' ** Char 'b', 3),    ['a';'b';'a';'b';'a';'b'], true);
]

let simp_test_cases : (regex * regex) list = [
  (Seq (Zero, Char 'a'), Zero);
  (Seq (Char 'a', Zero), Zero);
  (Seq (One, Char 'b'), Char 'b');
  (Seq (Char 'b', One), Char 'b');
  (Star One, One);
  (Alt (Zero, Char 'a'), Char 'a');
  (Alt (Char 'a', Zero), Char 'a');
  (Alt (Seq (One, Char 'b'), Seq (Char 'a', Zero)), Char 'b');
  (Seq (Alt (Zero, Char 'a'), Seq (Char 'b', One)), Char 'a' ** Char 'b');
  (Char 'c', Char 'c');
]


let mkEps_test_cases : (regex * value) list = [
  (Alt(Seq(Alt(Zero, Alt(Zero**Char 'b', One)), Alt(Char 'b', One)), Alt(One, Zero)), Left(Pair(Right(Right(Empty)), Right(Empty))))
]

let inj_test_cases : (regex * char * value * value) list = [
  (
    Seq(Alt(Char 'a', Seq(Char 'a', Char 'b')), Alt(Char 'b', One)), 'a', Pair(Right(Pair(Empty, Literal 'b')), Right(Empty)),
    Pair(Right(Pair(Literal 'a', Literal 'b')), Right(Empty))
  )
]

let lex_test_cases : (regex * char list * string) list = [
  (Char 'a' ||| Zero ** Char 'b',         ['a'], "a");
  ((Zero ** Char 'b' ||| Char 'a'),       ['a'], "a");
  (Star(Char 'a'),                        ['a'], "a");
  (Star(Seq(Char 'a', Char 'b')),         ['a';'b';'a';'b'], "abab");
  (Star(Alt(Seq(Char 'a', Char 'b'), Seq(Char 'd', Char 'e'))), ['a';'b';'d';'e';'a';'b'], "abdeab");
  (Star One,                              [], "");
  (Star(Char 'a'),                        [], "");
  (Star(Char 'a'),                        ['a'; 'a'; 'a'], "aaa");
]


let env_test_cases: (regex * char list * (string * string) list) list = [
  (Recd("Nonsense", Star(Char 'a')), ['a';'a';'a';], [("Nonsense", "aaa")]);
]

let run_env_test_cases() = 
  List.iter(fun (r, cl, toks) -> 
    let result = parse r cl in
    if result = toks then
      Printf.printf "Pass, tokenized %s" @@ c_str cl
    else (
      Printf.printf "FAIL: Expected env to be:\n";
      display_env toks;
      Printf.printf "But found this instead: \n";
      display_env result;)
    ) env_test_cases

let run_tests_lex() = 
  List.iter(fun (r, s, m) ->
    Printf.printf "\n\nNew Test\n";
    let lexed = lex r s in
    let res = s_str (flatten lexed) in
    if res = m then
      Printf.printf "Pass %s \n" res
    else
      Printf.printf "FAIL \n";
      Printf.printf "Expected %s to match %s\n" res m 
    ) lex_test_cases

let run_tests_inj() = 
  List.iter(fun (r, c, vs, vr) ->
      let res = inj r c vs in
      if res = vr then
        Printf.printf "Pass"
      else
        Printf.printf "Failed, inj resulted in %s when %s was expected" (display_value res) (display_value vr)
    ) inj_test_cases


let run_tests_mkEps() = 
  List.iter (fun (regex, parseTree) -> 
    if (mkEps regex) = parseTree then
      Printf.printf "Pass"
    else
      Printf.printf "Failed, generated %s when %s was expected" (display_value (mkEps regex)) (display_value parseTree)
  ) mkEps_test_cases

let run_tests_match () =
  List.iter (fun (regex, input, should_match) ->
    let result = matches regex input in
    if result = should_match then
      Printf.printf "Test passed for input: %s\n" (String.of_seq (List.to_seq input))
    else
      Printf.printf "\n---------------\nTest failed for input: %s\n----------------\n" (String.of_seq (List.to_seq input))
  ) regex_test_cases

let run_tests_simp () =
  List.iter (fun (input, expected_output) ->
    let result = simp input in
    if result = expected_output then
      Printf.printf "Test passed for input\n"
    else
      Printf.printf "Test failed for input: \n %s simplified to %s instead of %s" (display_regex input) (display_regex result) (display_regex expected_output)
  ) simp_test_cases

let verbose = ref false

let () =
  run_tests_match ();
  run_tests_simp ();
  run_tests_mkEps ();
  run_tests_inj ();
  run_tests_lex ();
  run_env_test_cases ();