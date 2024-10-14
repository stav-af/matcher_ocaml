type regex = 
  | Zero
  | One
  | Char of char
  | Seq of regex * regex
  | Alt of regex * regex
  | Star of regex

(* Syntactic sugar for our expressions *)
let ( ** ): regex -> regex -> regex = fun r1 r2 -> Seq (r1, r2)
let ( ||| ): regex -> regex -> regex = fun r1 r2 -> Alt (r1, r2)
let star: regex -> regex = fun r -> Star r

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



let rec nullable(r: regex) = 
  match r with
  | Zero | Char(_) -> false
  | One  | Star(_) -> true
  | Seq(r1, r2) -> nullable r1 && nullable r2
  | Alt(r1, r2) -> nullable r1 || nullable r2


let rec der(r: regex) (c: char) =
  match r with
  | Zero | One -> Zero
  | Char(x) -> if x = c then One else Zero
  | Seq(r1, r2) -> 
    if nullable r1
    then (der r1 c ** r2) ||| der r2 c
    else der r1 c ** r2
  | Alt(r1, r2) -> der r1 c ||| der r2 c
  | Star(r) -> der r c ** r

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
  | r -> r

let rec matches (r: regex) (cs: char list) =
  match cs with
  | [] -> nullable r
  | c::s -> matches (simp (der r c)) s


let regex_test_cases : (regex * char list * bool) list = [
  (Char 'a',                        ['a'], true);
  (Char 'a' ** Char 'b',            ['a'; 'b'], true);
  (star (Char 'a') ** Char 'b',     ['a'; 'a'; 'b'], true);
  (Char 'a' ||| Char 'b',           ['a'], true);
  (Char 'a' ||| Char 'b',           ['c'], false);
  (Char 'a' ** star (Char 'b'),     ['a'; 'b'; 'b'], true);
  (Char 'a' ** Char 'b',            ['a'; 'b'; 'b'], false);
  (Char 'a' ||| Zero ** Char 'b',   ['a'], true);
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

let run_tests () =
  List.iter (fun (regex, input, should_match) ->
    let result = matches regex input in
    if result = should_match then
      Printf.printf "Test passed for input: %s\n" (String.of_seq (List.to_seq input))
    else
      Printf.printf "Test failed for input: %s\n" (String.of_seq (List.to_seq input))
  ) regex_test_cases

let run_tests_simp () =
  List.iter (fun (input, expected_output) ->
    let result = simp input in
    if result = expected_output then
      Printf.printf "Test passed for input\n"
    else
      Printf.printf "Test failed for input: \n %s simplified to %s instead of %s" (display_regex input) (display_regex result) (display_regex expected_output)
  ) simp_test_cases

let () =
  run_tests ();
  run_tests_simp ()