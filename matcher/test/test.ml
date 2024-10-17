open Matcher.Regex
open Matcher.Formatter
open Matcher.Core

(* 

Test Suite for functions in the Core module

All test cases are of format [(...inputs, expected_output)]
and test runners are of format run_tests_{func_name}

To add a test case, simply append a tuple to any of the test case lists, with the inputs and expected output

dune run will 


*)

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
  (Opt(Char 'a'),                    ['a'], true);
  (Opt(Char 'a'),                    [], true);
  (Opt(Seq(Char 'a', Char 'b')),     [], true);
  (Opt(Seq(Char 'a', Char 'b')),     ['a'], false);
  (Opt(Seq(Char 'a', Char 'b')),     ['a';'b'], true);
]
let run_tests_match () =
  List.iter (fun (regex, input, should_match) ->
    let result = matches regex input in
    if result = should_match then
      Printf.printf "Pass: Correct for matching %s to %s\n" (display_regex regex) (c_str input)
    else
      Printf.printf "Fail: Expected match '%s' against '%s' to be '%s' but matcher returned '%s'\n" (display_regex regex) (c_str input) (b_str should_match) (b_str result)
  ) regex_test_cases


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
let run_tests_simp () =
  List.iter (fun (input, expected_output) ->
    let result = simp input in
    if result = expected_output then
      Printf.printf "Pass: Correctly simplified %s\n" (display_regex input)
    else
      Printf.printf "Fail: %s simplified to %s instead of %s\n" (display_regex input) (display_regex result) (display_regex expected_output)
  ) simp_test_cases


let mkEps_test_cases : (regex * value) list = [
  (Alt(Seq(Alt(Zero, Alt(Zero**Char 'b', One)), Alt(Char 'b', One)), Alt(One, Zero)), Left(Pair(Right(Right(Empty)), Right(Empty))))
]
let run_tests_mkEps() = 
  List.iter (fun (regex, parseTree) -> 
    if (mkEps regex) = parseTree then
      Printf.printf "Pass: mkEps correct for %s\n" (display_regex regex)
    else
      Printf.printf "Fail: mkEps generated %s when %s was expected\n" (display_value (mkEps regex)) (display_value parseTree)
  ) mkEps_test_cases



let inj_test_cases : (regex * char * value * value) list = [
  (
    Seq(Alt(Char 'a', Seq(Char 'a', Char 'b')), Alt(Char 'b', One)), 'a', Pair(Right(Pair(Empty, Literal 'b')), Right(Empty)),
    Pair(Right(Pair(Literal 'a', Literal 'b')), Right(Empty))
  )
]
let run_tests_inj() = 
  List.iter(fun (r, c, vs, vr) ->
      let res = inj r c vs in
      if res = vr then
        Printf.printf "Pass: Correctly injected %s into %s \n" (c_str [c]) (display_value vs)
      else
        Printf.printf "Fail: inj resulted in %s when %s was expected\n" (display_value res) (display_value vr)
    ) inj_test_cases



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
let run_tests_lex() = 
  List.iter(fun (r, s, m) ->
    let lexed = lex r s in
    let res = s_str (flatten lexed) in
    if res = m then
      Printf.printf "Pass: lexed %s \n" res
    else
      Printf.printf "Fail: Expected %s to match %s\n" res m 
    ) lex_test_cases



let env_test_cases: (regex * char list * (string * string) list) list = [
  (Recd("Nonsense", Star(Char 'a')), ['a';'a';'a';], [("Nonsense", "aaa")]);
]

let run_tests_env() = 
  List.iter(fun (r, cl, toks) -> 
    let result = parse r cl in
    if result = toks then
      Printf.printf "Pass: tokenized %s\n" @@ c_str cl
    else (
      Printf.printf "Fail: Expected env to be:\n";
      display_env toks;
      Printf.printf "But found this instead: \n";
      display_env result;)
    ) env_test_cases

let run_suite() =
  run_tests_env ();
  run_tests_inj ();
  run_tests_lex ();
  run_tests_lex ();
  run_tests_match ();
  run_tests_mkEps ();
  run_tests_simp ()

let () = 
    run_suite ()