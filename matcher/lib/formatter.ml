open Regex

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
  | Any cset -> Printf.sprintf "Any{%s}"  (String.concat "," (CharSet.fold(fun c acc -> (String.make 1 c) :: acc) cset []))
  | Seq (r1, r2) -> 
      Printf.sprintf "(%s ** %s)" (display_regex r1) (display_regex r2)
  | Alt (r1, r2) -> 
      Printf.sprintf "(%s ||| %s)" (display_regex r1) (display_regex r2)
  | Star r -> Printf.sprintf "(%s)*" (display_regex r)
  | Recd (s, r) -> Printf.sprintf "Record(%s: %s)" s (display_regex r) 
  | NTimes (r, n) -> Printf.sprintf "Times(%s:{%d})" (display_regex r) n
  | Plus(r1) -> Printf.sprintf "%s+" (display_regex r1)
  | Opt(r1) -> Printf.sprintf "%s?" (display_regex r1)


let display_env ev = 
  List.iter (fun (x, s) -> Printf.printf "Found submatch (%s: %s)" x s) ev

let c_str (cs: char list) = 
  String.concat "" (List.map (String.make 1) cs)

let s_str (ss: string list) =
  String.concat "" ss

let b_str(b: bool) = 
  if b then "True" else "False"