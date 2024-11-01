open Regex

let rec cl_to_rexp cl = match cl with
 | c::cs -> if cs = [] then Char c else Char c ** (cl_to_rexp cs)
 | _ -> One

let (~-) str =
 let chars = List.init (String.length str) (String.get str) in
 cl_to_rexp chars

let f_letter = fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let f_digit = fun c -> (c >= '0' && c <= '9')
let f_symbol = fun c -> String.contains "._><=;:,\\" c
let f_paren = fun c -> String.contains "({})" c
let f_whitespace = fun c -> String.contains " \t\n\r" c

let r_keyword = ~-"while" ||| ~-"or" ||| ~-"then" ||| ~-"else" ||| ~-"do" ||| ~-"for" 
  ||| ~-"to" ||| ~-"true" ||| ~-"false" ||| ~-"read" ||| ~-"write" ||| ~-"skip"
let r_op = ~-"+" ||| ~-"-" ||| ~-"*" ||| ~-"%" ||| ~-"/" ||| ~-"==" ||| ~-"!="
  ||| ~-"<" ||| ~-">" ||| ~-"<=" ||| ~-">=" ||| ~-":=" ||| ~-"&&" ||| ~-"||"

let r_letter = CFun ("f_l", f_letter)
let r_digit = CFun ("f_d", f_digit)
let r_symbol = CFun("f_sb", fun c -> f_letter c || f_digit c || f_symbol c)
let r_paren = CFun ("f_p", f_paren)
let r_semi = Char ';'
let r_whitespace = CFun("ws", f_whitespace) 

let r_identifier = r_letter ** Star(CFun("f_id", fun c -> f_letter c || f_digit c) ||| Char '_')
let r_number = CFun("f_n", fun c -> (c >= '1' && c <= '9')) ** r_digit
let r_str = Char '"' ** Star(CFun("f_s", fun c -> f_letter c || f_symbol c || f_digit c || f_paren c || f_whitespace c)) ** Char '"'
let r_comment = ~-"//" ** Star(Char(' ') ||| CFun("f_com", fun c -> f_letter c || f_symbol c || f_paren c || f_digit c)) ** Char '\n'

let while_syntax = Star(
  Recd("k", r_keyword) |||
  Recd("p", r_paren) |||
  Recd("s", r_semi) |||
  Recd("w", r_whitespace) |||
  Recd("i", r_identifier) |||
  Recd("symb", r_symbol) |||
  Recd("n", r_number) |||
  Recd("str", r_str) |||
  Recd("c", r_comment) |||
  Recd("o", r_op)
)