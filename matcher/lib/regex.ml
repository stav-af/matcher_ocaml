module CharSet = Set.Make(Char)

type regex = 
  | Zero
  | One
  | Char of char
  | Seq of regex * regex
  | Alt of regex * regex
  | Star of regex
  | NTimes of regex * int
  | Recd of string * regex
  | Any of CharSet.t
  | Opt of regex
  | Plus of regex

type value =
  | StarVal of value list
  | Empty
  | Literal of char
  | Pair of value * value
  | Left of value
  | Right of value
  | Rec of string * value

(* Syntactic sugar for our expressions *)
