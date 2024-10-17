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
