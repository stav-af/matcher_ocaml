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
  | CFun of string * (char -> bool)

type value =
  | StarVal of value list
  | Empty
  | Literal of char
  | VSeq of value * value
  | Left of value
  | Right of value
  | NTVal of value * int
  | Rec of string * value


let ( ** ): regex -> regex -> regex = fun r1 r2 -> Seq (r1, r2)
let ( ||| ): regex -> regex -> regex = fun r1 r2 -> Alt (r1, r2)
let star: regex -> regex = fun r -> Star r

let clts: char list -> CharSet.t = fun chars -> List.fold_left (fun set c -> CharSet.add c set) CharSet.empty chars
let strtcl s = List.init (String.length s) (String.get s)
