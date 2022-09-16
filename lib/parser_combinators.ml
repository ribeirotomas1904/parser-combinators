let ( >> ) f g x = g (f x)

type 'a parser = string -> ('a * string, string) result

let return value : 'a parser = fun input -> Ok (value, input)
let fail message : 'a parser = fun _ -> Error message
let run_parser input parser = parser input

let bind (parser : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun input -> Result.bind (parser input) (fun (value, input') -> f value input')

let ( >>= ) = bind

let map (f : 'a -> 'b) (parser : 'a parser) : 'b parser =
  parser >>= fun value -> return (f value)

let ( <$> ) = map
let ( <*> ) parser_a parser_b = parser_a >>= fun f -> f <$> parser_b

let choice (parser_a : 'a parser) (parser_b : 'a parser) : 'a parser =
 fun input ->
  match parser_a input with
  | Ok _ as result -> result
  | Error _ -> parser_b input

let ( <|> ) = choice

let rec many (parser : 'a parser) : 'a list parser =
  List.cons <$> parser <*> (fun input -> many parser input) <|> return []

let many1 parser = List.cons <$> parser <*> many parser

let just_left (parser_a : 'a parser) (parser_b : 'b parser) : 'a parser =
  Fun.const <$> parser_a <*> parser_b

let just_right (parser_a : 'a parser) (parser_b : 'b parser) : 'b parser =
  Fun.(flip const) <$> parser_a <*> parser_b

let ( *<* ) = just_left
let ( *>* ) = just_right

let any_char : char parser = function
  | "" -> Error ""
  | input ->
      let first = String.get input 0 in
      let rest = String.sub input 1 (String.length input - 1) in
      Ok (first, rest)

let char_predicate predicate =
  any_char >>= function
  | char when predicate char -> return char
  | _ -> fail ""

let char c = char_predicate (Char.equal c)

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_whitespace = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false

let lower = char_predicate is_lower
let upper = char_predicate is_upper
let digit = char_predicate is_digit
let letter = choice lower upper
let whitespace = char_predicate is_whitespace
let string_of_char_list = List.to_seq >> String.of_seq

let eof : unit parser = function
  | "" -> Ok ((), "")
  | _ -> Error ""

let string s : string parser = function
  | input when String.starts_with input ~prefix:s ->
      let prefix_length = String.length s in
      let input' =
        String.sub input prefix_length (String.length input - prefix_length)
      in
      Ok (s, input')
  | _ -> Error ""
