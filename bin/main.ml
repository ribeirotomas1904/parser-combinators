open Parser_combinators

type expression =
  | E_nat of int
  | E_variable of string
  | E_abstraction of { parameters : string list; body : expression list }
  | E_application of { function_ : expression; arguments : expression list }
  | E_if_then_else of {
      predicate : expression;
      then_branch : expression;
      else_branch : expression;
    }

let natural =
  string_of_char_list >> int_of_string >> (fun x -> E_nat x) <$> many1 digit

let identifier =
  List.cons <$> lower <*> many (lower <|> digit) |> map string_of_char_list

let keyword k =
  identifier >>= function
  | ident when ident = k -> return k
  | _ -> fail ""

let plus = char '+' *>* return "+"

let variable =
  identifier
  >>= (function
        | name when name = "fun" -> fail ""
        | name -> return name)
  <|> plus
  |> map (fun x -> E_variable x)

let parens parser =
  char '(' *>* many whitespace *>* parser *<* many whitespace *<* char ')'

let list element separator =
  List.cons <$> element <*> many (separator *>* element)

let empty_parens_list = parens @@ return []

let parens_list parser =
  empty_parens_list <|> parens @@ list parser @@ many1 whitespace

let rec expression input =
  natural <|> variable <|> if_then_else <|> application <|> abstraction
  |> run_parser input

and application input =
  let mapping function_ arguments = E_application { function_; arguments } in
  mapping <$> expression
  <*> many (many1 whitespace *>* expression)
  |> parens |> run_parser input

and abstraction input =
  let parameters = parens_list identifier in
  let mapping parameters body = E_abstraction { parameters; body } in
  mapping
  <$> keyword "fun" *>* many1 whitespace *>* parameters
  <*> many1 whitespace *>* list expression (many1 whitespace)
  |> parens |> run_parser input

and if_then_else input =
  let mapping predicate then_branch else_branch =
    E_if_then_else { predicate; then_branch; else_branch }
  in

  mapping
  <$> keyword "if" *>* many1 whitespace *>* expression
  <*> many1 whitespace *>* expression
  <*> many1 whitespace *>* expression
  |> parens |> run_parser input

let file = many whitespace *>* expression *<* many whitespace *<* eof
