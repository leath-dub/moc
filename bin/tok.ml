open Seq

let sym' fmt _ = Format.pp_print_string fmt
let sym a b c = sym' b c a

type t =
  | Illegal of char 
  | Eof

  (* Identifiers and literals *)
  | Ident of string
  | Int of int

  (* Operators *)
  | Assign [@printer sym "="]
  | Plus [@printer sym "+"]
  | Minus [@printer sym "-"]
  | Bang [@printer sym "!"]
  | Asterisk [@printer sym "*"]
  | Slash [@printer sym "/"]

  | Lt [@printer sym "<"]
  | Gt [@printer sym ">"]

  | Eq [@printer sym "=="]
  | NotEq [@printer sym "!="]

  (* Delimiters *)
  | Comma [@printer sym ","]
  | SemiColon [@printer sym ";"]

  | LParen [@printer sym "("]
  | RParen [@printer sym ")"]
  | LBrace [@printer sym "{"]
  | RBrace [@printer sym "}"]

  (* Keywords *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show]

let lookup_ident = function
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | str -> Ident str

let is_letter = function
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let read_letters lex =
  let name = Util.take_until is_letter lex in
  name, lex |> drop @@ String.length name

let read_ident_or_keyword lex =
  let (str, lex) = read_letters lex in
  lookup_ident str, lex

let read_integer lex =
  let integer_name = Util.take_until is_digit lex in
  let integer = int_of_string integer_name in
  Int integer, lex |> drop @@ String.length integer_name

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let munch_whitespace =
  drop_while is_whitespace

let peek lex =
  match Seq.uncons lex with
  | Some (ch, _) -> Some ch
  | None -> None

let rec next_token chars () =
  let chars = munch_whitespace chars in
  match chars () with
  | Cons (ch, lex) ->
    let tok, lex = match ch with
    | '=' -> (
      match peek lex with
      | Some '=' -> Eq, drop 1 lex
      | _ -> Assign, lex
    )
    | ';' -> SemiColon, lex
    | '(' -> LParen, lex
    | ')' -> RParen, lex
    | ',' -> Comma, lex
    | '+' -> Plus, lex
    | '!' -> (
      match peek lex with
      | Some '=' -> NotEq, drop 1 lex
      | _ -> Bang, lex
    )
    | '*' -> Asterisk, lex
    | '-' -> Minus, lex
    | '<' -> Lt, lex
    | '>' -> Gt, lex
    | '/' -> Slash, lex
    | '{' -> LBrace, lex
    | '}' -> RBrace, lex
    | ch when is_letter ch -> read_ident_or_keyword chars
    | ch when is_digit ch -> read_integer chars
    | ch -> Illegal ch, lex in
    Cons (tok, next_token lex)
  | Nil -> Cons (Eof, fun () -> Nil)

let%test "next_token" =
  let chars = String.to_seq {|
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;
  |} in

  let tests = List.to_seq [
    Let; Ident "five"; Assign; Int 5; SemiColon;
    Let; Ident "ten"; Assign; Int 10; SemiColon;
    Let; Ident "add"; Assign;
    Function; LParen; Ident "x"; Comma; Ident "y"; RParen;
    LBrace;
    Ident "x"; Plus; Ident "y"; SemiColon;
    RBrace; SemiColon;
    Let; Ident "result"; Assign; Ident "add";
    LParen;
    Ident "five"; Comma; Ident "ten";
    RParen; SemiColon;
    Bang; Minus; Slash; Asterisk; Int 5; SemiColon;
    Int 5; Lt; Int 10; Gt; Int 5; SemiColon;
    If; LParen; Int 5; Lt; Int 10; RParen; LBrace;
    Return; True; SemiColon;
    RBrace; Else; LBrace;
    Return; False; SemiColon;
    RBrace;
    Int 10; Eq; Int 10; SemiColon;
    Int 10; NotEq; Int  9; SemiColon;
    Eof
  ] in

  Seq.fold_left2 (fun b t1 t2 ->
    match b with
    | false -> false
    | true -> if not (t1 = t2) then
      (Printf.printf "%s != %s\n" (show t1) (show t2); false)
    else t1 = t2
  ) true tests @@ next_token chars
