type ord = Lt | Eq | Gt | Undef

(* Describes our operators and their precedence *)
let lookup_op = function
| `Lo -> 0
| `Eq -> 1
| `Lt | `Gt -> 2
| `Sum -> 3
| `Prod -> 4
| `Pfx -> 5
| `Call -> 6

let lookup_prec = Tok.(function
| Eq -> `Eq
| NotEq -> `Eq
| Lt -> `Lt
| Gt -> `Gt
| Plus -> `Sum
| Minus -> `Sum
| Slash -> `Prod
| Asterisk -> `Prod
| _ -> `Lo)

let ( <=> ) l r =
  match compare (lookup_op l) (lookup_op r) with
  | -1 -> Lt
  | 0 -> Eq
  | 1 -> Gt
  | _ -> Undef

type t = stmt Seq.t [@polyprinter Format.pp_print_seq]
[@@deriving show]

and stmt =
| Let of let_stmt
| Return of expr
| Expr of expr
| EmptyStmt
[@@deriving show]

and let_stmt = Tok.t * expr
[@@deriving show]

and unchecked_stmt = (stmt, string) result
[@@deriving show]

and unchecked_t = unchecked_stmt list
[@@deriving show]

and expr =
| Ident of Tok.t
| Const of Tok.t
| Pfx of Tok.t * expr
| Ifx of expr * Tok.t * expr
| Call of fn_args * t
| EmptyExpr (* this allows for post and prefix notation, e.g. + 0 = (Plus (Empty, Const (Int 0)) ) *)
[@@deriving show]

and fn_args = expr Seq.t [@polyprinter Format.pp_print_seq]

and unchecked_expr = (expr, string) result
[@@deriving show]

type prefix_parse_fn = unit -> expr
type infix_parse_fn = expr -> expr

let rec parse_expr lex prec () =
  let lhs_expr, lex = match Tok.peek lex with
  | Some t -> (
    match lookup_prefix_parse_fn t with
    | Some pfx -> pfx lex
    | None -> Error (Printf.sprintf "No prefix parse function for `%s'" (Tok.show t)), Seq.drop 1 lex
  )
  | None -> Ok EmptyExpr, lex in

  let rec loop lex lhs =
    let peek = Tok.peek lex in
    if Option.fold ~none:false ~some:(fun t -> (t <> Tok.SemiColon) && (prec <=> (lookup_prec t) == Lt)) peek then
      let t = Option.get peek in
      let lhs, lex = match lookup_infix_parse_fn t with
      | Some ifx -> (
        match lhs with
        | Error _ as e -> e, lex
        | Ok lhs' -> ifx lex lhs'
      )
      | None -> Error (Printf.sprintf "No infix parse function for `%s'" (Tok.show t)), Seq.drop 1 lex in
      loop lex lhs 
    else lhs, lex in

  loop (fun () -> lex ()) lhs_expr

and parse_prefix lex =
  let open Seq in
  match lex () with
  | Cons (tok, lex) -> (
    let expr', lex = parse_expr lex `Pfx () in
    match expr' with
    | Error _ as e -> e, lex
    | Ok expr' -> Ok (Pfx (tok, expr')), lex
  )
  | _ -> Error "Syntax error while parsing identifier", fun () -> lex ()

and parse_infix lex lhs =
  let x, l = Tok.peek lex |> Option.fold ~none:(Error "Parsing infix expression", lex) ~some:(fun tok ->
    let expr', lex = parse_expr (Seq.drop 1 lex) (lookup_prec tok) () in
    match expr' with
    | Error _ as e -> e, lex
    | Ok expr' -> Ok (Ifx (lhs, tok, expr')), lex
  ) in
  x, l

and lookup_prefix_parse_fn = Tok.(function
| Ident _ -> Some parse_ident
| Int _ -> Some parse_int
| Bang | Minus -> Some parse_prefix
| _ -> None)

and lookup_infix_parse_fn = Tok.(function
| Plus | Minus | Slash | Asterisk | Eq | NotEq | Lt | Gt -> Some parse_infix
| _ -> None)

and parse_ident lex =
  let open Seq in
  match lex () with
  | Cons (Tok.Ident _ as id, lex) -> Ok (Ident id), lex
  | _ -> Error "Syntax error while parsing identifier", fun () -> lex ()

and parse_int lex =
  let open Seq in
  match lex () with
  | Cons (Tok.Int _ as int', lex) -> Ok (Const int'), lex
  | _ -> Error "Syntax error while parsing identifier", fun () -> lex ()

(* let <ident> = <expr>; *)
let parse_let_stmt lex =
  match Util.find_advance_buf ((=) Tok.SemiColon) lex with
  | Some _, lex, visited -> (
    match visited with
    | Tok.Ident _ as id :: Tok.Assign :: expr' -> (
      let parsed_expr = parse_expr (List.to_seq expr') `Lo () in
      match parsed_expr with (* If the expr failed to parse, use this as the resolved ast node, otherwise insert the expr beneath a Let node *)
      | Error _ as e, _ -> e, lex
      | Ok expr', _ -> Ok (Let (id, expr')), lex
    )
    | Tok.Ident _ :: invalid_token :: _  -> Error (Printf.sprintf "Invalid token %s, expected `='" (Tok.show invalid_token)), lex
    | invalid_token :: _ -> Error (Printf.sprintf "Invalid token %s, expect identifier" (Tok.show invalid_token)), lex
    | [] -> Error "Unknown syntax error", lex
  )
  | None, lex, _ -> Error "Reached Eof while looking for `;'", lex

(* return <expr>; *)
let parse_return_stmt lex =
  match Util.find_advance_buf ((=) Tok.SemiColon) lex with
  | Some _, lex, visited -> (
    let parsed_expr = parse_expr (List.to_seq visited) `Lo () in
    match parsed_expr with
    | Error _ as e, _ -> e, lex
    | Ok expr', _ -> Ok (Return expr'), lex
  )
  | None, lex, _ -> Error "Unknown Syntax error", lex

(* <expr> *)
let parse_expr_stmt lex =
  let expr, lex = parse_expr lex `Lo () in
  let lex = Seq.drop_while ((=) Tok.SemiColon) lex in

  match expr with
  | Error _ as e -> e, lex
  | Ok expr' -> Ok (Expr expr'), lex

let rec parse lex () =
  let open Seq in
  let old_lex = lex in
  match lex () with
  | Cons (tok, lex) ->
    let stmt, lex = match tok with
    | Tok.Let -> parse_let_stmt lex
    | Tok.Return -> parse_return_stmt lex
    | Tok.Eof -> Ok (EmptyStmt), lex
    | _ -> parse_expr_stmt old_lex
    in Cons (stmt, parse lex)
  | Nil -> Nil

let%test "let statments" =
  let chars = {|
    let x = 5;
    let y = 10;
    let foobar = 838383;
  |} in

  let ast = chars
  |> String.to_seq
  |> Tok.next_token
  |> parse
  |> List.of_seq in

  let tests = [
    Ok (Let (Tok.Ident "x", Const (Int 5)));
    Ok (Let (Tok.Ident "y", Const (Int 10)));
    Ok (Let (Tok.Ident "foobar", Const (Int 838383)));
    Ok EmptyStmt] in

  if ast <> tests then (
    Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t ast) (show_unchecked_t tests);
    false
  ) else true

let%test "return statments" =
  let chars = {|
    return 5;
    return 10;
    return 993322;
  |} in

  let ast = chars
  |> String.to_seq
  |> Tok.next_token
  |> parse
  |> List.of_seq in

  let tests = [
    Ok (Return (Const (Int 5)));
    Ok (Return (Const (Int 10)));
    Ok (Return (Const (Int 993322)));
    Ok EmptyStmt] in

  if ast <> tests then (
    Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t ast) (show_unchecked_t tests);
    false
  ) else true

let%test "identifier expression" =
  let chars = "foobar;" in

  let ast = chars
  |> String.to_seq
  |> Tok.next_token
  |> parse
  |> List.of_seq in

  let tests = [
    Ok (Expr (Ident (Ident "foobar")));
    Ok EmptyStmt] in

  if ast <> tests then (
    Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t ast) (show_unchecked_t tests);
    false
  ) else true

let%test "integer literal expression" =
  let chars = "5;" in

  let ast = chars
  |> String.to_seq
  |> Tok.next_token
  |> parse
  |> List.of_seq in

  let tests = [
    Ok (Expr (Const (Int 5)));
    Ok EmptyStmt] in

  if ast <> tests then (
    Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t ast) (show_unchecked_t tests);
    false
  ) else true

let%test "parsing prefix expression" =
  let tests = [
    ("!5;", [Ok (Expr (Pfx (Bang, Const (Int 5)))); Ok EmptyStmt]);
    ("-15;", [Ok (Expr (Pfx (Minus, Const (Int 15)))); Ok EmptyStmt]);
  ] in

  List.fold_left (fun acc (a, b) ->
    let a = a |> String.to_seq |> Tok.next_token |> parse |> List.of_seq in
    if not acc then false else
    if a <> b then (
      Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t a) (show_unchecked_t b);
      false
    ) else true
  ) true tests

let%test "parsing infix expression" =
  let tests = [
    ("5 + 5", [Ok (Expr (Ifx (Const (Int 5), Plus ,Const (Int 5)))); Ok EmptyStmt]);
    ("5 - 5", [Ok (Expr (Ifx (Const (Int 5), Minus, Const (Int 5)))); Ok EmptyStmt]);
    ("5 * 5", [Ok (Expr (Ifx (Const (Int 5), Asterisk, Const (Int 5)))); Ok EmptyStmt]);
    ("5 / 5", [Ok (Expr (Ifx (Const (Int 5), Slash, Const (Int 5)))); Ok EmptyStmt]);
    ("5 > 5", [Ok (Expr (Ifx (Const (Int 5), Gt, Const (Int 5)))); Ok EmptyStmt]);
    ("5 < 5", [Ok (Expr (Ifx (Const (Int 5), Lt, Const (Int 5)))); Ok EmptyStmt]);
    ("5 == 5", [Ok (Expr (Ifx (Const (Int 5), Eq, Const (Int 5)))); Ok EmptyStmt]);
    ("5 != 5", [Ok (Expr (Ifx (Const (Int 5), NotEq, Const (Int 5)))); Ok EmptyStmt]);
  ] in

  List.fold_left (fun acc (a, b) ->
    let a = a |> String.to_seq |> Tok.next_token |> parse |> List.of_seq in
    if not acc then false else
    if a <> b then (
      Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t a) (show_unchecked_t b);
      false
    ) else true
  ) true tests

let%test "operator precedence parsing" =
  let tests = [
    ("-a * b", [Ok (Expr (Ifx (Pfx (Minus, Ident (Ident "a")), Asterisk, Ident (Ident "b")))); Ok EmptyStmt]);
    ("!-a", [Ok (Expr (Pfx (Bang, Pfx (Minus, Ident (Ident "a"))))); Ok EmptyStmt]);
    ("a + b + c", [Ok (Expr (Ifx (Ifx (Ident (Ident "a"), Plus, Ident (Ident "b")), Plus, Ident (Ident "c")))); Ok EmptyStmt]);
    ("a + b + c", [Ok (Expr (Ifx (Ifx (Ident (Ident "a"), Plus, Ident (Ident "b")), Plus, Ident (Ident "c")))); Ok EmptyStmt]);
    ("a * b * c", [Ok (Expr (Ifx (Ifx (Ident (Ident "a"), Asterisk, Ident (Ident "b")), Asterisk, Ident (Ident "c")))); Ok EmptyStmt]);
    ("a * b / c", [Ok (Expr (Ifx (Ifx (Ident (Ident "a"), Asterisk, Ident (Ident "b")), Slash, Ident (Ident "c")))); Ok EmptyStmt]);
    ("a + b / c", [Ok (Expr (Ifx (Ident (Ident "a"), Plus, Ifx (Ident (Ident "b"), Slash, Ident (Ident "c"))))); Ok EmptyStmt]);
    ("a + b * c + d / e - f", [Ok (Expr (Ifx ((Ifx ((Ifx (Ident (Ident "a"), Plus, Ifx ((Ident (Ident "b")), Asterisk, Ident (Ident "c")))), Plus, Ifx ((Ident (Ident "d")), Slash, (Ident (Ident "e"))))), Minus, Ident (Ident "f")))); Ok EmptyStmt]);
    ("3 + 4; -5 * 5", [Ok (Expr (Ifx (Const (Int 3), Plus, Const (Int 4)))); Ok (Expr (Ifx (Pfx (Minus, Const (Int 5)), Asterisk, Const (Int 5)))); Ok EmptyStmt]);
    ("5 > 4 == 3 < 4", [Ok (Expr (Ifx (Ifx (Const (Int 5), Gt, Const (Int 4)), Eq, Ifx (Const (Int 3), Lt, Const (Int 4))))); Ok EmptyStmt]);
    ("5 < 4 != 3 > 4", [Ok (Expr (Ifx (Ifx (Const (Int 5), Lt, Const (Int 4)), NotEq, Ifx (Const (Int 3), Gt, Const (Int 4))))); Ok EmptyStmt]);
    ("3 + 4 * 5 == 3 * 1 + 4 * 5", [(Ok (Expr (Ifx ( (Ifx ((Const (Int 3)), Plus, (Ifx ((Const (Int 4)), Asterisk, (Const (Int 5)))))), Eq, (Ifx ( (Ifx ((Const (Int 3)), Asterisk, (Const (Int 1)))), Plus, (Ifx ((Const (Int 4)), Asterisk, (Const (Int 5)))))))))); Ok EmptyStmt]);
  ] in

  List.fold_left (fun acc (a, b) ->
    let a = a |> String.to_seq |> Tok.next_token |> parse |> List.of_seq in
    if not acc then false else
    if a <> b then (
      Printf.printf "Got %s,\nexpected %s\n" (show_unchecked_t a) (show_unchecked_t b);
      false
    ) else true
  ) true tests

