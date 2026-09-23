(* Minimal JSON implementation for the utf_bridge protocol.

   Supported subset: null, booleans, numbers, strings, arrays and objects.
   Python's json module extensions NaN / Infinity / -Infinity are accepted when
   parsing.  Printing only escapes what has to be escaped ("\\", '"' and control
   characters < 0x20); non-ASCII bytes are emitted as raw UTF-8.

   Note on style: every [if]/[match] whose branch is followed by a [;] is
   explicitly parenthesised, so that no sequencing [;] can be swallowed by the
   last branch. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list

exception Parse_error of string

(* ------------------------------------------------------------------ *)
(* Printing                                                            *)
(* ------------------------------------------------------------------ *)

let add_escaped_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

(* [Float.to_string] may produce "2.", which is not valid JSON, and it produces
   "nan"/"inf" for non-finite values, which we replace by the Python
   spellings. *)
let float_to_string f =
  match classify_float f with
  | FP_nan -> "NaN"
  | FP_infinite -> if f > 0.0 then "Infinity" else "-Infinity"
  | _ ->
      let s = Float.to_string f in
      let n = String.length s in
      if n > 0 && s.[n - 1] = '.' then s ^ "0" else s

let rec add buf = function
  | Null -> Buffer.add_string buf "null"
  | Bool true -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | Int i -> Buffer.add_string buf (string_of_int i)
  | Float f -> Buffer.add_string buf (float_to_string f)
  | String s -> add_escaped_string buf s
  | List l ->
      Buffer.add_char buf '[';
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_char buf ',';
          add buf v)
        l;
      Buffer.add_char buf ']'
  | Object fields ->
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char buf ',';
          add_escaped_string buf k;
          Buffer.add_char buf ':';
          add buf v)
        fields;
      Buffer.add_char buf '}'

let to_string t =
  let buf = Buffer.create 128 in
  add buf t;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Parsing                                                             *)
(* ------------------------------------------------------------------ *)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let hex_value c =
  if is_digit c then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else Char.code c - Char.code 'A' + 10

let add_utf8 buf code =
  if code < 0x80 then Buffer.add_char buf (Char.chr code)
  else if code < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end
  else if code < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end
  else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (code lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end

(* Parse errors carry the offending offset. *)
let of_string s =
  let n = String.length s in
  let pos = ref 0 in
  let fail msg =
    raise (Parse_error (Printf.sprintf "%s at offset %d" msg !pos))
  in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let skip_ws () =
    while
      !pos < n
      && (match s.[!pos] with
         | ' ' | '\t' | '\n' | '\r' -> true
         | _ -> false)
    do
      incr pos
    done
  in
  let expect_literal lit =
    let len = String.length lit in
    if !pos + len > n || String.sub s !pos len <> lit then fail "invalid literal"
    else pos := !pos + len
  in
  let parse_hex4 () =
    if !pos + 4 > n then (fail "truncated \\u escape");
    let code = ref 0 in
    for _ = 1 to 4 do
      let c = s.[!pos] in
      (if not (is_hex c) then fail "invalid \\u escape");
      code := (!code * 16) + hex_value c;
      incr pos
    done;
    !code
  in
  let parse_string () =
    (* the opening quote has already been consumed *)
    let buf = Buffer.create 16 in
    let rec loop () =
      if !pos >= n then (fail "unterminated string");
      let c = s.[!pos] in
      incr pos;
      if c = '"' then Buffer.contents buf
      else if c = '\\' then begin
        if !pos >= n then (fail "unterminated escape");
        let e = s.[!pos] in
        incr pos;
        (match e with
        | '"' -> Buffer.add_char buf '"'
        | '\\' -> Buffer.add_char buf '\\'
        | '/' -> Buffer.add_char buf '/'
        | 'b' -> Buffer.add_char buf '\b'
        | 'f' -> Buffer.add_char buf '\012'
        | 'n' -> Buffer.add_char buf '\n'
        | 'r' -> Buffer.add_char buf '\r'
        | 't' -> Buffer.add_char buf '\t'
        | 'u' -> (
            let code = parse_hex4 () in
            if code >= 0xD800 && code <= 0xDBFF then begin
              (* high surrogate: try to combine with a following low surrogate *)
              if !pos + 1 < n && s.[!pos] = '\\' && s.[!pos + 1] = 'u' then
                begin
                  let save = !pos in
                  pos := !pos + 2;
                  let low = parse_hex4 () in
                  if low >= 0xDC00 && low <= 0xDFFF then
                    add_utf8 buf
                      (0x10000 + ((code - 0xD800) lsl 10) + (low - 0xDC00))
                  else begin
                    (* not a pair after all: replacement char *)
                    pos := save;
                    add_utf8 buf 0xFFFD
                  end
                end
              else add_utf8 buf 0xFFFD
            end
            else if code >= 0xDC00 && code <= 0xDFFF then add_utf8 buf 0xFFFD
            else add_utf8 buf code)
        | _ -> fail "invalid escape");
        loop ()
      end
      else begin
        Buffer.add_char buf c;
        loop ()
      end
    in
    loop ()
  in
  let parse_number () =
    let start = !pos in
    let is_number_char c =
      match c with
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
      | _ -> false
    in
    while !pos < n && is_number_char s.[!pos] do
      incr pos
    done;
    let text = String.sub s start (!pos - start) in
    let has_float_syntax =
      let found = ref false in
      String.iter
        (fun c ->
          match c with '.' | 'e' | 'E' -> found := true | _ -> ())
        text;
      !found
    in
    if has_float_syntax then
      match float_of_string_opt text with
      | Some f -> Float f
      | None ->
          pos := start;
          fail "invalid number"
    else
      match int_of_string_opt text with
      | Some i -> Int i
      | None -> (
          (* integer literal too large for [int]: fall back to a float *)
          match float_of_string_opt text with
          | Some f -> Float f
          | None ->
              pos := start;
              fail "invalid number")
  in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | None -> fail "unexpected end of input"
    | Some '{' -> (
        incr pos;
        skip_ws ();
        if peek () = Some '}' then begin
          incr pos;
          Object []
        end
        else begin
          let fields = ref [] in
          let rec members () =
            skip_ws ();
            if peek () <> Some '"' then (fail "expected a string key");
            incr pos;
            let key = parse_string () in
            skip_ws ();
            if peek () <> Some ':' then (fail "expected ':'");
            incr pos;
            let value = parse_value () in
            fields := (key, value) :: !fields;
            skip_ws ();
            match peek () with
            | Some ',' ->
                incr pos;
                members ()
            | Some '}' -> incr pos
            | Some _ -> fail "expected ',' or '}'"
            | None -> fail "unexpected end of input in object"
          in
          members ();
          Object (List.rev !fields)
        end)
    | Some '[' -> (
        incr pos;
        skip_ws ();
        if peek () = Some ']' then begin
          incr pos;
          List []
        end
        else begin
          let items = ref [] in
          let rec elements () =
            let v = parse_value () in
            items := v :: !items;
            skip_ws ();
            match peek () with
            | Some ',' ->
                incr pos;
                elements ()
            | Some ']' -> incr pos
            | Some _ -> fail "expected ',' or ']'"
            | None -> fail "unexpected end of input in array"
          in
          elements ();
          List (List.rev !items)
        end)
    | Some '"' ->
        incr pos;
        String (parse_string ())
    | Some 't' ->
        expect_literal "true";
        Bool true
    | Some 'f' ->
        expect_literal "false";
        Bool false
    | Some 'n' ->
        expect_literal "null";
        Null
    | Some 'N' ->
        expect_literal "NaN";
        Float nan
    | Some 'I' ->
        expect_literal "Infinity";
        Float infinity
    | Some '-' when !pos + 1 < n && s.[!pos + 1] = 'I' ->
        expect_literal "-Infinity";
        Float neg_infinity
    | Some ('-' | '0' .. '9') -> parse_number ()
    | Some c -> fail (Printf.sprintf "unexpected character %C" c)
  in
  skip_ws ();
  if !pos >= n then (raise (Parse_error "empty input"));
  let value = parse_value () in
  skip_ws ();
  if !pos <> n then
    (raise (Parse_error (Printf.sprintf "trailing garbage at offset %d" !pos)));
  value

(* ------------------------------------------------------------------ *)
(* Accessors                                                           *)
(* ------------------------------------------------------------------ *)

let member key = function
  | Object fields ->
      let rec find = function
        | [] -> None
        | (k, v) :: rest -> if String.equal k key then Some v else find rest
      in
      find fields
  | _ -> None
