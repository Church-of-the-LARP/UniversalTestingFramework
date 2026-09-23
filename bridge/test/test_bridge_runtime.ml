(* Unit tests for the utf_bridge runtime: JSON encoding/decoding and the typed
   bridge value layer.  The socket client is exercised end-to-end by the
   framework tests, not here. *)

module Json = Utf_bridge.Json
module Value = Utf_bridge.Value

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec at i j = j >= nl || (i + j < hl && haystack.[i + j] = needle.[j] && at i (j + 1)) in
  let rec scan i = i + nl <= hl && (at i 0 || scan (i + 1)) in
  nl = 0 || scan 0

(* [failwith] (rather than a fixed-arity Alcotest helper) keeps [failf]
   polymorphic so that it can be used in any position. *)
let failf fmt = Printf.ksprintf failwith fmt
let fail msg = failwith msg

(* Round-trip through the printer and the parser; comparing the printed forms
   also pins down the printed representation. *)
let check_roundtrip name json =
  let printed = Json.to_string json in
  match Json.of_string printed with
  | reparsed ->
      Alcotest.(check string) (name ^ ": round-trip") printed (Json.to_string reparsed)
  | exception Json.Parse_error msg ->
      failf "%s: printing %s did not round-trip: %s" name printed msg

let check_parse_error name input =
  match Json.of_string input with
  | parsed -> failf "%s: expected a parse error, got %s" name (Json.to_string parsed)
  | exception Json.Parse_error _ -> ()

(* Run [f] and return the message of the Error it raises, or fail the test. *)
let error_message name f =
  match f () with
  | _ -> fail (name ^ ": expected Utf_bridge.Error to be raised")
  | exception Utf_bridge.Error msg -> msg
  | exception e ->
      fail (name ^ ": expected Utf_bridge.Error, got " ^ Printexc.to_string e)

(* ------------------------------------------------------------------ *)
(* JSON                                                                *)
(* ------------------------------------------------------------------ *)

let test_json_roundtrips () =
  check_roundtrip "null" Json.Null;
  check_roundtrip "true" (Json.Bool true);
  check_roundtrip "false" (Json.Bool false);
  check_roundtrip "empty list" (Json.List []);
  check_roundtrip "empty object" (Json.Object []);
  check_roundtrip "nested" (Json.List [ Json.List [Json.Int 1; Json.Null]; Json.Object [ ("a", Json.List [Json.Bool true]); ("b", Json.String "x") ] ]);
  check_roundtrip "quotes/backslashes" (Json.String "a\"b\\c");
  check_roundtrip "newline and tab" (Json.String "a\nb\tc\r\b\012");
  check_roundtrip "raw utf8" (Json.String "héllo");
  check_roundtrip "emoji" (Json.String "😀");
  check_roundtrip "int 0" (Json.Int 0);
  check_roundtrip "int -3" (Json.Int (-3));
  check_roundtrip "float 2.5" (Json.Float 2.5);
  check_roundtrip "float -0.5" (Json.Float (-0.5));
  check_roundtrip "float 1e22" (Json.Float 1e22);
  check_roundtrip "float 2.0" (Json.Float 2.0);
  check_roundtrip "keys are escaped too" (Json.Object [ ("a\"b", Json.Int 1) ])

let test_json_printer () =
  Alcotest.(check string) "2.0 prints as 2.0" "2.0" (Json.to_string (Json.Float 2.0));
  Alcotest.(check string) "ints stay ints" "2" (Json.to_string (Json.Int 2));
  Alcotest.(check string) "nan" "NaN" (Json.to_string (Json.Float nan));
  Alcotest.(check string) "infinity" "Infinity" (Json.to_string (Json.Float infinity));
  Alcotest.(check string) "-infinity" "-Infinity" (Json.to_string (Json.Float neg_infinity));
  Alcotest.(check string) "quotes escaped" "\"a\\\"b\"" (Json.to_string (Json.String "a\"b"));
  Alcotest.(check string) "control chars escaped" "\"a\\u0001b\"" (Json.to_string (Json.String "a\001b"));
  Alcotest.(check string) "newline escaped" "\"a\\nb\"" (Json.to_string (Json.String "a\nb"));
  Alcotest.(check string) "utf8 stays raw" "\"héllo\"" (Json.to_string (Json.String "héllo"))

let test_json_escapes () =
  let expected = "a\nb" ^ "é" ^ "😀" in
  (match Json.of_string {|{"a\nb\u00e9\ud83d\ude00": null}|} with
  | Json.Object [ (k, Json.Null) ] ->
      Alcotest.(check string) "decoded key" expected k
  | other -> fail ("unexpected parse result: " ^ Json.to_string other));
  match Json.of_string {|"a\nb\u00e9\ud83d\ude00"|} with
  | Json.String s -> Alcotest.(check string) "decoded string" expected s
  | other -> fail ("unexpected parse result: " ^ Json.to_string other)

let test_json_parse_errors () =
  check_parse_error "unterminated object" "{";
  check_parse_error "truncated literal" "tru";
  check_parse_error "unterminated array" "[1,2";
  check_parse_error "trailing garbage" "1 x";
  check_parse_error "empty" "";
  check_parse_error "trailing garbage after object" "{} {}";
  check_parse_error "unquoted key" "{a:1}"

let test_json_numbers () =
  (match Json.of_string "2" with
  | Json.Int 2 -> ()
  | other -> failf "\"2\" should be Int 2, got %s" (Json.to_string other));
  (match Json.of_string "2.0" with
  | Json.Float f -> Alcotest.(check (float 0.0)) "\"2.0\" is a float" 2.0 f
  | other -> failf "\"2.0\" should be Float 2.0, got %s" (Json.to_string other));
  (match Json.of_string "  -3\n" with
  | Json.Int (-3) -> ()
  | other -> failf "\"-3\" should be Int -3, got %s" (Json.to_string other));
  (match Json.of_string "NaN" with
  | Json.Float f -> Alcotest.(check bool) "NaN" true (classify_float f = FP_nan)
  | other -> failf "NaN should parse as a float, got %s" (Json.to_string other));
  (match Json.of_string "Infinity" with
  | Json.Float f -> Alcotest.(check bool) "Infinity" true (f = infinity)
  | other -> failf "Infinity should parse as a float, got %s" (Json.to_string other));
  (match Json.of_string "-Infinity" with
  | Json.Float f -> Alcotest.(check bool) "-Infinity" true (f = neg_infinity)
  | other -> failf "-Infinity should parse as a float, got %s" (Json.to_string other));
  (* integer literals that do not fit in an int fall back to float *)
  (match Json.of_string "99999999999999999999999999" with
  | Json.Float _ -> ()
  | other -> failf "huge int should fall back to float, got %s" (Json.to_string other))

let test_json_member () =
  let obj = Json.of_string {|{"a": 1, "b": [true, "x"]}|} in
  (match Json.member "a" obj with
  | Some (Json.Int 1) -> ()
  | Some other -> failf "member a: unexpected %s" (Json.to_string other)
  | None -> failf "member a: not found");
  (match Json.member "b" obj with
  | Some (Json.List [ Json.Bool true; Json.String "x" ]) -> ()
  | Some other -> failf "member b: unexpected %s" (Json.to_string other)
  | None -> failf "member b: not found");
  Alcotest.(check bool) "missing member" true (Json.member "zzz" obj = None);
  Alcotest.(check bool) "member of a list" true (Json.member "a" (Json.List []) = None);
  Alcotest.(check bool) "member of a string" true (Json.member "a" (Json.String "a") = None);
  Alcotest.(check bool) "member of null" true (Json.member "a" Json.Null = None)

(* ------------------------------------------------------------------ *)
(* Values                                                              *)
(* ------------------------------------------------------------------ *)

let test_value_happy_path () =
  Alcotest.(check int) "to_int" 7 (Value.to_int ~context:"x" (Value.int 7));
  Alcotest.(check (float 0.0)) "to_float on float" 1.5 (Value.to_float ~context:"x" (Value.float 1.5));
  Alcotest.(check (float 0.0)) "to_float on int" 3.0 (Value.to_float ~context:"x" (Value.int 3));
  Alcotest.(check string) "to_string" "hi" (Value.to_string ~context:"x" (Value.string "hi"));
  Alcotest.(check bool) "to_bool" true (Value.to_bool ~context:"x" (Value.bool true));
  Alcotest.(check (list int)) "to_list" [ 1; 2 ]
    (Value.to_list ~context:"x" (fun v -> Value.to_int ~context:"x.item" v)
       (Value.list Value.int [ 1; 2 ]));
  Alcotest.(check (list int)) "to_list on empty" []
    (Value.to_list ~context:"x" (fun v -> Value.to_int ~context:"x.item" v)
       (Value.list Value.int []));
  Alcotest.(check string) "nested to_list" "[1,2]"
    (Json.to_string (Value.to_json (Value.list Value.int [ 1; 2 ])));
  Alcotest.(check string) "list of lists"
    (Value.list (fun l -> Value.list Value.int l) [ [ 1 ]; [ 2; 3 ] ]
    |> Value.to_json |> Json.to_string)
    "[[1],[2,3]]"

let test_value_errors () =
  let msg =
    error_message "to_int on float" (fun () ->
        Value.to_int ~context:"my context" (Value.float 1.0))
  in
  Alcotest.(check string) "to_int on float message" "my context: expected int, got float" msg;
  Alcotest.(check bool) "to_int on float mentions context" true (contains ~needle:"my context" msg);
  let msg =
    error_message "to_int on bool" (fun () ->
        Value.to_int ~context:"my context" (Value.bool true))
  in
  Alcotest.(check bool) "to_int on bool mentions context" true (contains ~needle:"my context" msg);
  Alcotest.(check bool) "to_int on bool mentions kinds" true
    (contains ~needle:"expected int, got bool" msg);
  let msg =
    error_message "to_list on non-list" (fun () ->
        Value.to_list ~context:"the context" (fun v -> v) (Value.int 1))
  in
  Alcotest.(check bool) "to_list mentions context" true (contains ~needle:"the context" msg);
  let msg =
    error_message "to_string on int" (fun () ->
        Value.to_string ~context:"ctx" (Value.int 1))
  in
  Alcotest.(check string) "to_string on int message" "ctx: expected string, got int" msg;
  ignore
    (error_message "to_bool on string" (fun () ->
         Value.to_bool ~context:"ctx" (Value.string "no")));
  ignore
    (error_message "to_float on string" (fun () ->
         Value.to_float ~context:"ctx" (Value.string "no")))

let test_value_json () =
  let roundtrip v =
    match Value.of_json (Value.to_json v) with
    | back -> Json.to_string (Value.to_json back) = Json.to_string (Value.to_json v)
    | exception e -> failf "of_json raised %s" (Printexc.to_string e)
  in
  Alcotest.(check bool) "int round-trip" true (roundtrip (Value.int 42));
  Alcotest.(check bool) "float round-trip" true (roundtrip (Value.float 0.5));
  Alcotest.(check bool) "string round-trip" true (roundtrip (Value.string "héllo"));
  Alcotest.(check bool) "bool round-trip" true (roundtrip (Value.bool false));
  Alcotest.(check bool) "list round-trip" true (roundtrip (Value.list Value.int [ 1; 2 ]));
  Alcotest.(check bool) "nested list round-trip" true
    (roundtrip (Value.list (fun l -> Value.list Value.int l) [ [ 1 ]; [ 2 ] ]));
  Alcotest.(check string) "to_json of int" "42"
    (Json.to_string (Value.to_json (Value.int 42)));
  Alcotest.(check bool) "of_json List" true
    (Value.of_json (Json.List [ Json.Int 1 ]) = Value.list Value.int [ 1 ]);
  (match Value.of_json Json.Null with
  | _ -> failf "of_json null should raise"
  | exception Utf_bridge.Error msg ->
      Alcotest.(check string) "null message" "unexpected null in bridge value" msg);
  match Value.of_json (Json.Object []) with
  | _ -> failf "of_json object should raise"
  | exception Utf_bridge.Error msg ->
      Alcotest.(check string) "object message" "unexpected object in bridge value" msg

(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "utf_bridge"
    [
      ( "json",
        [
          ("round-trips", `Quick, test_json_roundtrips);
          ("printer", `Quick, test_json_printer);
          ("escapes", `Quick, test_json_escapes);
          ("parse errors", `Quick, test_json_parse_errors);
          ("numbers", `Quick, test_json_numbers);
          ("member", `Quick, test_json_member);
        ] );
      ( "value",
        [
          ("happy paths", `Quick, test_value_happy_path);
          ("conversion errors", `Quick, test_value_errors);
          ("json round-trip", `Quick, test_value_json);
        ] );
    ]
