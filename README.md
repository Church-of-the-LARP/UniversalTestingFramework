# UTF - Universal Testing Framework

**WIP**: This is a WIP repo, still just laying things out.

Not really a framework at all, an ocaml ppx system to support writing tests for
many languages on a write once, run everywhere basis in the context of
predefined functions to test.

Specifically made for the QuitLARP academic project to be able to test many
languages for one assignment in a probing manner.

## How it works

```
OCaml signature (source of truth)
  module Spec : sig val add : a:int -> b:int -> int end = struct end [@@utest]
       |
       |  ppx expansion (utf_ppx)
       v
  bridge bindings ------------------------------> spec_server.py
  (utf_bridge runtime client)   unix socket,       (wrapper server, generated)
                                one JSON object          | imports
                                per line                 v
                                                spec_functions.py
                                                (your implementation)
```

- The `[@@utest]` module's body has to be empty: the PPX replaces it with one
  function per `val`, each of which marshals its arguments, calls the service
  over a unix socket and unmarshals the result. The signature stays in place as
  a module constraint, so the generated bindings are type-checked against it.
- `<service>_server.py` is generated. It validates the implementation against
  the signature at startup (names, order, annotations), type-checks every
  request and dispatches to `<service>_functions.py`.
- `<service>_functions.py` is yours. The generator creates it when missing and
  never overwrites it unless `--force` is passed.
- The first bridge call spawns the wrapper server automatically and kills it
  when the test process exits.

## Workflow

1. Write the spec, implementing nothing:

   ```ocaml
   module Spec : sig
     val add : a:int -> b:int -> int
     val names : n:int -> string list
   end = struct end [@@utest]
   ```

2. Generate the python side (next to the `.ml` file):

   ```
   dune exec utf -- gen python test/test_universal_testing_framework.ml
   ```

3. Implement `spec_functions.py`. Keep the parameter names, order and type
   annotations: the server checks them and refuses to start on drift.

4. Run the tests. The server is spawned automatically:

   ```
   dune runtest
   ```

`test/test_universal_testing_framework.ml` is a working example covering ints,
floats, bools, strings (utf-8 and escapes), lists, nested lists, positional
parameters, remote exceptions and the typed boundary.

### Try the proof of concept

`examples/poc/` is a runnable demo: `poc.ml` declares three functions, and
`poc_functions.py` implements them in python. `poc_server.py` is the generated
wrapper server - the two python files stay separate so regenerating the glue
never touches your implementation.

```
dune exec utf -- gen python examples/poc/poc.ml   # writes poc_server.py, keeps poc_functions.py
dune build @examples/poc/runtest                  # spawns the server and runs the demo
```

Running the binary directly works too, as long as `poc_server.py` is in the
current directory (or `UTF_SERVER` points at it):

```
(cd examples/poc && dune exec ./poc.exe)
UTF_SERVER=examples/poc/poc_server.py dune exec examples/poc/poc.exe
```

To see exactly what the PPX expanded the signature into:

```
dune describe pp examples/poc/poc.ml
```

## Types

Supported today: `int`, `float`, `string`, `bool` and (nested) lists of those.
Labelled arrows (`a:int -> ...`) give the parameter its name; unlabelled ones
become `arg1`, `arg2`, ... Optional arguments and other types are rejected at
compile time.

## The protocol

One JSON object per line over a unix domain socket, both directions:

```
request   {"service": "spec", "func": "add", "args": {"a": 1, "b": 2}}
success   {"ok": true,  "value": 3}
failure   {"ok": false, "error": "remote error: ValueError: boom"}
```

Values are booleans, numbers, strings and arrays. Error strings are passed
through to OCaml verbatim, as `Utf_bridge.Error`.

## Environment variables

- `UTF_SOCKET` - socket path (client and server). Defaults to a per-process
  path under the temp directory, so parallel test runs cannot collide.
- `UTF_SERVER` - wrapper server script, for clients that should not spawn the
  `<service>_server.py` sitting next to them.
- `UTF_PYTHON` - python 3 interpreter used to spawn the server (default
  `python3`).
- `UTF_DEBUG=1` - verbose client diagnostics, plus server tracebacks on stderr.

## Layout

- `core/` - IR, source extraction and validation, generators (`utf_core`).
- `ppx/` - the `[@@utest]` rewriter (`utf_ppx`). It never writes files.
- `bridge/` - the runtime client: JSON codec, typed values, socket +
  spawn/retry (`utf_bridge`).
- `bin/` - the `utf` CLI (`gen python`).
- `test/` - the end-to-end example and test suite.

The wrapper server also has a `--check` mode, which validates the
implementation against the signature and exits - handy in CI without starting
a server.
