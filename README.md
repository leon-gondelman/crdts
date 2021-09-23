# crdts
OCaml implementations of Conflict-Free Replicated Datatypes for Fun and Profit. (Distributed System Project Course)


# 1. Structure of the repo

The two main folder of the repository are `ml_sources/` and
`/Users/apollo/repositories/aneris/crdts/vendor/aneris/ml_sources/aneris_lang`.


- `ml_sources/`: This folder contains the reliable causal broadcast
  implementation `rcb/` and CRDT examples `rcb/crdt/...`.


 - `.../ml_sources/aneris_lang/`: This folder contains the libraries that are
  used for implementation of examples:
	  - `ast.ml` file contains core network utilities (sockets, receive/send,
        etc), manipulation of locks and strings.
	  - `lib/` contains library files for manipulation of basic data structures
	    such as lists, maps, and serialization of messages.

# 2. Setting up a new example.

To set a new example, create a folder `mkdir .../rcb/crdt/<new_folder_name>`.
Add file `touch .../rcb/crdt/<new_folder_name>/<new_example_code.ml>` for the
implementation of the example and add file
`touch .../rcb/crdt/<new_folder_name>/<new_example_runner.ml>` to run it.
Of course you can add more files, and organize example differently.

# 3. Compilation and testing.

To compile your code, run `dune build` at the root of the repository.
To run your code, run `dune exec ./executable_runner.exe <arguments of executable>`
for the folder where `executable_runner.ml` exists.

Example: `.../ml_sources/rcb/crdt/counter dune exec ./counter_runner.exe 0 1035 1023 1034`

You can also write a script like `.../ml_sources/rcb/crdt/counter/run.sh`
which will launch all needed terminals automatically.

# 4. Translation to AnerisLang (Coq) using o2a compiler. (Optional)

It is not required, but you might wish to see whether your OCaml sources compile to  AnerisLang programs
(if they do, then we will be able in the future to use your OCaml sources
directly to prove the corresponding examples in Coq).

To automatically generate AnerisLang programs from Ocaml source files, pin the `ocaml2lang` package:

    opam pin git+https://github.com/leon-gondelman/ocaml2lang

This will produce an executable `o2a`. After installation succeeds, you can try `o2a` by doing

    o2a --h

And run `o2a --rewrite` at the root of the folder.
If you wish to include your examples in the translation process, then modify the
`_OCamlProject` file by adding a new line in the `ML_SOURCES:` section, e.g.

	rcb/crdt/two_pset/two_pset_code.ml

The generated Coq file will be in the folder

	/theories/rcb/crdt/two_pset/two_pset_code.v
