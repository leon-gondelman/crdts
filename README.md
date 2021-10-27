# crdts
OCaml implementations of Conflict-Free Replicated Datatypes for Fun and Profit. (Distributed System Project Course)

###Table of Contents
=================

* [0\. Welcome](#0-welcome)
* [1\. Structure of the repo](#1-structure-of-the-repo)
* [2\. Setting up a new example\.](#2-setting-up-a-new-example)
* [3\. Compilation and testing\.](#3-compilation-and-testing)
* [4\. Translation to AnerisLang (Coq) using o2a compiler\. (Optional)](#4-translation-to-anerislang-coq-using-o2a-compiler-optional)

# 0. Welcome
Welcome to the repository. Here you will find several CRDTs implemented using a Pure Operation Based framework implementation inspired by the paper [Pure Operation-Based Replicated Data Types](https://arxiv.org/abs/1710.04469) by Carlos Baquero, Paulo Sérgio Almeida and Ali Shoker.
It builds on aneris-lang with the goal of turning the OCaml sources into Coq for formal provability. To implement the CRDTs a framework was created in accordance with the aforementioned paper that the CRDTs in turn use. Many of the implemented CRDTs come from the paper, but there are also CRDT implementations that are inspired by other sources.
Noteworthy among these is the collaborative text editing systems that allow several users to insert characters into a shared text system, including editing different parts of the document at once.
Some CRDTs in this repository presently do not use the Pure Operation Based Framework and are instead built directly on Reliable Causal Broadcast with no extra layer. This includes dynamo, gset and counter, illustrating how a CRDT may look when not using the framework, and serving as a comparison point against the CRDTs using the framework that are abstracted away from the details of message broadcasting, locks and concurrency handling. 

# 1. Structure of the repo

The two main folder of the repository are `ml_sources/` and
`/Users/apollo/repositories/aneris/crdts/vendor/aneris/ml_sources/aneris_lang`.


- `ml_sources/`: This folder contains the reliable causal broadcast
  implementation `rcb/` and the CRDTs `rcb/crdt/...`.
  Inside it one also finds the generalised pure_op_based_framework code that the CRDTs build upon `rcb/crdt/pure_op_based_framework`


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
The run scripts in the repo right now are adapted to the paths used for testing during development, and are not relative to CWD.
The run scripts also use AppleScript for Terminal manipulation but the idea of the run script may easily be adapted to relevant platform of choice.

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
