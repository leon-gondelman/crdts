open Unix
open Infinite_values_code
open Collaborative_editor_runner_shared

let () = Unix.handle_unix_error init_exec editor_init
