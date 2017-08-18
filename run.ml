
open Script_interpreter
open Concrete_parser
open Concrete_lexer
open Nodejs
open Nodejs_high_level

let main () =
  prerr_endline "Starting up...";
  Fs.read_file_sync "test.tz" Fs.Read |> print_endline

let _ = main ()

