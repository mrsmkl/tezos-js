
open Script_interpreter
open Concrete_parser
open Concrete_lexer
open Nodejs
open Nodejs_high_level

let main () =
  prerr_endline "Starting up... ???";
  let str = Fs.read_file_sync "test.tz" Fs.Read in
  print_endline str;
  prerr_endline "trying to parse";
  let prog = Client_proto_programs.parse_program str in
  prerr_endline "something happened"
  
let _ = main ()

