
open Script_interpreter
open Concrete_parser
open Concrete_lexer
open Nodejs
open Nodejs_high_level
open Tezos_context
open Client_proto_programs
open Error_monad

let nonce = {
  Contract.operation_hash = Hash.Operation_hash.hash_string ["aaas"];
  Contract.origination_index = Int32.of_int 123 }

let contract1 = Contract.Originated (Tezos_hash.Contract_hash.hash_string ["lklkjlkj"])
let contract2 = Contract.Originated (Tezos_hash.Contract_hash.hash_string ["lklkjl33"])

let money = Tez.zero

let main () =
  prerr_endline "Starting up... ???";
  let str = Fs.read_file_sync "test.tz" Fs.Read in
  print_endline str;
  prerr_endline "trying to parse";
  begin
    parse_program str >>=? fun {ast=prog} ->
    parse_data "\"asdf\"" >>=? fun {ast=data} ->
    parse_data "Unit" >>=? fun {ast=stor} ->
    parse_data "unit" >>=? fun {ast=stor_type} ->
    let storage = {Script.storage = stor; Script.storage_type = stor_type} in
    let _ = prerr_endline "something happened" in
    Script_interpreter.execute nonce contract1 contract2 Tezos_context.empty_context storage prog money data 1000 >>=? fun what ->
    return ()
  end;
  ()

let _ = main ()

