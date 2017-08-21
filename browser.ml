open Script_interpreter
open Concrete_parser
open Concrete_lexer
(*
open Nodejs
open Nodejs_high_level
*)

open Js_of_ocaml
open Tezos_context
open Client_proto_programs
open Error_monad

module Html = Dom_html

let js = Js.string
let document = Html.window##.document

let nonce = {
  Contract.operation_hash = Hash.Operation_hash.hash_string ["aaas"];
  Contract.origination_index = Int32.of_int 123 }

let contract1 = Contract.Originated (Tezos_hash.Contract_hash.hash_string ["lklkjlkj"])
let contract2 = Contract.Originated (Tezos_hash.Contract_hash.hash_string ["lklkjl33"])

let money = Tez.zero

let get_text id =
  match Html.tagged (Js.Opt.get (document##getElementById (js id)) (fun () -> assert false)) with
  | Html.Textarea a -> a
  | _ -> raise (Failure "not a text area element")

let get_textinput id =
  match Html.tagged (Js.Opt.get (document##getElementById (js id)) (fun () -> assert false)) with
  | Html.Input a -> a
  | _ -> raise (Failure "not a text area element")

let code_elem = get_text "code"
let input_elem = get_textinput "callinput"
let type_elem = get_textinput "storagetype"
let storage_elem = get_text "instorage"
let output_elem = Js.Opt.get (document##getElementById (js"calloutput")) (fun () -> assert false)
let outstorage_elem = Js.Opt.get (document##getElementById (js"outstorage")) (fun () -> assert false)
let button_elem = Js.Opt.get (document##getElementById (js"run_button")) (fun () -> assert false)

let update _ =
  let code_str = Js.to_string (code_elem##.value) in
  let stor_str = Js.to_string (storage_elem##.value) in
  let type_str = Js.to_string (type_elem##.value) in
  let input_str = Js.to_string (input_elem##.value) in
  prerr_endline "trying to parse";
  begin
    parse_program code_str >>=? fun {ast=prog} ->
    parse_data input_str >>=? fun {ast=data} ->
    parse_data stor_str >>=? fun {ast=stor} ->
    parse_data type_str >>=? fun {ast=stor_type} ->
    let storage = {Script.storage = stor; Script.storage_type = stor_type} in
    let _ = prerr_endline "something happened" in
    Script_interpreter.execute nonce contract1 contract2 Tezos_context.empty_context storage prog money data 1000 >>=? fun (ret, stor, _, _, _) ->
    let _ = print_expr no_locations Format.str_formatter ret in
    let ret_str = Format.flush_str_formatter () in
    let _ = prerr_endline ret_str in
    let _ = print_expr no_locations Format.str_formatter stor in
    let outstor_str = Format.flush_str_formatter () in
    let _ = prerr_endline outstor_str in
    let _ = output_elem##.textContent := Js.Opt.return (js ret_str) in
    let _ = outstorage_elem##.textContent := Js.Opt.return (js outstor_str) in
    return ()
  end;
  Js._false

let _ =
  button_elem##.onclick := Html.handler update;
  prerr_endline "Here"


