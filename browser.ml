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
  match Html.tagged (Js.Opt.get (document##querySelector (js id)) (fun () -> assert false)) with
  | Html.Textarea a -> a
  | _ -> raise (Failure "not a text area element")

let get_textinput id =
  match Html.tagged (Js.Opt.get (document##getElementById (js id)) (fun () -> assert false)) with
  | Html.Input a -> a
  | _ -> raise (Failure "not a text area element")

(*
let code_elem = get_text "#code_editor textarea"
*)
let get_code () =
  let code_elem = Js.Unsafe.variable "window.editor" in
  Js.to_string (Js.Unsafe.meth_call code_elem "getValue" [| |])

let make_context log =
  let error fmt =
    Format.kasprintf
      (fun msg ->
         Lwt.fail (Failure msg))
      fmt in
  let warning fmt =
    Format.kasprintf
      (fun msg -> log "stderr" msg)
      fmt in
  let message fmt =
    Format.kasprintf
      (fun msg -> log "stdout" msg)
      fmt in
  let answer =
    message in
  let log name fmt =
    Format.kasprintf
      (fun msg -> log name msg)
      fmt in
  { error ; warning ; message ; answer ; log }

let error_report err =
  let msg = Buffer.create 5000 in
  let cctxt = make_context (fun _ t -> Buffer.add_string msg t ; Buffer.add_char msg '\n' ; Lwt.return ()) in
  report_errors cctxt [err];
  Buffer.contents msg

let expr_to_string expr =
    let _ = print_expr no_locations Format.str_formatter expr in
    Format.flush_str_formatter ()

let contract_to_string c =
    let _ = Contract.pp Format.str_formatter c in
    Format.flush_str_formatter ()

let type_to_string ty =
    let expr = Script_ir_translator.unparse_ty ty in
    expr_to_string expr

let stack_type_to_string ty =
    let _ = print_expr no_locations Format.str_formatter (Script.Seq (0,Script_ir_translator.unparse_stack ty)) in
    Format.flush_str_formatter ()


let get_info err =
  let open Script_typed_ir in
  let open Script_ir_translator in
  let open Script_interpreter in
  match err with
  | Exn (Script_located_ir.Missing_program_field a) -> (1, "Missing program field " ^ a)
  | Script_ir_translator.Invalid_primitive (loc, exp, got) ->
    (loc, "Invalid primitive " ^ got)
  | Script_ir_translator.Invalid_case (loc, got) -> (loc, "Invalid primitive name case " ^ got)
  | Script_ir_translator.Ill_typed_contract (code, arg_type, ret_type, storage_type, _) -> (1, "Invalid contract type")
  | Script_ir_translator.Bad_return (loc, got, expected) ->
    let _ = print_expr no_locations Format.str_formatter (Script.Seq (0,Script_ir_translator.unparse_stack got)) in
    let ret_str = Format.flush_str_formatter () in
    (loc, "Bad return type " ^ ret_str)
  | Script_ir_translator.Bad_stack (loc, name, numargs, got) ->
    let _ = print_expr no_locations Format.str_formatter (Script.Seq (0,Script_ir_translator.unparse_stack got)) in
    let ret_str = Format.flush_str_formatter () in
    (loc, "Bad stack type for " ^ name ^ ": " ^ ret_str)
  | Ill_typed_data (name, expr, ty) -> (0, "Ill typed data " ^ expr_to_string expr ^ " expected " ^ expr_to_string (Script_ir_translator.unparse_ty ty))
  | Ill_formed_type (name, expr) -> (0, "Ill formed type " ^ expr_to_string expr)
  | Runtime_contract_error (contract, expr, arg_ty, ret_ty, storage_ty) -> (0, "Runtime error at " ^ expr_to_string expr)
  | Invalid_arity (loc, name, exp, got) -> (loc, Printf.sprintf "Primitive %s expects %d arguments but is given %d." name exp got)
  | Invalid_namespace (loc, name, exp, got) ->
        let human_namespace = function
          | Instr_namespace -> ("an", "instruction")
          | Type_namespace -> ("a", "type name")
          | Constant_namespace -> ("a", "constant constructor") in
        (loc, Printf.sprintf "Unexpected %s %s, only@ %s@ %s@ can@ be@ used@ here."
          (snd (human_namespace got))
          name
          (fst (human_namespace exp)) (snd (human_namespace exp)))
  | Invalid_kind (loc, exp, got) ->
        let human_kind = function
          | Seq_kind -> ("a", "sequence")
          | Prim_kind -> ("a", "primitive")
          | Int_kind -> ("an", "int")
          | String_kind -> ("a", "string") in
        (loc, Printf.sprintf  "Unexpected %s, only %s can be used here."
          (snd (human_kind got))
          (String.concat ", " (List.map (fun k -> let (a, n) = human_kind k in a ^ " " ^ n) exp)))
  | Fail_not_in_tail_position loc ->
        (loc, Printf.sprintf "The FAIL instruction must appear in a tail position.")
  | Undefined_binop (loc, name, tya, tyb) ->
      (loc, Printf.sprintf
          "Operator %s is undefined between %s and %s."
          name
          (type_to_string tya)
          (type_to_string tyb))
  | Unmatched_branches (loc, sta, stb) ->
      (loc, Printf.sprintf
          "Two branches don't end with the same stack type.
           1st stack type: %s, 2nd stack type: %s"
          (stack_type_to_string sta)
          (stack_type_to_string stb))
  | Transfer_in_lambda loc -> (loc, "The TRANSFER_TOKENS instruction cannot appear in a lambda.")
    | Bad_stack_length -> (0, "Bad stack length.")
    | Bad_stack_item lvl -> (0, Printf.sprintf "Bad stack item %d ." lvl)
    | Invalid_constant (loc, got, exp) -> (loc,
        Printf.sprintf
          "Value %s is invalid for type %s." (expr_to_string got) (type_to_string exp))
    | Invalid_contract (loc, contract) ->
        (loc, Printf.sprintf "Invalid contract %s." (contract_to_string contract))
    | Comparable_type_expected (loc, ty) ->
        (loc, Printf.sprintf "Comparable type expected. Type %s is not comparable."
          (type_to_string ty))
    | Inconsistent_types (tya, tyb) ->
        (0, Printf.sprintf "Type %s is not compatible with type %s." (type_to_string tya) (type_to_string tyb))
    | Reject _ -> (0, "Script reached FAIL instruction")
    | Overflow _ -> (0, "Unexpected arithmetic overflow")
  | _ -> (1, "Fix error reporting")

let set_annotations lst loc_map =
  let open Script_located_ir in
  let loc_to_string (l1,l2) =
     " (at " ^ string_of_int l1.line ^  ":" ^ string_of_int l1.column ^ " - " ^ string_of_int l2.line ^  ":" ^ string_of_int l2.column ^ ")" in
  let arr = new%js Js.array_empty in
  let add_annotation (row, str) =
    let row, rtext =
      try
         let l_map = List.assoc "code" loc_map in
         let loc = List.assoc row l_map in
         (fst loc).line, loc_to_string loc
      with Not_found -> row, "" in
    let obj = Js.Unsafe.obj [|
      ("column", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("row", Js.Unsafe.inject (Js.number_of_float (float_of_int (row-1))));
      ("type", Js.Unsafe.inject (js"error"));
      ("text", Js.Unsafe.inject (js (str ^ rtext))) |] in
    ignore (arr##push obj) in
  List.iter add_annotation lst;
  let session = Js.Unsafe.variable "window.session" in
  Js.Unsafe.meth_call session "setAnnotations" [| Js.Unsafe.inject arr |];
  ()

let input_elem = get_textinput "callinput"
let type_elem = get_textinput "storagetype"
let storage_elem = get_text "#instorage"
let output_elem = Js.Opt.get (document##getElementById (js"calloutput")) (fun () -> assert false)
let outstorage_elem = Js.Opt.get (document##getElementById (js"outstorage")) (fun () -> assert false)
let button_elem = Js.Opt.get (document##getElementById (js"run_button")) (fun () -> assert false)

let error_lst = ref []

let report x = x >>= function
 | Ok a -> Lwt.return (Ok a)
 | Pervasives.Error lst ->
   error_lst := lst @ !error_lst;
   Lwt.return (Pervasives.Error lst)

let update _ =
  let code_str = get_code () in
  let stor_str = Js.to_string (storage_elem##.value) in
  let type_str = Js.to_string (type_elem##.value) in
  let input_str = Js.to_string (input_elem##.value) in
  error_lst := [];
  let locs = ref [] in
  begin
    report (parse_program code_str) >>=? fun {ast=prog; loc_table=locations} ->
(*    let [ "code", code_loc_table ;
           "parameter", parameter_loc_table ;
           "return", return_loc_table ;
           "storage", storage_loc_table ] *)
    let _ = locs := locations in
    report (parse_data input_str) >>=? fun {ast=data} ->
    report (parse_data stor_str) >>=? fun {ast=stor} ->
    report (parse_data type_str) >>=? fun {ast=stor_type} ->
    let storage = {Script.storage = stor; Script.storage_type = stor_type} in
    report (Script_interpreter.execute nonce contract1 contract2 Tezos_context.empty_context storage prog money data 1000) >>=? fun (ret, stor, _, _, _) ->
       let _ = print_expr no_locations Format.str_formatter ret in
       let ret_str = Format.flush_str_formatter () in
       let _ = print_expr no_locations Format.str_formatter stor in
       let outstor_str = Format.flush_str_formatter () in
       let _ = output_elem##.textContent := Js.Opt.return (js ret_str) in
       let _ = outstorage_elem##.textContent := Js.Opt.return (js outstor_str) in
       return ()
  end;
  prerr_endline ("errs: " ^ string_of_int (List.length !error_lst));
  set_annotations (List.map get_info !error_lst) !locs;
  (* Set annotations *)
  Js._false

let _ =
  button_elem##.onclick := Html.handler update;
  ()

