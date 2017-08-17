(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad



module Ed25519 = struct

  module Public_key_hash = Hash.Make_Blake2B(Base58)(struct
      let name = "Ed25519.Public_key_hash"
      let title = "An Ed25519 public key ID"
      let b58check_prefix = Base58.Prefix.ed25519_public_key_hash
      let size = Some 20
    end)

  let () =
    Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36
    
    let sodium_Sign_public_key_size = 32
    let sodium_Sign_secret_key_size = 32
    let sodium_Sign_signature_size = 32
    let sodium_Sign_Bytes_of_public_key x = x
    let sodium_Sign_Bytes_to_public_key x = x
    let sodium_Sign_Bigbytes_of_public_key x = Bigbytes.of_bytes x
    let sodium_Sign_Bigbytes_to_public_key x = Bigbytes.to_bytes x
    let sodium_Sign_Bytes_of_secret_key x = x
    let sodium_Sign_Bytes_to_secret_key x = x
    let sodium_Sign_Bigbytes_of_secret_key x = Bigbytes.of_bytes x
    let sodium_Sign_Bigbytes_to_secret_key x = Bigbytes.to_bytes x
    
  module Public_key = struct

(*    type t = Sodium.Sign.public_key
    let compare = Sodium.Sign.compare_public_keys *)
    
    type t = Bytes.t

    let compare = compare
    
    let (=) xs ys = compare xs ys = 0
    let (<>) xs ys = compare xs ys <> 0
    let (<) xs ys = compare xs ys < 0
    let (<=) xs ys = compare xs ys <= 0
    let (>=) xs ys = compare xs ys >= 0
    let (>) xs ys = compare xs ys > 0
    let max x y = if x >= y then x else y
    let min x y = if x <= y then x else y
    
    

    type Base58.data +=
      | Public_key of t
      
    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_public_key
        ~length:sodium_Sign_public_key_size
        ~to_raw:(fun x -> Bytes.to_string (sodium_Sign_Bytes_of_public_key x))
        ~of_raw:(fun x ->
            try Some (sodium_Sign_Bytes_to_public_key (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Public_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 public key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = sodium_Sign_Bytes_to_public_key s

    let param ?(name="ed25519-public") ?(desc="Ed25519 public key (b58check-encoded)") t =
      Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

    let () =
      Base58.check_encoded_prefix b58check_encoding "edpk" 54

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (describe
             ~title: "An Ed25519 public key (Base58Check encoded)" @@
           conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 public key: unexpected prefix.")
             string)
        ~binary:
          (conv
             sodium_Sign_Bigbytes_of_public_key
             sodium_Sign_Bigbytes_to_public_key
             (Fixed.bytes sodium_Sign_public_key_size))

    let hash v =
      Public_key_hash.hash_bytes
        [ sodium_Sign_Bigbytes_of_public_key v ]

  end

  module Secret_key = struct

    type t = Bytes.t
(*     type t = Sodium.Sign.secret_key *)

    type Base58.data +=
      | Secret_key of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_secret_key
        ~length:sodium_Sign_secret_key_size
        ~to_raw:(fun x -> Bytes.to_string (sodium_Sign_Bytes_of_secret_key x))
        ~of_raw:(fun x ->
            try Some (sodium_Sign_Bytes_to_secret_key (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Secret_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 secret key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = sodium_Sign_Bytes_to_secret_key s

    let param ?(name="ed25519-secret") ?(desc="Ed25519 secret key (b58check-encoded)") t =
      Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

    let () =
      Base58.check_encoded_prefix b58check_encoding "edsk" 98

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (describe
             ~title: "An Ed25519 secret key (Base58Check encoded)" @@
           conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 secret key: unexpected prefix.")
             string)
        ~binary:
          (conv
             sodium_Sign_Bigbytes_of_secret_key
             sodium_Sign_Bigbytes_to_secret_key
             (Fixed.bytes sodium_Sign_secret_key_size))

  end

  let sign key msg = msg

(*
  let sign key msg =
    Sodium.Sign.Bigbytes.(of_signature @@ sign_detached key msg)
*)

  module Signature = struct

    type t = MBytes.t

    type Base58.data +=
      | Signature of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_signature
        ~length:sodium_Sign_signature_size
        ~to_raw:MBytes.to_string
        ~of_raw:(fun s -> Some (MBytes.of_string s))
        ~wrap:(fun x -> Signature x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 signature)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = MBytes.of_string (Bytes.to_string s)

    let param ?(name="signature") ?(desc="Signature (b58check-encoded)") t =
      Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

    let () =
      Base58.check_encoded_prefix b58check_encoding "edsig" 99

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (describe
             ~title: "An Ed25519 signature (Base58Check encoded)" @@
           conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 signature: unexpected prefix.")
             string)
        ~binary: (Fixed.bytes sodium_Sign_signature_size)

(*
    let check public_key signature msg =
      try
        Sodium.Sign.Bigbytes.(verify public_key (to_signature signature) msg) ;
        true
      with _ -> false
*)

    let check public_key signature msg = true

    let append key msg =
      MBytes.concat msg (sign key msg)

  end

    let sodium_Sign_random_keypair () =
       (Bytes.make 32 'a', Bytes.make 32 'b')

  let generate_key () =
    let secret, pub = sodium_Sign_random_keypair () in
    (Public_key.hash pub, pub, secret)

end
module Make(Param : sig val name: string end)() = struct

  include Pervasives
  module Pervasives = Pervasives
  module Compare = Compare
  module Array = Array
  module List = List
  module Bytes = struct
    include Bytes
    include EndianBytes.BigEndian
    module LE = EndianBytes.LittleEndian
  end
  
  module String = struct
    include String
    include EndianString.BigEndian
    module LE = EndianString.LittleEndian
  end
  module Set = Set
  module Map = Map
  module Int32 = Int32
  module Int64 = Int64
  module Nativeint = Nativeint
  module Buffer = Buffer
  module Format = Format
  module Hex_encode = Hex_encode
  
  module Z = Z

  module Lwt_sequence = Lwt_sequence
  module Lwt = Lwt
  module Lwt_list = Lwt_list
  module MBytes = MBytes
  module Uri = Uri
  module Data_encoding = Data_encoding
  module Time = Time
  module Ed25519 = Ed25519
  module Hash = Hash
  module Tezos_data = Tezos_data
  
  module RPC = RPC
  module Error_monad = Error_monad 
  (*struct
    type error_category = [ `Branch | `Temporary | `Permanent ]
    include Error_monad.Make()
  end*)

  module Logging = Logging.Make(Param)
  module Base58 = struct
    include Base58
    let simple_encode enc s = simple_encode enc s
    let simple_decode enc s = simple_decode enc s
(*
    include Make(struct type context = Context.t end)
    let decode s = decode s
  *)
  end
  
  
  module Fitness = Tezos_data.Fitness
  module Persist = Persist
  (*
  module Context = struct
    include Context
    let register_resolver = Base58.register_resolver
    let complete ctxt s = Base58.complete ctxt s
  end
  *)

(*
  module Updater = struct

(* open Logging.Updater *)
open Tezos_data
open Hash

let (//) = Filename.concat

type validation_result (* = Protocol_sigs.validation_result *) = {
  context: Context.t ;
  fitness: Fitness.t ;
  message: string option ;
  max_operations_ttl: int ;
}

type rpc_context (* = Protocol_sigs.rpc_context *) = {
  block_hash: Block_hash.t ;
  block_header: Block_header.t ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> Operation.t list list Lwt.t ;
  context: Context.t ;
}

module type PROTOCOL = sig end (* Protocol_sigs.PROTOCOL *)
(*
module type PACKED_PROTOCOL = sig end (* Protocol_sigs.PACKED_PROTOCOL *)
module type REGISTRED_PROTOCOL = sig
  val hash: Protocol_hash.t
  include PROTOCOL with type error := error
                             and type 'a tzresult := 'a tzresult
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end
*)


(** Version table *)

module VersionTable = Protocol_hash.Table

(*
let versions : ((module REGISTRED_PROTOCOL)) VersionTable.t =
  VersionTable.create 20

let register hash proto =
  VersionTable.add versions hash proto
*)

let activate = Context.set_protocol
let fork_test_network = Context.fork_test_network

(*
let get_exn hash = VersionTable.find versions hash

let get hash =
  try Some (get_exn hash)
  with Not_found -> None
*)

(** Compiler *)

(*
let datadir = ref None
let get_datadir () =
  match !datadir with
  | None -> fatal_error "not initialized"
  | Some m -> m

let init dir =
  datadir := Some dir

let create_files dir units =
  Lwt_utils.remove_dir dir >>= fun () ->
  Lwt_utils.create_dir dir >>= fun () ->
  Lwt_list.map_s
    (fun { Protocol.name; interface; implementation } ->
       let name = String.lowercase_ascii name in
       let ml = dir // (name ^ ".ml") in
       let mli = dir // (name ^ ".mli") in
       Lwt_utils.create_file ml implementation >>= fun () ->
       match interface with
       | None -> Lwt.return [ml]
       | Some content ->
           Lwt_utils.create_file mli content >>= fun () ->
           Lwt.return [mli;ml])
    units >>= fun files ->
  let files = List.concat files in
  Lwt.return files

let extract dirname hash units =
  let source_dir = dirname // Protocol_hash.to_short_b58check hash // "src" in
  create_files source_dir units >|= fun _files ->
  Tezos_compiler.Meta.to_file source_dir ~hash
    (List.map (fun {Protocol.name} -> String.capitalize_ascii name) units)

let do_compile hash units =
  let datadir = get_datadir () in
  let source_dir = datadir // Protocol_hash.to_short_b58check hash // "src" in
  let log_file = datadir // Protocol_hash.to_short_b58check hash // "LOG" in
  let plugin_file = datadir // Protocol_hash.to_short_b58check hash //
                    Format.asprintf "protocol_%a.cmxs" Protocol_hash.pp hash
  in
  create_files source_dir units >>= fun _files ->
  Tezos_compiler.Meta.to_file source_dir ~hash
    (List.map (fun {Protocol.name} -> String.capitalize_ascii name) units);
  let compiler_command =
    (Sys.executable_name,
     Array.of_list [Node_compiler_main.compiler_name; plugin_file; source_dir]) in
  let fd = Unix.(openfile log_file [O_WRONLY; O_CREAT; O_TRUNC] 0o644) in
  let pi =
    Lwt_process.exec
      ~stdin:`Close ~stdout:(`FD_copy fd) ~stderr:(`FD_move fd)
      compiler_command in
  pi >>= function
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      log_error "INTERRUPTED COMPILATION (%s)" log_file;
      Lwt.return false
  | Unix.WEXITED x when x <> 0 ->
      log_error "COMPILATION ERROR (%s)" log_file;
      Lwt.return false
  | Unix.WEXITED _ ->
      try Dynlink.loadfile_private plugin_file; Lwt.return true
      with Dynlink.Error err ->
        log_error "Can't load plugin: %s (%s)"
          (Dynlink.error_message err) plugin_file;
        Lwt.return false

let compile hash units =
  if VersionTable.mem versions hash then
    Lwt.return true
  else begin
    do_compile hash units >>= fun success ->
    let loaded = VersionTable.mem versions hash in
    if success && not loaded then
      log_error "Internal error while compiling %a" Protocol_hash.pp hash;
    Lwt.return loaded
  end
*)
end
  
  module type PACKED_PROTOCOL = sig
    val hash : Hash.Protocol_hash.t
    include Updater.PROTOCOL
    val error_encoding : error Data_encoding.t
    val classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
    val pp : Format.formatter -> error -> unit
    val complete_b58prefix : Context.t -> string -> string list Lwt.t
  end
*)  

end

module Env = Make (struct let name = "testing" end) ()


