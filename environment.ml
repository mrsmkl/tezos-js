(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

type bigbytes =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(*
module type S = sig
  type t
  type ctype

  val ctype      : ctype typ

  val create     : int -> t
  val zero       : t -> int -> int -> unit
  val blit       : t -> int -> t -> int -> int -> unit
  val sub        : t -> int -> int -> t
  val length     : t -> int
  val len_size_t : t -> PosixTypes.size_t
  val len_ullong : t -> Unsigned.ullong
  val to_ptr     : t -> ctype
  val to_bytes   : t -> Bytes.t
  val of_bytes   : Bytes.t -> t
end
*)

module Bigbytes = struct
  type t = bigbytes
  (*
  type ctype = char ptr

  let ctype = ptr char
  *)

  open Bigarray

  let create     len = (Array1.create char c_layout len)
  let length     str = Array1.dim str
  
  (*
  let len_size_t str = Unsigned.Size_t.of_int (Array1.dim str)
  let len_ullong str = Unsigned.ULLong.of_int (Array1.dim str)
  let to_ptr     str = bigarray_start array1 str
  *)
  
  let zero       str pos len = (Array1.fill (Array1.sub str pos len) '\x00')

  let to_bytes str =
    let str' = Bytes.create (Array1.dim str) in
    Bytes.iteri (fun i _ -> Bytes.set str' i (Array1.unsafe_get str i)) str';
    str'

  let of_bytes str =
    let str' = create (Bytes.length str) in
    Bytes.iteri (Array1.unsafe_set str') str;
    str'

  let sub = Array1.sub

  let blit src srcoff dst dstoff len =
    Array1.blit (Array1.sub src srcoff len)
                (Array1.sub dst dstoff len)
end

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
  module Persist = Persist
  module RPC = RPC
  module Fitness = Fitness
  module Updater = Updater
  module Error_monad = struct
    type error_category = [ `Branch | `Temporary | `Permanent ]
    include Error_monad.Make()
  end
  module Logging = Logging.Make(Param)
  module Base58 = struct
    include Base58
    let simple_encode enc s = simple_encode enc s
    let simple_decode enc s = simple_decode enc s
    include Make(struct type context = Context.t end)
    let decode s = decode s
  end
  module Context = struct
    include Context
    let register_resolver = Base58.register_resolver
    let complete ctxt s = Base58.complete ctxt s
  end

  module type PACKED_PROTOCOL = sig
    val hash : Protocol_hash.t
    include Updater.PROTOCOL
    val error_encoding : error Data_encoding.t
    val classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
    val pp : Format.formatter -> error -> unit
    val complete_b58prefix : Context.t -> string -> string list Lwt.t
  end

end
