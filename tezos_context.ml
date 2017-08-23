(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Environment.Env
open Error_monad

(*
type t = Storage.t
*)
open Hash
include Tezos_hash

type public_key = Ed25519.Public_key.t
type public_key_hash = Ed25519.Public_key_hash.t
type secret_key = Ed25519.Secret_key.t
type signature = Ed25519.Signature.t


module ContractMap = Map.Make (struct
  type t = Contract_hash.t
  let compare = compare
end)

module AccountMap = Map.Make (struct
  type t = Ed25519.Public_key_hash.t
  let compare = compare
end)

let default_hash = Environment.Ed25519.Public_key_hash.of_string_exn "12345123451234512345"

module Tez = Tez_repr
module Period = Period_repr



type account = {
  balance : Tez.t;
  script : Script_repr.t option;
  manager : public_key_hash;
}

type t = {
   accounts : account AccountMap.t;
   contracts : account ContractMap.t;
}

type context = t

let empty_context = {
  accounts = AccountMap.empty;
  contracts = ContractMap.empty;
}

module type BASIC_DATA = sig
  type t
  include Compare.S with type t := t
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
end


module Timestamp = struct
  include Time_repr
  let current ctx = Time.now ()
(*  let current = Storage.current_timestamp *)
end

include Operation_repr
module Operation = struct
  type t = operation
  include Operation_repr
end
module Block_header = Block_header_repr
module Vote = struct
  include Vote_repr
(*  include Vote_storage *)
end
module Raw_level = Raw_level_repr
module Cycle = Cycle_repr
module Script_int = Script_int_repr
module Script = Script_repr

module Constants = struct
  include Constants_repr
  (*
  let cycle_length c =
    let constants = Storage.constants c in
    constants.cycle_length
  let voting_period_length c =
    let constants = Storage.constants c in
    constants.voting_period_length
  let time_before_reward c =
    let constants = Storage.constants c in
    constants.time_before_reward
  let slot_durations c =
    let constants = Storage.constants c in
    constants.slot_durations
  let first_free_mining_slot c =
    let constants = Storage.constants c in
    constants.first_free_mining_slot
  let max_signing_slot c =
    let constants = Storage.constants c in
    constants.max_signing_slot
  let instructions_per_transaction c =
    let constants = Storage.constants c in
    constants.instructions_per_transaction
  let proof_of_work_threshold c =
    let constants = Storage.constants c in
    constants.proof_of_work_threshold
  let dictator_pubkey c =
    let constants = Storage.constants c in
    constants.dictator_pubkey
    *)
end

module Public_key = struct
   let default_key = Ed25519.Public_key.of_bytes (Bytes.create 32)
   let get:
    context -> public_key_hash -> public_key tzresult Lwt.t = fun ctx h -> return default_key

end

(* Public_key_storage *)

module Voting_period = Voting_period_repr



module Level = struct
  include Level_repr
(*include Level_storage*)
end
module Contract = struct
  include Contract_repr
  
  (*
  include Contract_storage
  *)
  
  let find c = function
   | Default x -> AccountMap.find x c.accounts
   | Originated x -> ContractMap.find x c.contracts
  
  let update c k f = match k with
   | Default x ->
      let y = AccountMap.find x c.accounts in
      {c with accounts=AccountMap.add x (f y) c.accounts}
   | Originated x ->
      let y = ContractMap.find x c.contracts in
      {c with contracts=ContractMap.add x (f y) c.contracts}
  
  let updateM c k (f:account -> account tzresult Lwt.t) = match k with
   | Default x ->
      let y = AccountMap.find x c.accounts in
      f y >>=? fun res ->
      return {c with accounts=AccountMap.add x res c.accounts}
   | Originated x ->
      let y = ContractMap.find x c.contracts in
      f y >>=? fun res ->
      return {c with contracts=ContractMap.add x res c.contracts}
  
  let exists c key = match key with
   | Default x -> return (AccountMap.mem x c.accounts)
   | Originated x -> return (ContractMap.mem x c.contracts)
  
  (* val get_script:
    context -> contract -> (Script.t option) tzresult Lwt.t *)
  let get_script (ctx:context) (key:contract) :  (Script.t option) tzresult Lwt.t =
    try return (find ctx key).script
    with Not_found -> return None
  
  type error += Initial_amount_too_low of contract * Tez.t * Tez.t
  type error += Balance_too_low of contract * Tez.t * Tez.t

(*  val get_manager: context -> contract -> public_key_hash tzresult Lwt.t *)
  let get_manager (ctx:context) (key:contract) : public_key_hash tzresult Lwt.t =
    return (find ctx key).manager

  let spend_from_script (ctx:context) (key:contract) (t:Tez.t) =
     updateM ctx key (fun a -> begin
        Lwt.return (Tez.(-?) a.balance t) >>=? fun res ->
        return {a with balance=res}
     end)

  let credit:
    context -> contract -> Tez.t -> context tzresult Lwt.t = fun ctx key t ->
     updateM ctx key (fun a -> begin
        Lwt.return (Tez.(+?) a.balance t) >>=? fun res ->
        return {a with balance=res}
     end)

  let originate :
    context ->
    origination_nonce ->
    balance: Tez.t ->
    manager: public_key_hash ->
    ?script: (Script.t * (Tez.t * Tez.t)) ->
    delegate: public_key_hash option ->
    spendable: bool ->
    delegatable: bool -> (context * contract * origination_nonce) tzresult Lwt.t = fun c nonce ~balance ~manager ?script ~delegate ~spendable ~delegatable ->
       return (c, originated_contract nonce, nonce)

  let update_script_storage_and_fees:
    context -> contract -> Tez.t -> Script.expr -> context tzresult Lwt.t = fun ctx c t expr -> return ctx
  let get_balance:
    context -> contract -> Tez.t tzresult Lwt.t = fun ctx c -> return (Tez.zero)

(*  
  val must_exist: context -> contract -> unit tzresult Lwt.t

  val list: context -> contract list tzresult Lwt.t

  type origination_nonce

  val origination_nonce_encoding : origination_nonce Data_encoding.t
  val originated_contract : origination_nonce -> contract
  val originated_contracts : origination_nonce -> contract list

  val initial_origination_nonce : Operation_hash.t -> origination_nonce

  val get_delegate_opt:
    context -> contract -> public_key_hash option tzresult Lwt.t
  val is_delegatable:
    context -> contract -> bool tzresult Lwt.t
  val is_spendable:
    context -> contract -> bool tzresult Lwt.t
  
  val get_storage:
    context -> contract -> (Script.storage option) tzresult Lwt.t

  val get_counter: context -> contract -> int32 tzresult Lwt.t

  val set_delegate:
    context -> contract -> public_key_hash option -> context tzresult Lwt.t



  val spend:
    context -> contract -> Tez.t -> context tzresult Lwt.t


  val increment_counter:
    context -> contract -> context tzresult Lwt.t

  val check_counter_increment:
    context -> contract -> int32 -> unit tzresult Lwt.t
*)

end
module Roll = struct
  include Roll_repr
(*  include Roll_storage *)
end
(* module Nonce = Nonce_storage *)
module Seed = struct
  include Seed_repr
(*  include Seed_storage *)
end
(*
module Bootstrap = Bootstrap_storage
module Reward = Reward_storage
*)

module Fitness = struct

  include Fitness_repr
  include Fitness
  type fitness = t
(*  include Fitness_storage *)

end

(*
let init = Init_storage.may_initialize

let finalize ?commit_message:message c =
  let fitness = Fitness.from_int64 (Fitness.current c) in
  let context = Storage.recover c in
  { Updater.context ; fitness ; message ; max_operations_ttl = 60 }

let configure_sandbox = Init_storage.configure_sandbox

let activate = Storage.activate
let fork_test_network = Storage.fork_test_network
*)




