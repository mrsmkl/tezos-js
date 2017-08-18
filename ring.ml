(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a raw =
  | Empty of int
  | Inited of {
      data : 'a array ;
      mutable pos : int ;
    }

type 'a t = 'a raw ref

let create size = ref (Empty size)

let add r v =
  match !r with
  | Empty size ->
      r := Inited { data = Array.make size v ; pos = 0 }
  | Inited s ->
      s.pos <-
        if s.pos = 2 * Array.length s.data - 1 then
          Array.length s.data
        else
          s.pos + 1 ;
      s.data.(s.pos mod Array.length s.data) <- v

let add_list r l = List.iter (add r) l

let last r =
  match !r with
  | Empty _ -> None
  | Inited { data ; pos } -> Some data.(pos mod Array.length data)

let fold r ~init ~f =
  match !r with
  | Empty _ -> init
  | Inited { data ; pos } ->
      let size = Array.length data in
      let acc = ref init in
      for i = 0 to min pos (size - 1) do
        acc := f !acc data.((pos - i) mod size)
      done ;
      !acc

let elements t =
  fold t ~init:[] ~f:(fun acc elt -> elt :: acc)

exception Empty

let last_exn r =
  match last r with
  | None -> raise Empty
  | Some d -> d
