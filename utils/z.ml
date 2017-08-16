
type t = Big_int.big_int
     open Big_int
     let compare = compare_big_int
     let zero = zero_big_int
     
     let to_string = string_of_big_int
     let of_string = big_int_of_string
     let to_int = int_of_big_int
     let of_int = big_int_of_int
     let to_int64 = int64_of_big_int
     let of_int64 = big_int_of_int64
     
     (* fix .. blah no they were not like this *)
     let of_bits str =
       let res = ref zero in
       for i = 0 to String.length str - 1 do
         res := mult_big_int !res (of_int 256);
         res := add_big_int !res (of_int (Char.code str.[String.length str - 1 - i]));
       done;
       !res


     let rec to_bits_aux x =
       if to_int x = 0 then "" else
       let rest = to_bits_aux (div_big_int x (of_int 256)) in
       String.make 1 (Char.chr (to_int (mod_big_int x (of_int 256)))) ^ rest

     let to_bits x =
       let res = to_bits_aux x in
       if String.length res = 0 then "0" else res
       
       let _ =
          prerr_endline (to_bits (of_int 0xefff));
          prerr_endline (to_bits (of_bits "as"))

     let add = add_big_int
     let sub = sub_big_int
     let mul = mult_big_int
     let ediv_rem = quomod_big_int
     let div_rem = quomod_big_int
     let abs = abs_big_int
     let neg x = sub zero x
     let shift_left = shift_left_big_int
     let shift_right = shift_right_big_int
     
     let logor = or_big_int
     let logand = and_big_int
     let logxor = xor_big_int
     let lognot x = neg x (* fix it *)

