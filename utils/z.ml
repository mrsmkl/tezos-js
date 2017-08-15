
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
     
     (* fix *)
     let of_bits x = zero
     let to_bits x = "01010110"
     
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

