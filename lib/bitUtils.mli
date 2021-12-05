val skff : int

(* Given a bitmap, counts the number of one-bits *)
val ctpop : int -> int

(* Given a bitmap and a subhash, returns the index into the list *)
val from_bitmap : int -> int -> int

(** Given a bitmap and an integer, returns the boolean corresponding to
  the n-th bit (0 is the weakest) of the bitmap *)
val nth_bit_set : int -> int -> bool

(** Given a list of indices, returns the bitmap with one-bits at these indices *)
val indices_to_bitmap :  int list -> int

(* Given a bitmap, returns the list of indices where a one-bit is present *)
val bitmap_to_indices :  int -> int list
