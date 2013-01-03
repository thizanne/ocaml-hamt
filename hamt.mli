type ('a, 'b) t

val empty : ('a, 'b) t
val singleton : 'a -> 'b -> ('a, 'b) t
val is_empty : ('a, 'b) t -> bool
val cardinal : ('a, 'b) t -> int

val alter : 'a -> ('b option -> 'b option) -> ('a, 'b) t -> ('a, 'b) t
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
val update : 'a -> ('b -> 'b option) -> ('a, 'b) t -> ('a, 'b) t
val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
val modify_def : 'a -> 'b -> ('a -> 'a) -> ('b, 'a) t -> ('b, 'a) t
val adjust : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t

val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
val filter : ('a -> bool) -> ('b, 'a) t -> ('b, 'a) t
val filter_map : ('a -> 'b -> 'c option) -> ('a, 'b) t -> ('a, 'c) t
val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val filteri : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

val find : 'a -> ('a, 'b) t -> 'b
val mem : 'a -> ('a, 'b) t -> bool

val foldi : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val fold : ('a -> 'b -> 'b) -> ('c, 'a) t -> 'b -> 'b
val to_assoc : ('a, 'b) t -> ('a * 'b) list
val bindings : ('a, 'b) t -> ('a * 'b) list
val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
val exists_f : ('a -> 'b -> bool) -> ('a, 'b) t -> bool

val choose : ('a, 'b) t -> 'a * 'b

module Import :
sig

  module type FOLDABLE =
  sig
    type key
    type 'v t
    val fold : (key -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
  end

  module Make (M : FOLDABLE) :
  sig
    val add_import : 'a M.t -> (M.key, 'a) t -> (M.key, 'a) t
    val import : 'a M.t -> (M.key, 'a) t
  end

  module List :
  sig
    val add_import : ('a * 'b) list -> ('a, 'b) t -> ('a, 'b) t
    val import : ('a * 'b) list -> ('a, 'b) t
  end

end

val intersect : ('a -> 'a -> 'a) -> ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t

val merge :
  ('a -> 'b option -> 'c option -> 'd option) ->
  ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
val union : ('a -> 'a -> 'a) -> ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t
