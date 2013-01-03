type ('a, 'b) t

val empty : ('a, 'b) t
val is_empty : ('a, 'b) t -> bool
val singleton : 'a -> 'b -> ('a, 'b) t
val cardinal : ('a, 'b) t -> int

val alter : 'a -> ('b option -> 'b option) -> ('a, 'b) t -> ('a, 'b) t
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val add_carry : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t * 'b option
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
val extract : 'a -> ('a, 'b) t -> 'b * ('a, 'b) t
val update : 'a -> ('b -> 'b option) -> ('a, 'b) t -> ('a, 'b) t
val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
val modify_def : 'b -> 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
val adjust : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val filterv : ('b -> bool) -> ('a, 'b) t -> ('a, 'b) t
val filter : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
val filteri : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
val filter_map : ('a -> 'b -> 'c option) -> ('a, 'b) t -> ('a, 'c) t

val find : 'a -> ('a, 'b) t -> 'b
val mem : 'a -> ('a, 'b) t -> bool

val fold : ('b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val foldi : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val keys : ('a, 'b) t -> 'a list
val values : ('a, 'b) t -> 'b list
val bindings : ('a, 'b) t -> ('a * 'b) list
val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
val partition : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
val choose : ('a, 'b) t -> 'a * 'b
val pop : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t

val intersect : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t

val merge :
  ('a -> 'b option -> 'c option -> 'd option) ->
  ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
val union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val union_f : ('b -> 'b -> 'b) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

module Import :
sig

  module type FOLDABLE =
  sig
    type key
    type 'a t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

  module Make (M : FOLDABLE) :
  sig
    val add_from : 'a M.t -> (M.key, 'a) t -> (M.key, 'a) t
    val from : 'a M.t -> (M.key, 'a) t
  end

  module List :
  sig
    val add_from : ('a * 'b) list -> ('a, 'b) t -> ('a, 'b) t
    val from : ('a * 'b) list -> ('a, 'b) t
  end

end

module ExceptionLess :
sig
  val extract : 'a -> ('a, 'b) t -> 'b option * ('a, 'b) t
  val find : 'a -> ('a, 'b) t -> 'b option
  val choose : ('a, 'b) t -> ('a * 'b) option
end

module Infix :
sig
  val ( --> ) : ('a, 'b) t -> 'a -> 'b
  val ( <-- ) : ('a, 'b) t -> 'a * 'b -> ('a, 'b) t
end
