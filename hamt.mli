module type CONFIG = sig
  val shift_step : int
  val bmnode_max : int
  val arrnode_min : int
end

module StdConfig : CONFIG
module StdConfig32 : CONFIG

module type HASHABLE = sig
  type t
  val hash : t -> int
end

module type S = sig

  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val cardinal : 'a t -> int

  val alter : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val add_carry : key -> 'a -> 'a t -> 'a t * 'a option
  val remove : key -> 'a t -> 'a t
  val extract : key -> 'a t -> 'a * 'a t
  val update : key -> ('a -> 'a option) -> 'a t -> 'a t
  val modify : key -> ('a -> 'a) -> 'a t -> 'a t
  val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
  val adjust : key -> ('a -> 'a) -> 'a t -> 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val filterv : ('a -> bool) -> 'a t -> 'a t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val find : key -> 'a t -> 'a
  val mem : key -> 'a t -> bool

  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val foldi : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val keys : 'a t -> key list
  val values : 'a t -> 'a list
  val bindings : 'a t -> (key * 'a) list
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val choose : 'a t -> key * 'a
  val pop : 'a t -> (key * 'a) * 'a t

  val intersect : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) ->
    'a t -> 'b t -> 'c t
  val union : 'a t -> 'a t -> 'a t
  val union_f : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  module Import :
  sig

    module type FOLDABLE =
    sig
      type key
      type 'a t
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    end

    module Make (M : FOLDABLE with type key = key) :
    sig
      val add_from : 'a M.t -> 'a t -> 'a t
      val from : 'a M.t -> 'a t
    end

    module AssocList :
    sig
      val add_from : (key * 'a) list -> 'a t -> 'a t
      val from : (key * 'a) list -> 'a t
    end

  end

  module ExceptionLess :
  sig
    val extract : key -> 'a t -> 'a option * 'a t
    val find : key -> 'a t -> 'a option
    val choose : 'a t -> (key * 'a) option
  end

  module Infix :
  sig
    val ( --> ) : 'a t -> key -> 'a
    val ( <-- ) : 'a t -> key * 'a -> 'a t
  end
end

module Make (Config : CONFIG) (Key : HASHABLE) : S with type key = Key.t
