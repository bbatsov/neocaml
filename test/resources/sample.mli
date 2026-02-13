(** Sample OCaml interface for integration testing. *)

(* A type definition with variants *)
type shape =
  | Circle of float
  | Rectangle of float * float

(* A record type *)
type point = {
  x: float;
  y: float;
}

(* Value specifications *)
val area : shape -> float

val factorial : int -> int

(* External declaration *)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"

(* Exception definition *)
exception Invalid_shape of string

val safe_head : 'a list -> 'a

val origin : point

(* Module type definition *)
module type PRINTABLE = sig
  type t
  val to_string : t -> string
end

(* Module declaration with signature *)
module PrintableInt : PRINTABLE with type t = int

val sum_to : int -> int

(* A module type with nested signatures *)
module type COLLECTION = sig
  type 'a t
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end
