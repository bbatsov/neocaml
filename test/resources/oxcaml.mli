(** Sample OxCaml interface for integration testing. *)

kind_ immediate_or_null = value_or_null mod external_

type ('a : value mod portable) boxed = { global_ contents : 'a }

type vec = #{ dx : float#; dy : float# }

type t : value mod contended portable

val zero : vec

val length : 'a list @ local -> int

val make : unit -> int list @ local

val apply : ('a -> 'b) @ local once -> 'a -> 'b

val counter : int ref @@ portable

val sum : int iarray -> int

include sig
  val digits : int iarray
end @@ portable
