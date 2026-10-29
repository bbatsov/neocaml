(** Sample OxCaml implementation for integration testing.

    The constructs follow the way Jane Street's OxCaml libraries use
    them: modes and modalities, kinds, stack allocation, unboxed types
    and immutable arrays. *)

(* Kinds *)
kind_ immediate_or_null = value_or_null mod external_

type ('a : value mod portable) boxed = { global_ contents : 'a }

type point : immutable_data = { x : float; y : float }

(* Unboxed types and literals *)
type vec = #{ dx : float#; dy : float# }

type pair = #(int * float#)

let zero = #{ dx = #0.0; dy = #0.0 }

let swap #(a, b) = #(b, a)

let dx v = v.#dx

let is_set = #true

(* Modes *)
let length (xs @ local) = List.length xs

let apply (f @ local once) x = f x

let make () : int list @ local = exclave_ [ 1; 2; 3 ]

let pairs xs = exclave_
  let ys = List.rev xs in
  List.combine xs ys

let first = function
  | [] -> None
  | x :: _ ->
    exclave_
    Some x

let with_stack f =
  let r =
    stack_
      (ref 0)
  in
  f r;
  !r

let peek t = borrow_ t

(* Immutable arrays and comprehensions *)
let digits = [: 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 :]

let evens =
  [ x
    for x = 0 to 20
    when x mod 2 = 0 ]

let squares xs =
  [| x * x
     for x in xs |]

let sum = function
  | [::] -> 0
  | [: x :] -> x
  | _ -> 0

(* Mutable lets *)
let count xs =
  let mutable n = 0 in
  List.iter (fun _ -> n <- n + 1) xs;
  n
