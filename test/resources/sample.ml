(** Sample OCaml implementation for integration testing. *)

(* A simple type definition with variants *)
type shape =
  | Circle of float
  | Rectangle of float * float

(* A record type *)
type point = {
  x: float;
  y: float;
}

(* A simple function *)
let area = function
  | Circle r -> Float.pi *. r *. r
  | Rectangle (w, h) -> w *. h

(* A recursive function *)
let rec factorial n =
  if n <= 1 then
    1
  else
    n * factorial (n - 1)

(* Module with signature constraint *)
module IntSet = Set.Make (Int)

(* A functor application *)
module StringMap = Map.Make (String)

(* External declaration *)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"

(* Exception definition *)
exception Invalid_shape of string

(* A function using try/with *)
let safe_head lst =
  try
    List.hd lst
  with
  | Failure _ -> raise (Invalid_shape "empty list")

(* Let with type annotation *)
let origin : point = { x = 0.0; y = 0.0 }

(* Module type definition *)
module type PRINTABLE = sig
  type t
  val to_string : t -> string
end

(* Module implementing a signature *)
module PrintableInt : PRINTABLE with type t = int = struct
  type t = int
  let to_string = string_of_int
end

(* A for loop *)
let sum_to n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total

(* Polymorphic variants *)
let color_to_string = function
  | `Red -> "red"
  | `Green -> "green"
  | `Blue -> "blue"

(* Function defined with fun *)
let double = fun x -> x * 2

(* Constructor expressions *)
let maybe_one = Some 1
let nothing = None

(* Array literal *)
let arr = [| 1; 2; 3 |]

(* PPX extension *)
let () = [%test "basic test"]

(* Pipe and apply operators *)
let result = [1; 2; 3] |> List.map double
let applied = print_endline @@ "hello"

(* Pattern matching with nested patterns *)
let describe_pair = function
  | (0, 0) -> "origin"
  | (x, 0) -> Printf.sprintf "x-axis at %d" x
  | (0, y) -> Printf.sprintf "y-axis at %d" y
  | (x, y) -> Printf.sprintf "(%d, %d)" x y
