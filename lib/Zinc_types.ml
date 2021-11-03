open Zinc_utils

type address = string [@@deriving show { with_path = false }, eq, yojson]

type contract = string * address option
[@@deriving show { with_path = false }, eq, yojson]

type 'a zinc_instruction =
  (*
      Everything in here should be safe and trustworthy. Our assumption is that an adversary
      can create whatever zinc they want and provide it as code to the interpreter.
      The code is guaranteed
  *)
  (* ====================
     zinc core operations
     ====================
  *)
  | Grab
  | Return
  | PushRetAddr of 'a zinc
  | Apply
  | Access of int
  | Closure of 'a zinc
  | EndLet
  (*
     ================
     Extra operations
     ================
  *)
  (* ASTs *)
  | MakeRecord of label list
  | RecordAccess of label
  | MakeVariant of label
  | MatchVariant of (label * 'a zinc) list
  (* math *)
  | Num of Z.t
  | Add
  (* boolean *)
  | Bool of bool
  | Eq
  (* misc *)
  | String of string
  (* Crypto *)
  | Key of string
  | HashKey
  | Hash of string
  (* serialization *)
  | Bytes of bytes
  (*
     ===========================
     tezos_specific instructions
     ===========================
  *)
  | Address of address
  | ChainID
  | Contract_opt
  | MakeTransaction
  | Mutez of Z.t
  (* Adding this to make contracts easier to interpret even though I think it's technically unecessary  *)
  | Done
  | Failwith
  (* Extensions *)
  | Extensions of 'a
[@@deriving show { with_path = false }, eq, yojson, map]

and 'a zinc = 'a zinc_instruction list
[@@deriving show { with_path = false }, eq, yojson, map]

type zinc_extension_constructors =
  (*
      Need to come up with a better name than zinc_extension_constructors,
      it's for zinc "instructions" that can't be passed as code.
      (Instead they can only be present in the stack or environment)
  *)
  | Contract of contract
  | Operation of operation
[@@deriving show { with_path = false }, eq, yojson]

and operation = Transaction of Z.t * contract (* todo: add parameter *)
[@@deriving show { with_path = false }, eq, yojson]

type zinc_instruction_code = Nothing.t zinc_instruction
[@@deriving show { with_path = false }, eq, yojson]

type zinc_instruction_extended = zinc_extension_constructors zinc_instruction
[@@deriving show { with_path = false }, eq, yojson]

(*
type zinc_code = { code : 'a. 'a zinc }

let pp_zinc_code : Format.formatter -> zinc_code -> unit =
 fun fmt { code } ->
  Format.fprintf fmt "%a" (pp_zinc (fun _ -> Nothing.unreachable_code)) code

let equal_zinc_code { code = a } { code = b } =
  equal_zinc Nothing.unreachable_code a b

let zinc_code_of_yojson input =
  zinc_of_yojson (fun _ -> failwith "should be unreach") input
  |> Result.map (fun code -> { code })
let zinc_code_to_yojson { code } = zinc_to_yojson Nothing.unreachable_code code
*)

type zinc_code = Nothing.t zinc
[@@deriving show { with_path = false }, eq, yojson]

type zinc_extended = zinc_extension_constructors zinc
[@@deriving show { with_path = false }, eq, yojson]

type program = (string * Nothing.t zinc) list
[@@deriving show { with_path = false }, eq, yojson]

type env_item =
  [ `Z of zinc_instruction_extended
  | `Clos of clos
  | `Record of stack_item LMap.t
  | `Variant of label * stack_item ]
[@@deriving show, eq, yojson]

and stack_item =
  [ (* copied from env_item *)
    `Z of zinc_instruction_extended
  | `Clos of clos
  | `Record of stack_item LMap.t
  | `Variant of label * stack_item
  | (* marker to note function calls *)
    `Marker of
    zinc_extension_constructors zinc * env_item list ]
[@@deriving show, eq]

and clos = { code : zinc_extension_constructors zinc; env : env_item list }
[@@deriving show, eq, yojson]

type env = env_item list [@@deriving show, eq, yojson]

type stack = stack_item list [@@deriving show, eq, yojson]

type interpreter_input = zinc_code * env * stack [@@deriving show, eq, yojson]

type interpreter_output = Success of env * stack | Failure of string
[@@deriving show, eq, yojson]

let generalize_zinc_instruction :
      'a. zinc_instruction_code -> 'a zinc_instruction =
  map_zinc_instruction Nothing.unreachable_code

and generalize_zinc : 'a. zinc_code -> 'a zinc =
  map_zinc Nothing.unreachable_code

type interpreter_context = { get_contract_opt : address -> contract option }
(* TODO: get_contract_opt needs to accept a type too *)

module Utils = struct
  let unit_record = `Record LMap.empty
end
