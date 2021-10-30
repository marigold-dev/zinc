open Zinc_utils

module type Zinc_description = sig
  type hash = string

  val pp_hash :
    Ppx_deriving_runtime.Format.formatter -> hash -> Ppx_deriving_runtime.unit

  val show_hash : hash -> Ppx_deriving_runtime.string

  val equal_hash : hash -> hash -> Ppx_deriving_runtime.bool

  val hash_to_yojson : hash -> Yojson.Safe.t

  val hash_of_yojson :
    Yojson.Safe.t -> hash Ppx_deriving_yojson_runtime.error_or

  type key = string

  val pp_key :
    Ppx_deriving_runtime.Format.formatter -> key -> Ppx_deriving_runtime.unit

  val show_key : key -> Ppx_deriving_runtime.string

  val equal_key : key -> key -> Ppx_deriving_runtime.bool

  val key_to_yojson : key -> Yojson.Safe.t

  val key_of_yojson : Yojson.Safe.t -> key Ppx_deriving_yojson_runtime.error_or

  type address = string

  val pp_address :
    Ppx_deriving_runtime.Format.formatter ->
    address ->
    Ppx_deriving_runtime.unit

  val show_address : address -> Ppx_deriving_runtime.string

  val equal_address : address -> address -> Ppx_deriving_runtime.bool

  val address_to_yojson : address -> Yojson.Safe.t

  val address_of_yojson :
    Yojson.Safe.t -> address Ppx_deriving_yojson_runtime.error_or
end

module Z : Zinc_description = struct
  type hash = string [@@deriving show { with_path = false }, eq, yojson]

  type key = string [@@deriving show { with_path = false }, eq, yojson]

  type address = string [@@deriving show { with_path = false }, eq, yojson]
end

module type Zinc = sig
  type address

  val pp_address :
    Ppx_deriving_runtime.Format.formatter ->
    address ->
    Ppx_deriving_runtime.unit

  val show_address : address -> Ppx_deriving_runtime.string

  val equal_address : address -> address -> Ppx_deriving_runtime.bool

  val address_to_yojson : address -> Yojson.Safe.t

  val address_of_yojson :
    Yojson.Safe.t -> address Ppx_deriving_yojson_runtime.error_or

  type hash

  val pp_hash :
    Ppx_deriving_runtime.Format.formatter -> hash -> Ppx_deriving_runtime.unit

  val show_hash : hash -> Ppx_deriving_runtime.string

  val equal_hash : hash -> hash -> Ppx_deriving_runtime.bool

  val hash_to_yojson : hash -> Yojson.Safe.t

  val hash_of_yojson :
    Yojson.Safe.t -> hash Ppx_deriving_yojson_runtime.error_or

  type key

  val pp_key :
    Ppx_deriving_runtime.Format.formatter -> key -> Ppx_deriving_runtime.unit

  val show_key : key -> Ppx_deriving_runtime.string

  val equal_key : key -> key -> Ppx_deriving_runtime.bool

  val key_to_yojson : key -> Yojson.Safe.t

  val key_of_yojson : Yojson.Safe.t -> key Ppx_deriving_yojson_runtime.error_or

  type contract = string * address option

  val pp_contract :
    Ppx_deriving_runtime.Format.formatter ->
    contract ->
    Ppx_deriving_runtime.unit

  val show_contract : contract -> Ppx_deriving_runtime.string

  val equal_contract : contract -> contract -> Ppx_deriving_runtime.bool

  val contract_to_yojson : contract -> Yojson.Safe.t

  val contract_of_yojson :
    Yojson.Safe.t -> contract Ppx_deriving_yojson_runtime.error_or

  type 'a zinc_instruction =
    | Grab
    | Return
    | PushRetAddr of 'a zinc
    | Apply
    | Access of int
    | Closure of 'a zinc
    | EndLet
    | MakeRecord of Zinc_utils.label list
    | RecordAccess of Zinc_utils.label
    | MakeVariant of Zinc_utils.label
    | MatchVariant of (Zinc_utils.label * 'a zinc) list
    | Num of Zinc_utils.Z.t
    | Add
    | Bool of bool
    | Eq
    | String of string
    | Key of key
    | HashKey
    | Hash of hash
    | Bytes of bytes
    | Address of address
    | ChainID
    | Contract_opt
    | MakeTransaction
    | Mutez of Zinc_utils.Z.t
    | Done
    | Failwith
    | Extensions of 'a

  and 'a zinc = 'a zinc_instruction list

  val pp_zinc_instruction :
    (Ppx_deriving_runtime.Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
    Ppx_deriving_runtime.Format.formatter ->
    'a zinc_instruction ->
    Ppx_deriving_runtime.unit

  val show_zinc_instruction :
    (Ppx_deriving_runtime.Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
    'a zinc_instruction ->
    Ppx_deriving_runtime.string

  val pp_zinc :
    (Ppx_deriving_runtime.Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
    Ppx_deriving_runtime.Format.formatter ->
    'a zinc ->
    Ppx_deriving_runtime.unit

  val show_zinc :
    (Ppx_deriving_runtime.Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
    'a zinc ->
    Ppx_deriving_runtime.string

  val equal_zinc_instruction :
    ('a -> 'a -> Ppx_deriving_runtime.bool) ->
    'a zinc_instruction ->
    'a zinc_instruction ->
    Ppx_deriving_runtime.bool

  val equal_zinc :
    ('a -> 'a -> Ppx_deriving_runtime.bool) ->
    'a zinc ->
    'a zinc ->
    Ppx_deriving_runtime.bool

  val zinc_instruction_to_yojson :
    ('a -> Yojson.Safe.t) -> 'a zinc_instruction -> Yojson.Safe.t

  val zinc_instruction_of_yojson :
    (Yojson.Safe.t -> 'a Ppx_deriving_yojson_runtime.error_or) ->
    Yojson.Safe.t ->
    'a zinc_instruction Ppx_deriving_yojson_runtime.error_or

  val zinc_to_yojson : ('a -> Yojson.Safe.t) -> 'a zinc -> Yojson.Safe.t

  val zinc_of_yojson :
    (Yojson.Safe.t -> 'a Ppx_deriving_yojson_runtime.error_or) ->
    Yojson.Safe.t ->
    'a zinc Ppx_deriving_yojson_runtime.error_or

  val map_zinc_instruction :
    ('a -> 'b) -> 'a zinc_instruction -> 'b zinc_instruction

  val map_zinc : ('a -> 'b) -> 'a zinc -> 'b zinc

  type zinc_extension_constructors =
    | Contract of contract
    | Operation of operation

  and operation = Transaction of Zinc_utils.Z.t * contract

  val pp_zinc_extension_constructors :
    Ppx_deriving_runtime.Format.formatter ->
    zinc_extension_constructors ->
    Ppx_deriving_runtime.unit

  val show_zinc_extension_constructors :
    zinc_extension_constructors -> Ppx_deriving_runtime.string

  val pp_operation :
    Ppx_deriving_runtime.Format.formatter ->
    operation ->
    Ppx_deriving_runtime.unit

  val show_operation : operation -> Ppx_deriving_runtime.string

  val equal_zinc_extension_constructors :
    zinc_extension_constructors ->
    zinc_extension_constructors ->
    Ppx_deriving_runtime.bool

  val equal_operation : operation -> operation -> Ppx_deriving_runtime.bool

  val zinc_extension_constructors_to_yojson :
    zinc_extension_constructors -> Yojson.Safe.t

  val zinc_extension_constructors_of_yojson :
    Yojson.Safe.t ->
    zinc_extension_constructors Ppx_deriving_yojson_runtime.error_or

  val operation_to_yojson : operation -> Yojson.Safe.t

  val operation_of_yojson :
    Yojson.Safe.t -> operation Ppx_deriving_yojson_runtime.error_or

  type zinc_instruction_code = Zinc_utils.Nothing.t zinc_instruction

  val pp_zinc_instruction_code :
    Ppx_deriving_runtime.Format.formatter ->
    zinc_instruction_code ->
    Ppx_deriving_runtime.unit

  val show_zinc_instruction_code :
    zinc_instruction_code -> Ppx_deriving_runtime.string

  val equal_zinc_instruction_code :
    zinc_instruction_code -> zinc_instruction_code -> Ppx_deriving_runtime.bool

  val zinc_instruction_code_to_yojson : zinc_instruction_code -> Yojson.Safe.t

  val zinc_instruction_code_of_yojson :
    Yojson.Safe.t -> zinc_instruction_code Ppx_deriving_yojson_runtime.error_or

  type zinc_instruction_extended = zinc_extension_constructors zinc_instruction

  val pp_zinc_instruction_extended :
    Ppx_deriving_runtime.Format.formatter ->
    zinc_instruction_extended ->
    Ppx_deriving_runtime.unit

  val show_zinc_instruction_extended :
    zinc_instruction_extended -> Ppx_deriving_runtime.string

  val equal_zinc_instruction_extended :
    zinc_instruction_extended ->
    zinc_instruction_extended ->
    Ppx_deriving_runtime.bool

  val zinc_instruction_extended_to_yojson :
    zinc_instruction_extended -> Yojson.Safe.t

  val zinc_instruction_extended_of_yojson :
    Yojson.Safe.t ->
    zinc_instruction_extended Ppx_deriving_yojson_runtime.error_or

  type zinc_code = Zinc_utils.Nothing.t zinc

  val pp_zinc_code :
    Ppx_deriving_runtime.Format.formatter ->
    zinc_code ->
    Ppx_deriving_runtime.unit

  val show_zinc_code : zinc_code -> Ppx_deriving_runtime.string

  val equal_zinc_code : zinc_code -> zinc_code -> Ppx_deriving_runtime.bool

  val zinc_code_to_yojson : zinc_code -> Yojson.Safe.t

  val zinc_code_of_yojson :
    Yojson.Safe.t -> zinc_code Ppx_deriving_yojson_runtime.error_or

  type zinc_extended = zinc_extension_constructors zinc

  val pp_zinc_extended :
    Ppx_deriving_runtime.Format.formatter ->
    zinc_extended ->
    Ppx_deriving_runtime.unit

  val show_zinc_extended : zinc_extended -> Ppx_deriving_runtime.string

  val equal_zinc_extended :
    zinc_extended -> zinc_extended -> Ppx_deriving_runtime.bool

  val zinc_extended_to_yojson : zinc_extended -> Yojson.Safe.t

  val zinc_extended_of_yojson :
    Yojson.Safe.t -> zinc_extended Ppx_deriving_yojson_runtime.error_or

  type program = (string * Zinc_utils.Nothing.t zinc) list

  val pp_program :
    Ppx_deriving_runtime.Format.formatter ->
    program ->
    Ppx_deriving_runtime.unit

  val show_program : program -> Ppx_deriving_runtime.string

  val equal_program : program -> program -> Ppx_deriving_runtime.bool

  val program_to_yojson : program -> Yojson.Safe.t

  val program_of_yojson :
    Yojson.Safe.t -> program Ppx_deriving_yojson_runtime.error_or

  type env_item =
    [ `Clos of clos
    | `Record of stack_item Zinc_utils.LMap.t
    | `Variant of Zinc_utils.label * stack_item
    | `Z of zinc_instruction_extended ]

  and stack_item =
    [ `Clos of clos
    | `Marker of zinc_extension_constructors zinc * env_item list
    | `Record of stack_item Zinc_utils.LMap.t
    | `Variant of Zinc_utils.label * stack_item
    | `Z of zinc_instruction_extended ]

  and clos = { code : zinc_extension_constructors zinc; env : env_item list }

  val pp_env_item :
    Ppx_deriving_runtime.Format.formatter ->
    env_item ->
    Ppx_deriving_runtime.unit

  val show_env_item : env_item -> Ppx_deriving_runtime.string

  val pp_stack_item :
    Ppx_deriving_runtime.Format.formatter ->
    stack_item ->
    Ppx_deriving_runtime.unit

  val show_stack_item : stack_item -> Ppx_deriving_runtime.string

  val pp_clos :
    Ppx_deriving_runtime.Format.formatter -> clos -> Ppx_deriving_runtime.unit

  val show_clos : clos -> Ppx_deriving_runtime.string

  val equal_env_item : env_item -> env_item -> Ppx_deriving_runtime.bool

  val equal_stack_item : stack_item -> stack_item -> Ppx_deriving_runtime.bool

  val equal_clos : clos -> clos -> Ppx_deriving_runtime.bool

  val env_item_to_yojson : env_item -> Yojson.Safe.t

  val env_item_of_yojson :
    Yojson.Safe.t -> env_item Ppx_deriving_yojson_runtime.error_or

  val stack_item_to_yojson : stack_item -> Yojson.Safe.t

  val stack_item_of_yojson :
    Yojson.Safe.t -> stack_item Ppx_deriving_yojson_runtime.error_or

  val clos_to_yojson : clos -> Yojson.Safe.t

  val clos_of_yojson :
    Yojson.Safe.t -> clos Ppx_deriving_yojson_runtime.error_or

  type env = env_item list

  val pp_env :
    Ppx_deriving_runtime.Format.formatter -> env -> Ppx_deriving_runtime.unit

  val show_env : env -> Ppx_deriving_runtime.string

  val equal_env : env -> env -> Ppx_deriving_runtime.bool

  val env_to_yojson : env -> Yojson.Safe.t

  val env_of_yojson : Yojson.Safe.t -> env Ppx_deriving_yojson_runtime.error_or

  type stack = stack_item list

  val pp_stack :
    Ppx_deriving_runtime.Format.formatter -> stack -> Ppx_deriving_runtime.unit

  val show_stack : stack -> Ppx_deriving_runtime.string

  val equal_stack : stack -> stack -> Ppx_deriving_runtime.bool

  val stack_to_yojson : stack -> Yojson.Safe.t

  val stack_of_yojson :
    Yojson.Safe.t -> stack Ppx_deriving_yojson_runtime.error_or

  type interpreter_input = zinc_code * env * stack

  val pp_interpreter_input :
    Ppx_deriving_runtime.Format.formatter ->
    interpreter_input ->
    Ppx_deriving_runtime.unit

  val show_interpreter_input : interpreter_input -> Ppx_deriving_runtime.string

  val equal_interpreter_input :
    interpreter_input -> interpreter_input -> Ppx_deriving_runtime.bool

  val interpreter_input_to_yojson : interpreter_input -> Yojson.Safe.t

  val interpreter_input_of_yojson :
    Yojson.Safe.t -> interpreter_input Ppx_deriving_yojson_runtime.error_or

  type interpreter_output = Success of env * stack | Failure of string

  val pp_interpreter_output :
    Ppx_deriving_runtime.Format.formatter ->
    interpreter_output ->
    Ppx_deriving_runtime.unit

  val show_interpreter_output :
    interpreter_output -> Ppx_deriving_runtime.string

  val equal_interpreter_output :
    interpreter_output -> interpreter_output -> Ppx_deriving_runtime.bool

  val interpreter_output_to_yojson : interpreter_output -> Yojson.Safe.t

  val interpreter_output_of_yojson :
    Yojson.Safe.t -> interpreter_output Ppx_deriving_yojson_runtime.error_or

  val generalize_zinc_instruction : zinc_instruction_code -> 'a zinc_instruction

  val generalize_zinc : zinc_code -> 'a zinc

  type interpreter_context = { get_contract_opt : address -> contract option }
end

module Make_zinc (Desc : Zinc_description) : Zinc = struct
  type address = Desc.address
  [@@deriving show { with_path = false }, eq, yojson]

  type hash = Desc.hash [@@deriving show { with_path = false }, eq, yojson]

  type key = Desc.key [@@deriving show { with_path = false }, eq, yojson]

  type contract = string * Desc.address option
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
    | Num of Zinc_utils.Z.t
    | Add
    (* boolean *)
    | Bool of bool
    | Eq
    (* misc *)
    | String of string
    (* Crypto *)
    | Key of Desc.key
    | HashKey
    | Hash of Desc.hash
    (* serialization *)
    | Bytes of bytes
    (*
     ===========================
     tezos_specific instructions
     ===========================
  *)
    | Address of Desc.address
    | ChainID
    | Contract_opt
    | MakeTransaction
    | Mutez of Zinc_utils.Z.t
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

  and operation =
    | Transaction of Zinc_utils.Z.t * contract (* todo: add parameter *)
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

  let rec generalize_zinc_instruction :
            'a. zinc_instruction_code -> 'a zinc_instruction =
    map_zinc_instruction Nothing.unreachable_code

  and generalize_zinc : 'a. zinc_code -> 'a zinc =
    map_zinc Nothing.unreachable_code

  type interpreter_context = {
    get_contract_opt : Desc.address -> contract option;
  }
  (* TODO: get_contract_opt needs to accept a type too *)
end
