type address = string

val pp_address :
  Ppx_deriving_runtime.Format.formatter -> address -> Ppx_deriving_runtime.unit

val show_address : address -> Ppx_deriving_runtime.string

val equal_address : address -> address -> Ppx_deriving_runtime.bool

val address_to_yojson : address -> Yojson.Safe.t

val address_of_yojson :
  Yojson.Safe.t -> address Ppx_deriving_yojson_runtime.error_or

type contract = string * address option

val pp_contract :
  Ppx_deriving_runtime.Format.formatter -> contract -> Ppx_deriving_runtime.unit

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
  | Key of string
  | HashKey
  | Hash of string
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
  Ppx_deriving_runtime.Format.formatter -> program -> Ppx_deriving_runtime.unit

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
  Ppx_deriving_runtime.Format.formatter -> env_item -> Ppx_deriving_runtime.unit

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

val clos_of_yojson : Yojson.Safe.t -> clos Ppx_deriving_yojson_runtime.error_or

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

val show_interpreter_output : interpreter_output -> Ppx_deriving_runtime.string

val equal_interpreter_output :
  interpreter_output -> interpreter_output -> Ppx_deriving_runtime.bool

val interpreter_output_to_yojson : interpreter_output -> Yojson.Safe.t

val interpreter_output_of_yojson :
  Yojson.Safe.t -> interpreter_output Ppx_deriving_yojson_runtime.error_or

val generalize_zinc_instruction : zinc_instruction_code -> 'a zinc_instruction

val generalize_zinc : zinc_code -> 'a zinc

type interpreter_context = { get_contract_opt : address -> contract option }

module Utils : sig
  val unit_record : [> `Record of 'a Zinc_utils.LMap.t ]
end
