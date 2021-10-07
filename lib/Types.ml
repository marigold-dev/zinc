module Z = struct
include Z
let to_yojson x = `String (Z.to_string x)
let of_yojson = function
  | `String s -> Ppx_deriving_yojson_runtime.Result.Ok (Z.of_string s) 
  | _ -> Ppx_deriving_yojson_runtime.Result.Error "JSON string expected" 
end


type label = Label of string [@@deriving show { with_path = false }, eq, yojson]

type zinc_instruction =
  (* ====================
     zinc core operations
     ====================
  *)
  | Grab
  | Return
  | PushRetAddr of zinc
  | Apply
  | Access of int
  | Closure of zinc
  | EndLet
  (*
     ===============
     zinc extensions
     ===============
  *)
  (* ASTs *)
  | MakeRecord of
      ((label))
      list
  | RecordAccess of
      (label)
  (* math *)
  | Num of
      (Z.t
      [@printer fun fmt v -> fprintf fmt "%s" (Z.to_string v)])
  (*
  | Add
  (* boolean *)
  | Bool of bool
  | Eq
  (* Crypto 
  | Key of string
  | HashKey
  | Hash of
      (Digestif.BLAKE2B.t
      [@to_yojson fun digest -> `String (Digestif.BLAKE2B.to_raw_string digest)]
      [@of_yojson
        function
        | `String digest -> Ok (Digestif.BLAKE2B.of_raw_string digest)
        | _ -> Error "string expected"])*)
  (* serialization *)
  | Bytes of bytes
  (*
  Thinking of replacing pack/unpack with this
  | Ty of ty
  | Set_global
  | Get_global
  *)
  (* tezos_specific operations *)
  | Address of string
  | ChainID
  (* Random handling stuff (need to find a better way to do that) *)
  | Done
  *)
[@@deriving show { with_path = false }, eq, yojson]

and zinc = zinc_instruction list
[@@deriving show { with_path = false }, eq, yojson]

type program = (string * zinc) list
[@@deriving show { with_path = false }, eq, yojson]
(*

type env_item =
  [ `Z of zinc_instruction
  | `Clos of clos
  | `Record of
    (stack_item label_map
    [@printer label_map_printer fprintf pp_stack_item]
    [@equal Stage_common_types.Types.LMap.equal equal_stack_item]) ] 
[@@deriving show, eq, yojson]

and stack_item =
  [ (* copied from env_item *)
    `Z of zinc_instruction
  | `Clos of clos
  | `Record of
    (stack_item label_map
    [@printer label_map_printer fprintf pp_stack_item]
    [@equal Stage_common_types.Types.LMap.equal equal_stack_item])
  | (* marker to note function calls *)
    `Marker of zinc * env_item list ]
[@@deriving show, eq]

and clos = { code : zinc; env : env_item list } [@@deriving show, eq, yojson]


type env = env_item list [@@deriving show, eq, yojson]

type stack = stack_item list [@@deriving show, eq, yojson]

type zinc_state = zinc * env * stack  [@@deriving show, eq, yojson]
*)
