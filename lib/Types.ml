module Z = struct
  include Z

  let to_yojson x = `String (Z.to_string x)

  let of_yojson = function
    | `String s -> Result.Ok (Z.of_string s)
    | _ -> Result.Error "JSON string expected"

  let pp : Format.formatter -> t -> unit =
   fun fmt v -> Format.fprintf fmt "%s" (Z.to_string v)
end

type label = Label of string
[@@deriving show { with_path = false }, eq, yojson]

module LMap = struct
  include Map.Make (struct
    type t = label

    let compare = compare
  end)

  type 'a association_list = (label * 'a) list [@@deriving yojson]

  let of_list (lst : 'a association_list) : 'a t =
    let aux prev (k, v) = add k v prev in
    List.fold_left aux empty lst

  let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
      =
   fun value ppf m ->
    let lst = bindings m in
    let lst =
      Base.List.dedup_and_sort
        ~compare:(fun (Label a, _) (Label b, _) -> String.compare a b)
        lst
    in
    let new_pp ppf (k, v) =
      Format.fprintf ppf "@[<h>%a -> %a@]" pp_label k value v
    in
    Format.fprintf ppf "%a"
      (Format.pp_print_list
         ?pp_sep:(Some (fun fmt _ -> Format.pp_print_string fmt ", "))
         new_pp)
      lst

  let of_yojson a m = Result.map of_list (association_list_of_yojson a m)

  let to_yojson a m = bindings m |> association_list_to_yojson a
end

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
  | MakeRecord of label list
  | RecordAccess of label
  (* math *)
  | Num of Z.t
  | Add
  (* boolean *)
  | Bool of bool
  | Eq
  (* Crypto *)
  | Key of string
  | HashKey
  | Hash of
      (Digestif.BLAKE2B.t
      [@to_yojson fun digest -> `String (Digestif.BLAKE2B.to_raw_string digest)]
      [@of_yojson
        function
        | `String digest -> Ok (Digestif.BLAKE2B.of_raw_string digest)
        | _ -> Error "string expected"])
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
[@@deriving show { with_path = false }, eq, yojson]

and zinc = zinc_instruction list
[@@deriving show { with_path = false }, eq, yojson]

type program = (string * zinc) list
[@@deriving show { with_path = false }, eq, yojson]

type env_item =
  [ `Z of zinc_instruction | `Clos of clos | `Record of stack_item LMap.t ]
[@@deriving show, eq, yojson]

and stack_item =
  [ (* copied from env_item *)
    `Z of zinc_instruction
  | `Clos of clos
  | `Record of stack_item LMap.t
  | (* marker to note function calls *)
    `Marker of zinc * env_item list ]
[@@deriving show, eq]

and clos = { code : zinc; env : env_item list } [@@deriving show, eq, yojson]

type env = env_item list [@@deriving show, eq, yojson]

type stack = stack_item list [@@deriving show, eq, yojson]

type zinc_state = zinc * env * stack [@@deriving show, eq, yojson]
