(* 11.1 -- FIXME: incomplete *)
type primary_expr = Literal of js_type | Identifier of string

(* 11.2 -- FIXME: incomplete *)
and member_expr = PrimaryExpr of primary_expr

and new_expr = MemberExpr of member_expr

and arguments = assign_expr list

and call_expr = member_expr * arguments

and lhs_expr = NewExpr of new_expr | CallExpr of call_expr

(* 11.3 -- FIXME: incomplete *)
and postfix_expr = LhsExpr of lhs_expr

(* 11.4 -- FIXME: incomplete *)
and unary_expr = PostfixExpr of postfix_expr

(* 11.5 -- FIXME: incomplete *)
and mult_expr = UnaryExpr of unary_expr

(* 11.6 *)
and add_expr =
  | MultExpr of mult_expr
  | Add of add_expr * mult_expr
  | Sub of add_expr * mult_expr

(* 11.7 -- FIXME incomplete *)
and shift_expr = AddExpr of add_expr

(* 11.8 *)
and relational_expr =
  | ShiftExpr of shift_expr
  | LtExpr of relational_expr * shift_expr
  | GtExpr of relational_expr * shift_expr
  | LteExpr of relational_expr * shift_expr
  | GteExpr of relational_expr * shift_expr

(* 11.9 -- FIXME incomplete *)
and equality_expr = RelationalExpr of relational_expr

(* 11.10 -- FIXME incomplete *)
and bitwise_and_expr = EqualityExpr of equality_expr

and bitwise_xor_expr = BitwiseAndExpr of bitwise_and_expr

and bitwise_or_expr = BitwiseXorExpr of bitwise_xor_expr

(* 11.10 -- FIXME incomplete *)
and logical_and_expr = BitwiseOrExpr of bitwise_or_expr

(* 11.11 -- FIXME incomplete *)
and logical_or_expr = LogicalAndExpr of logical_and_expr

(* 11.12 -- FIXME conditional_expr *)
and conditional_expr = LogicalOrExpr of logical_or_expr

(* 11.13 -- FIXME incomplete *)
and assign_expr =
  | ConditionalExpr of conditional_expr
  | SimpleAssignExpr of lhs_expr * assign_op * assign_expr

and assign_op = Assign

and expression = assign_expr

(* 14 -- FIXME incomplete *)
and var_stmt = VarDeclaration of string * assign_expr

(* NB: print is non-standard but can be seen in the og impl of JS. *)
and stmt =
  | VarStmt of var_stmt
  | ExprStmt of expression
  | IterationStmt of iteration_stmt
  | PrintStmt of expression
  | IfStmt of expression * stmt * stmt option
  | Block of block

(* 12.6 *)
and iteration_stmt = WhileStmt of expression * stmt

and block = stmt list

and function_declaration = string * string list * block

and source_element =
  | Stmt of stmt
  | FunctionDeclaration of function_declaration

and source_elements =
  | SourceElements of source_elements * source_element
  | SourceElement of source_element

and program = source_elements

(* 10 -- FIXME incomplete *)
and execution_ctx = { var_object : (string, js_type) Hashtbl.t }

and completion = Normal of js_type option | Abrupt

and js_type =
  | TyBoolean of bool
  | TyNumber of float
  | TyUndefined
  | TyReference of js_type * string
  | TyString of string
  | TyObject of js_object

and js_object = {
  properties : (string, js_type) Hashtbl.t;
  prototype : js_object option;
  call : block option;
}

type attribute = ReadOnly | DontEnum | DontDelete | Internal

type formal_parameters = string list

let mk_execution_ctx () = { var_object = Hashtbl.create 100 }

let new_object () =
  { properties = Hashtbl.create 100; prototype = None; call = None }

let new_object_with_prototype prototype =
  { properties = Hashtbl.create 100; prototype = Some prototype; call = None }

(* 8.6.2.1 *)
let rec get name obj =
  match Hashtbl.find_opt obj.properties name with
  | Some value -> value
  | None -> (
      match obj.prototype with
      | Some proto -> get name proto
      | None -> TyUndefined)

let with_call block obj = { obj with call = block }

let js_object_call _args _obj = TyUndefined

(* FIXME 8.6.2.3 *)
let can_put _name _obj = true

let put name value obj =
  if can_put name obj then Hashtbl.replace obj.properties name value else ()

let with_property name value obj =
  let () = put name value obj in
  obj

let has_property name obj =
  let rec lookup = function
    | None -> false
    | Some proto -> Hashtbl.mem proto.properties name || lookup proto.prototype
  in
  Hashtbl.mem obj.properties name || lookup obj.prototype

let rec string_of_ty = function
  | TyNumber n -> Printf.sprintf "%g" n
  | TyBoolean b -> string_of_bool b
  | TyUndefined -> "undefined"
  | TyReference (r, _) -> string_of_ty r
  | TyString s -> s
  | TyObject _ -> failwith "todo"

(* 9.1 ToPrimitive FIXME handle objects *)
let primitive_of_ty ty = ty

(* 9.2 ToBoolean *)
let rec bool_of_ty = function
  | TyNumber n -> n != 0.
  | TyBoolean b -> b
  | TyUndefined -> false
  | TyReference (r, _) -> bool_of_ty r
  | TyString s -> String.length s <> 0
  | TyObject _ -> failwith "todo"

let rec number_of_ty = function
  | TyUndefined -> Float.nan
  | TyBoolean true -> 1.
  | TyBoolean false -> 0.
  | TyNumber n -> n
  | TyString s -> float_of_string s
  | TyReference (r, _) -> number_of_ty r
  | TyObject _ -> failwith "todo"

let string_of_completion = function
  | Normal (Some ty) -> string_of_ty ty
  | Normal None -> "undefined"
  | Abrupt -> "abrupt"

(* Tests *)
let%test "add properties" =
  let obj = new_object () in
  let () = put "foo" (TyNumber 42.) obj in
  get "foo" obj = TyNumber 42.

let%test "update properties" =
  let obj = new_object () in
  let () = put "foo" (TyNumber 100.) obj in
  let () = put "foo" (TyNumber 42.) obj in
  get "foo" obj = TyNumber 42.

let%test "has property" =
  let obj = new_object () in
  let () = put "foo" (TyNumber 42.) obj in
  has_property "foo" obj

let%test "prototype properties" =
  let a = new_object () in
  let b = new_object_with_prototype a in
  let c = new_object_with_prototype b in
  let () = put "foo" (TyNumber 100.) a in
  has_property "foo" c
