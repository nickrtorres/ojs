type js_base_type =
  | TyBoolean of bool
  | TyNumber of float
  | TyUndefined
  | TyReference of js_base_type * string

(* 11.1 -- FIXME: incomplete *)
type primary_expr = Literal of js_base_type | Identifier of string

(* 11.2 -- FIXME: incomplete *)
type member_expr = PrimaryExpr of primary_expr

type new_expr = MemberExpr of member_expr

type lhs_expr = NewExpr of new_expr

(* 11.3 -- FIXME: incomplete *)
type postfix_expr = LhsExpr of lhs_expr

(* 11.4 -- FIXME: incomplete *)
type unary_expr = PostfixExpr of postfix_expr

(* 11.5 -- FIXME: incomplete *)
type mult_expr = UnaryExpr of unary_expr

(* 11.6 *)
type add_expr =
  | MultExpr of mult_expr
  | Add of add_expr * mult_expr
  | Sub of add_expr * mult_expr

(* 11.7 -- FIXME incomplete *)
type shift_expr = AddExpr of add_expr

(* 11.8 -- FIXME incomplete *)
type relational_expr = ShiftExpr of shift_expr

(* 11.9 -- FIXME incomplete *)
type equality_expr = RelationalExpr of relational_expr

(* 11.10 -- FIXME incomplete *)
type bitwise_and_expr = EqualityExpr of equality_expr

type bitwise_xor_expr = BitwiseAndExpr of bitwise_and_expr

type bitwise_or_expr = BitwiseXorExpr of bitwise_xor_expr

(* 11.10 -- FIXME incomplete *)
type logical_and_expr = BitwiseOrExpr of bitwise_or_expr

(* 11.11 -- FIXME incomplete *)
type logical_or_expr = LogicalAndExpr of logical_and_expr

(* 11.12 -- FIXME conditional_expr *)
type conditional_expr = LogicalOrExpr of logical_or_expr

(* 11.13 -- FIXME incomplete *)
type assign_expr =
  | ConditionalExpr of conditional_expr
  | SimpleAssignExpr of lhs_expr * assign_op * assign_expr

and assign_op = Assign

type expression = assign_expr

(* 14 -- FIXME incomplete *)
type var_stmt = VarDeclaration of string * assign_expr

type stmt =
  | VarStmt of var_stmt
  | ExprStmt of expression
  | IterationStmt of iteration_stmt

(* 12.6 *)
and iteration_stmt = WhileStmt of expression * stmt

and block = stmt list

type function_declaration = string * string list * block

type source_element =
  | Stmt of stmt
  | FunctionDeclaration of function_declaration

type source_elements =
  | SourceElements of source_elements * source_element
  | SourceElement of source_element

type program = source_elements

(* 10 -- FIXME incomplete *)
type execution_ctx = { var_object : (string, js_base_type) Hashtbl.t }

let mk_execution_ctx () = { var_object = Hashtbl.create 100 }

type completion = Normal of js_base_type option

let rec string_of_base_ty = function
  | TyNumber n -> string_of_float n
  | TyBoolean b -> string_of_bool b
  | TyUndefined -> "undefined"
  | TyReference (r, _) -> string_of_base_ty r

(* 9.2 ToBoolean *)
let rec bool_of_base_ty = function
  | TyNumber n -> n != 0.
  | TyBoolean b -> b
  | TyUndefined -> false
  | TyReference (r, _) -> bool_of_base_ty r

let string_of_completion = function
  | Normal (Some base_ty) -> string_of_base_ty base_ty
  | Normal None -> "undefined"