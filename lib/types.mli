type js_base_type = BaseTyBoolean of bool | BaseTyNumber of float

type js_type = TyBase of js_base_type | TyReference of js_base_type

(* 11.1 -- FIXME: incomplete *)
type primary_expr = Literal of js_base_type

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
type assign_expr = ConditionalExpr of conditional_expr

(* 14 -- FIXME incomplete *)
type var_stmt = VarDeclaration of string * assign_expr

type stmt = VarStmt of var_stmt

type source_element = Stmt of stmt

type program = SourceElement of source_element

type js_object = (string, js_base_type) Hashtbl.t

(* 10 -- FIXME incomplete *)
type execution_ctx = { var_object : js_object }
