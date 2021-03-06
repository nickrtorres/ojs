open Types

let eval expr ctx =
  (* 11.8.5 *)
  let abs_relational_comp left right =
    match (primitive_of_ty left, primitive_of_ty right) with
    | TyString left, TyString right -> Some (left < right)
    | left, right -> (
        match (number_of_ty left, number_of_ty right) with
        | n, _ when Float.is_nan n -> None
        | _, n when Float.is_nan n -> None
        | 0.0, 0.0 -> Some false
        | inf, _ when inf = Float.infinity -> Some false
        | _, inf when inf = Float.infinity -> Some true
        | inf, _ when inf = Float.neg_infinity -> Some true
        | _, inf when inf = Float.neg_infinity -> Some false
        | left, right -> Some (left < right))
  in
  let rec assign = function
    | ConditionalExpr expr -> conditional expr
    | SimpleAssignExpr (iden, op, value) -> (
        let iden =
          match lhs iden with
          | TyReference (_, iden) -> iden
          | _ -> failwith "TODO!"
        in
        let value = assign value in
        match op with
        | Assign ->
            (* TODO FIXME undefined variable *)
            let () = Ctx.put iden value ctx in
            value)
  and conditional = function LogicalOrExpr expr -> logical_or expr
  and logical_or = function LogicalAndExpr expr -> logical_and expr
  and logical_and = function BitwiseOrExpr expr -> bitwise_or expr
  and bitwise_or = function BitwiseXorExpr expr -> bitwise_xor expr
  and bitwise_xor = function BitwiseAndExpr expr -> bitwise_and expr
  and bitwise_and = function EqualityExpr expr -> equality expr
  and equality = function RelationalExpr expr -> relational expr
  and relational = function
    | ShiftExpr expr -> shift expr
    | LtExpr (left, right) -> (
        match abs_relational_comp (relational left) (shift right) with
        | None | Some false -> TyBoolean false
        | Some true -> TyBoolean true)
    | GtExpr (left, right) -> (
        match abs_relational_comp (shift right) (relational left) with
        | None | Some false -> TyBoolean false
        | Some true -> TyBoolean true)
    | LteExpr (left, right) -> (
        match abs_relational_comp (shift right) (relational left) with
        | None | Some true -> TyBoolean false
        | Some false -> TyBoolean true)
    | GteExpr (left, right) -> (
        match abs_relational_comp (relational left) (shift right) with
        | None | Some true -> TyBoolean false
        | Some false -> TyBoolean true)
  and shift = function AddExpr expr -> add expr
  and add = function
    | MultExpr expr -> mult expr
    | Add (lhs, rhs) ->
        let lhs = add lhs in
        let rhs = mult rhs in
        TyNumber (number_of_ty lhs +. number_of_ty rhs)
    | Sub (lhs, rhs) ->
        let lhs = add lhs in
        let rhs = mult rhs in
        TyNumber (number_of_ty lhs -. number_of_ty rhs)
  and mult = function UnaryExpr expr -> unary expr
  and unary = function PostfixExpr expr -> postfix expr
  and postfix = function LhsExpr expr -> lhs expr
  and lhs = function NewExpr expr -> newe expr | CallExpr expr -> call expr
  and call expr =
    let meme, args = expr in
    let _args = List.map assign args in
    let _obj =
      match member meme with
      | TyObject obj -> obj
      | _ -> failwith "not an object!"
    in
    failwith "todo"
  and newe = function MemberExpr expr -> member expr
  and member = function PrimaryExpr expr -> primary expr
  and primary = function
    | Literal ty -> ty
    | Identifier iden -> TyReference (Ctx.get iden ctx, iden)
  in

  assign expr

let exec stmt ctx =
  let rec exec' = function
    | VarStmt (VarDeclaration (iden, expr)) ->
        let value = eval expr ctx in
        let () = Ctx.put iden value ctx in
        Normal None
    | ExprStmt expr ->
        let value = eval expr ctx in
        Normal (Some value)
    | IterationStmt (WhileStmt (cond, body)) ->
        let rec loop cond =
          if eval cond ctx |> bool_of_ty then
            let _ = exec' body in
            loop cond
          else Normal None
        in
        loop cond
    | PrintStmt expr ->
        let () = eval expr ctx |> string_of_ty |> print_endline in
        Normal None
    | IfStmt (cond, if_stmt, else_stmt) -> (
        if eval cond ctx |> bool_of_ty then exec' if_stmt
        else
          match else_stmt with Some stmt -> exec' stmt | None -> Normal None)
    | Block stmts -> stmt_list stmts
  and stmt_list = function
    | [] -> Normal None
    | [ stmt ] -> exec' stmt
    | hd :: tl -> (
        match exec' hd with Normal _ -> stmt_list tl | Abrupt -> Abrupt)
  in

  exec' stmt

let declare fn _ctx =
  let _iden, args, block = fn in
  let _fn_obj =
    JsObject.new_object ()
    |> JsObject.with_call (Some block)
    |> JsObject.with_property "length"
         (TyNumber (List.length args |> float_of_int))
  in
  ()

let run program ctx =
  let rec run' = function
    | SourceElement (Stmt stmt) -> exec stmt ctx
    | SourceElement (FunctionDeclaration fn) ->
        declare fn ctx;
        Normal None
    | SourceElements (se, Stmt stmt) ->
        (* FIXME how to return the first Completion? *)
        let _ = run' se in
        exec stmt ctx
    | SourceElements (se, FunctionDeclaration fn) ->
        (* FIXME how to return the first Completion? *)
        let _ = run' se in
        let () = declare fn ctx in
        Normal None
  in

  run' program
