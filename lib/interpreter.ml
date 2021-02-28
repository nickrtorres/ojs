open Types

let add_var ctx iden value = Hashtbl.add ctx.var_object iden value

(* FIXME undefined if var does not exist *)
let update_var ctx iden value = Hashtbl.replace ctx.var_object iden value

let eval expr ctx =
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
            let () = update_var ctx iden value in
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
    | LtExpr (left, right) ->
        (* FIXME : implement abstract relational comparison algorithm *)
        let left =
          match relational left with
          | TyNumber n | TyReference (TyNumber n, _) -> n
          | _ -> failwith "todo!"
        in
        let right =
          match shift right with
          | TyNumber n | TyReference (TyNumber n, _) -> n
          | _ -> failwith "todo!"
        in
        TyBoolean (left < right)
  and shift = function AddExpr expr -> add expr
  and add = function
    | MultExpr expr -> mult expr
    | Add (lhs, rhs) -> (
        let lhs = add lhs in
        let rhs = mult rhs in
        match (lhs, rhs) with
        | TyNumber lhs, TyNumber rhs -> TyNumber (lhs +. rhs)
        | TyReference (TyNumber value, _), TyNumber rhs ->
            TyNumber (value +. rhs)
        | _ -> failwith "todo!")
    | Sub (_, _) -> failwith "todo!"
  and mult = function UnaryExpr expr -> unary expr
  and unary = function PostfixExpr expr -> postfix expr
  and postfix = function LhsExpr expr -> lhs expr
  and lhs = function NewExpr expr -> newe expr
  and newe = function MemberExpr expr -> member expr
  and member = function PrimaryExpr expr -> primary expr
  and primary = function
    | Literal ty -> ty
    (* FIXME "The result of an identifier is always a value of type Reference" *)
    | Identifier iden -> TyReference (Hashtbl.find ctx.var_object iden, iden)
  in

  assign expr

let rec exec stmt ctx =
  match stmt with
  | VarStmt (VarDeclaration (iden, expr)) ->
      let value = eval expr ctx in
      let () = add_var ctx iden value in
      Normal None
  | ExprStmt expr ->
      let value = eval expr ctx in
      Normal (Some value)
  | IterationStmt (WhileStmt (cond, body)) ->
      let rec loop cond =
        if eval cond ctx |> bool_of_base_ty then
          let _ = exec body ctx in
          loop cond
        else Normal None
      in
      loop cond

let declare fn _ctx =
  let _iden, args, _block = fn in
  let string_of_args xs =
    let rec string_of_args' xs acc =
      match xs with
      | [] -> acc
      | [ x ] -> acc ^ x
      | hd :: tl -> string_of_args' tl (acc ^ hd ^ ",")
    in
    string_of_args' xs ""
  in
  let _p = string_of_args args in
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
    | _ -> failwith "todo!"
  in

  run' program

let%test "basic var" =
  let iden = "foo" in
  let expr =
    ConditionalExpr
      (LogicalOrExpr
         (LogicalAndExpr
            (BitwiseOrExpr
               (BitwiseXorExpr
                  (BitwiseAndExpr
                     (EqualityExpr
                        (RelationalExpr
                           (ShiftExpr
                              (AddExpr
                                 (MultExpr
                                    (UnaryExpr
                                       (PostfixExpr
                                          (LhsExpr
                                             (NewExpr
                                                (MemberExpr
                                                   (PrimaryExpr
                                                      (Literal (TyNumber 42.))))))))))))))))))
  in
  let stmt = VarStmt (VarDeclaration (iden, expr)) in
  let ctx = { var_object = Hashtbl.create 100 } in
  let _ = exec stmt ctx in
  Hashtbl.find ctx.var_object iden = TyNumber 42.

let%test "basic var" =
  let iden = "foo" in
  let addend =
    MultExpr
      (UnaryExpr
         (PostfixExpr
            (LhsExpr
               (NewExpr (MemberExpr (PrimaryExpr (Literal (TyNumber 10.))))))))
  in
  let augend =
    UnaryExpr
      (PostfixExpr
         (LhsExpr (NewExpr (MemberExpr (PrimaryExpr (Literal (TyNumber 32.)))))))
  in
  let expr =
    ConditionalExpr
      (LogicalOrExpr
         (LogicalAndExpr
            (BitwiseOrExpr
               (BitwiseXorExpr
                  (BitwiseAndExpr
                     (EqualityExpr
                        (RelationalExpr
                           (ShiftExpr (AddExpr (Add (addend, augend)))))))))))
  in
  let stmt = VarStmt (VarDeclaration (iden, expr)) in
  let ctx = { var_object = Hashtbl.create 100 } in
  let _ = exec stmt ctx in
  Hashtbl.find ctx.var_object iden = TyNumber 42.
