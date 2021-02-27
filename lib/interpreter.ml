open Types

let rec eval expr = assign expr

and assign = function ConditionalExpr expr -> conditional expr

and conditional = function LogicalOrExpr expr -> logical_or expr

and logical_or = function LogicalAndExpr expr -> logical_and expr

and logical_and = function BitwiseOrExpr expr -> bitwise_or expr

and bitwise_or = function BitwiseXorExpr expr -> bitwise_xor expr

and bitwise_xor = function BitwiseAndExpr expr -> bitwise_and expr

and bitwise_and = function EqualityExpr expr -> equality expr

and equality = function RelationalExpr expr -> relational expr

and relational = function ShiftExpr expr -> shift expr

and shift = function AddExpr expr -> add expr

and add = function
  | MultExpr expr -> mult expr
  | Add (lhs, rhs) -> (
      let lhs = add lhs in
      let rhs = mult rhs in
      match (lhs, rhs) with
      | BaseTyNumber lhs, BaseTyNumber rhs -> BaseTyNumber (lhs +. rhs)
      | _ -> failwith "todo!")
  | Sub (_, _) -> failwith "todo!"

and mult = function UnaryExpr expr -> unary expr

and unary = function PostfixExpr expr -> postfix expr

and postfix = function LhsExpr expr -> lhs expr

and lhs = function NewExpr expr -> newe expr

and newe = function MemberExpr expr -> member expr

and member = function PrimaryExpr expr -> primary expr

and primary = function Literal ty -> ty

let add_var ctx iden value =
  let () = Hashtbl.add ctx.var_object iden value in
  ctx

let exec stmt ctx =
  match stmt with
  | VarStmt (VarDeclaration (iden, expr)) ->
      let value = eval expr in
      add_var ctx iden value

let%test "basic add" =
  let lhs =
    MultExpr
      (UnaryExpr
         (PostfixExpr
            (LhsExpr
               (NewExpr (MemberExpr (PrimaryExpr (Literal (BaseTyNumber 10.))))))))
  in
  let rhs =
    UnaryExpr
      (PostfixExpr
         (LhsExpr
            (NewExpr (MemberExpr (PrimaryExpr (Literal (BaseTyNumber 32.)))))))
  in
  let expr = Add (lhs, rhs) in
  add expr = BaseTyNumber 42.

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
                                                      (Literal
                                                         (BaseTyNumber 42.))))))))))))))))))
  in
  let stmt = VarStmt (VarDeclaration (iden, expr)) in
  let ctx = { var_object = Hashtbl.create 100 } in
  let updated = exec stmt ctx in
  Hashtbl.find updated.var_object iden = BaseTyNumber 42.
