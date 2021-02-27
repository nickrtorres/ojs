%{
%}

%start program
%token VAR PLUS EQ MINUS EOF
%token <bool>   BOOL
%token <string> IDEN
%token <float>  NUM
%type <Types.program> program
%%
program            : source_elements EOF             { $1                          }
  ;

source_elements    : source_element                 { Types.SourceElement($1)      }
                   | source_elements source_element { Types.SourceElements($1, $2) }
  ;

source_element     : stmt                           { Types.Stmt($1)               }
  ;

stmt               : var_stmt                       { Types.VarStmt($1)            }
                   | expr_stmt                      { Types.ExprStmt($1)           }
  ;

var_stmt           : VAR IDEN EQ assign_expr        { Types.VarDeclaration($2, $4) }
  ;

expr_stmt          : assign_expr                     { $1                          }
  ;

assign_expr        : conditional_expr               { Types.ConditionalExpr($1)    }
  ;

conditional_expr   : logical_or_expr                { Types.LogicalOrExpr($1)      }
  ;

logical_or_expr    : logical_and_expr               { Types.LogicalAndExpr($1)     }
  ;

logical_and_expr   : bitwise_or_expr                { Types.BitwiseOrExpr($1)      }
  ;

bitwise_or_expr    : bitwise_xor_expr               { Types.BitwiseXorExpr($1)     }
  ;

bitwise_xor_expr   : bitwise_and_expr               { Types.BitwiseAndExpr($1)     }
  ;

bitwise_and_expr   : equality_expr                  { Types.EqualityExpr($1)       }
  ;

equality_expr      : relational_expr                { Types.RelationalExpr($1)     }
  ;

relational_expr    : shift_expr                     { Types.ShiftExpr($1)          }
  ;

shift_expr         : add_expr                       { Types.AddExpr($1)            }
  ;

add_expr           : mult_expr                      { Types.MultExpr($1)           }
                   | add_expr PLUS mult_expr        { Types.Add($1, $3)            }
                   | add_expr MINUS mult_expr       { Types.Sub($1, $3)            }
  ;

mult_expr          : unary_expr                     { Types.UnaryExpr($1)          }
  ;

unary_expr         : postfix_expr                   { Types.PostfixExpr($1)        }
  ;

postfix_expr       : lhs_expr                       { Types.LhsExpr($1)            }
  ;

lhs_expr           : new_expr                       { Types.NewExpr($1)            }
  ;

new_expr           : member_expr                    { Types.MemberExpr($1)         }
  ;

member_expr        : primary_expr                   { Types.PrimaryExpr($1)        }
  ;

primary_expr       : literal                        { Types.Literal($1)            }
                   | identifier                     { Types.Identifier($1)         }
  ;

literal            : NUM                            { Types.BaseTyNumber($1)       }
                   | BOOL                           { Types.BaseTyBoolean($1)      }
  ;

identifier         : IDEN                           { $1                           }
  ;
%%

