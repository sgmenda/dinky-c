use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(id: {})", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    Return {
        expr_ptr: Box<ASTNode>,
    },
    Assign {
        variable: Identifier,
        optional_expr_ptr: Option<Box<ASTNode>>,
    },
    Expression {
        expr_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for StatementKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementKind::Return { expr_ptr } => write!(f, "RETURN {}", expr_ptr),
            StatementKind::Assign {
                variable,
                optional_expr_ptr,
            } => match optional_expr_ptr {
                Some(expr_ptr) => write!(f, "INT {} = {}", variable, expr_ptr),
                None => write!(f, "INT {}", variable),
            },
            StatementKind::Expression { expr_ptr } => write!(f, "{}", expr_ptr),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOperator::Negation => write!(f, "-"),
            UnaryOperator::BitwiseComplement => write!(f, "~"),
            UnaryOperator::LogicalNegation => write!(f, "!"),
        }
    }
}

/// Lower precedence binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum LowerBinaryOperator {
    Addition,
    Subtraction,
}

impl fmt::Display for LowerBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LowerBinaryOperator::Addition => write!(f, "+"),
            LowerBinaryOperator::Subtraction => write!(f, "-"),
        }
    }
}

/// Higher precedence binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum HigherBinaryOperator {
    Multiplication,
    Division,
}

impl fmt::Display for HigherBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HigherBinaryOperator::Multiplication => write!(f, "*"),
            HigherBinaryOperator::Division => write!(f, "/"),
        }
    }
}

/// Relational binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum RelationalBinaryOperator {
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl fmt::Display for RelationalBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelationalBinaryOperator::LessThan => write!(f, "<"),
            RelationalBinaryOperator::LessThanEquals => write!(f, "<="),
            RelationalBinaryOperator::GreaterThan => write!(f, ">"),
            RelationalBinaryOperator::GreaterThanEquals => write!(f, ">="),
        }
    }
}

/// Equality binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum EqualityBinaryOperator {
    DoubleEquals,
    BangEquals,
}

impl fmt::Display for EqualityBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EqualityBinaryOperator::DoubleEquals => write!(f, "=="),
            EqualityBinaryOperator::BangEquals => write!(f, "!="),
        }
    }
}

/// Logical and binary operator
#[derive(Clone, Debug, PartialEq)]
pub enum LogicalAndBinaryOperator {
    DoubleAmpersand,
}

impl fmt::Display for LogicalAndBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicalAndBinaryOperator::DoubleAmpersand => write!(f, "&&"),
        }
    }
}

/// Logical or binary operator
#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOrBinaryOperator {
    DoublePipe,
}

impl fmt::Display for LogicalOrBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicalOrBinaryOperator::DoublePipe => write!(f, "||"),
        }
    }
}

/// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionKind {
    LogicalOrExpression { lor_expr_ptr: Box<ASTNode> },
}

impl fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExpressionKind::LogicalOrExpression { lor_expr_ptr } => write!(f, "{}", lor_expr_ptr),
        }
    }
}

/// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOrExpressionKind {
    LogicalAndExpression { land_expr_ptr: Box<ASTNode> },
}

impl fmt::Display for LogicalOrExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicalOrExpressionKind::LogicalAndExpression { land_expr_ptr } => {
                write!(f, "{}", land_expr_ptr)
            }
        }
    }
}

/// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
#[derive(Clone, Debug, PartialEq)]
pub enum LogicalAndExpressionKind {
    EqualityExpression {
        eq_expr_ptr: Box<ASTNode>,
    },
    LogicalOrBinaryOperator {
        operator: LogicalOrBinaryOperator,
        land_expr1_ptr: Box<ASTNode>,
        land_expr2_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for LogicalAndExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicalAndExpressionKind::EqualityExpression { eq_expr_ptr } => {
                write!(f, "{}", eq_expr_ptr)
            }
            LogicalAndExpressionKind::LogicalOrBinaryOperator {
                operator,
                land_expr1_ptr,
                land_expr2_ptr,
            } => write!(f, "({} {} {})", land_expr1_ptr, operator, land_expr2_ptr),
        }
    }
}

/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
#[derive(Clone, Debug, PartialEq)]
pub enum EqualityExpressionKind {
    RelationalExpression {
        rel_expr_ptr: Box<ASTNode>,
    },
    LogicalAndBinaryOperator {
        operator: LogicalAndBinaryOperator,
        eq_expr1_ptr: Box<ASTNode>,
        eq_expr2_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for EqualityExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EqualityExpressionKind::RelationalExpression { rel_expr_ptr } => {
                write!(f, "{}", rel_expr_ptr)
            }
            EqualityExpressionKind::LogicalAndBinaryOperator {
                operator,
                eq_expr1_ptr,
                eq_expr2_ptr,
            } => write!(f, "({} {} {})", eq_expr1_ptr, operator, eq_expr2_ptr),
        }
    }
}

/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
#[derive(Clone, Debug, PartialEq)]
pub enum RelationalExpressionKind {
    AdditiveExpression {
        add_expr_ptr: Box<ASTNode>,
    },
    EqualityBinaryOperator {
        operator: EqualityBinaryOperator,
        rel_expr1_ptr: Box<ASTNode>,
        rel_expr2_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for RelationalExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelationalExpressionKind::AdditiveExpression { add_expr_ptr } => {
                write!(f, "{}", add_expr_ptr)
            }
            RelationalExpressionKind::EqualityBinaryOperator {
                operator,
                rel_expr1_ptr,
                rel_expr2_ptr,
            } => write!(f, "({} {} {})", rel_expr1_ptr, operator, rel_expr2_ptr),
        }
    }
}

/// <additive exp> ::= <term> { ("+"|"-") <term> }
#[derive(Clone, Debug, PartialEq)]
pub enum AdditiveExpressionKind {
    Term {
        term_ptr: Box<ASTNode>,
    },
    RelationalBinaryOperator {
        operator: RelationalBinaryOperator,
        add_expr1_ptr: Box<ASTNode>,
        add_expr2_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for AdditiveExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AdditiveExpressionKind::Term { term_ptr } => write!(f, "{}", term_ptr),
            AdditiveExpressionKind::RelationalBinaryOperator {
                operator,
                add_expr1_ptr,
                add_expr2_ptr,
            } => write!(f, "({} {} {})", add_expr1_ptr, operator, add_expr2_ptr),
        }
    }
}

/// <term> ::= <factor> { ("*"|"/") <factor> }
#[derive(Clone, Debug, PartialEq)]
pub enum TermKind {
    Factor {
        factor_ptr: Box<ASTNode>,
    },
    LowerBinaryOperator {
        operator: LowerBinaryOperator,
        term1_ptr: Box<ASTNode>,
        term2_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for TermKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TermKind::Factor { factor_ptr } => write!(f, "{}", factor_ptr),
            TermKind::LowerBinaryOperator {
                operator,
                term1_ptr,
                term2_ptr,
            } => write!(f, "({} {} {})", term1_ptr, operator, term2_ptr),
        }
    }
}

/// <factor> ::= <int> | <unary op> <factor> | <lparen> <factor> <rparen>
#[derive(Clone, Debug, PartialEq)]
pub enum FactorKind {
    Constant {
        value: u32,
    },
    ParenthesizedFactor {
        factor_ptr: Box<ASTNode>,
    },
    UnaryOperator {
        operator: UnaryOperator,
        factor_ptr: Box<ASTNode>,
    },
    HigherBinaryOperator {
        operator: HigherBinaryOperator,
        factor1_ptr: Box<ASTNode>,
        factor2_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for FactorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FactorKind::Constant { value } => write!(f, "{}", value),
            FactorKind::ParenthesizedFactor { factor_ptr } => write!(f, "{}", factor_ptr),
            FactorKind::UnaryOperator {
                operator,
                factor_ptr,
            } => write!(f, "({} {})", operator, factor_ptr),
            FactorKind::HigherBinaryOperator {
                operator,
                factor1_ptr,
                factor2_ptr,
            } => write!(f, "({} {} {})", factor1_ptr, operator, factor2_ptr),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTNode {
    Factor(FactorKind),
    Term(TermKind),
    AdditiveExpression(AdditiveExpressionKind),
    RelationalExpression(RelationalExpressionKind),
    EqualityExpression(EqualityExpressionKind),
    LogicalAndExpression(LogicalAndExpressionKind),
    LogicalOrExpression(LogicalOrExpressionKind),
    Expression(ExpressionKind),
    Statement(StatementKind),
    Function {
        fn_name: Identifier,
        stmt_ptr: Box<ASTNode>,
    },
    Program {
        fn_ptr: Box<ASTNode>,
    },
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ASTNode::Factor(factor_kind) => write!(f, "{}", factor_kind),
            ASTNode::Term(term_kind) => write!(f, "{}", term_kind),
            ASTNode::AdditiveExpression(add_expr) => write!(f, "{}", add_expr),
            ASTNode::RelationalExpression(rel_expr) => write!(f, "{}", rel_expr),
            ASTNode::EqualityExpression(eq_expr) => write!(f, "{}", eq_expr),
            ASTNode::LogicalAndExpression(land_expr) => write!(f, "{}", land_expr),
            ASTNode::LogicalOrExpression(lor_expr) => write!(f, "{}", lor_expr),
            ASTNode::Expression(expr) => write!(f, "{}", expr),
            ASTNode::Statement(stmt) => write!(f, "{}", stmt),
            ASTNode::Function { fn_name, stmt_ptr } => {
                let return_type = "int";
                let params = "()";
                let mut aligned_fn_body = String::new();
                for line in format!("{}", stmt_ptr).lines() {
                    aligned_fn_body.push_str(format!("\t\t{}", line).as_str());
                }
                write!(
                    f,
                    "FN {}\n\
                     \treturns: {}\n\
                     \tparams: {}\n\
                     \tbody:\n\
                     {}",
                    fn_name, return_type, params, aligned_fn_body
                )
            }
            ASTNode::Program { fn_ptr } => write!(f, "{}", fn_ptr),
        }
    }
}
