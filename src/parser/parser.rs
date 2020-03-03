use super::symbols::*;
use crate::lexer::lexer;

/// Return Ok(()) and advances the token stream by 1 if the next token is the
/// expected token, else returns Err(string explaining the error).
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn expect_token(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    token: &lexer::Token,
) -> Result<(), String> {
    match token_stream.peek() {
        Some(t) if t == &token => {
            token_stream.next();
            Ok(())
        }
        c => Err(format!("Expected {:?}, found {:?}.", token, c)),
    }
}

/// Returns an UnaryOperator and advances token stream if found.
/// <unary op> = '!' | '~' | '-'
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
fn parse_unary_operator(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<UnaryOperator, String> {
    match token_stream.peek() {
        Some(lexer::Token::Minus) => {
            token_stream.next();
            Ok(UnaryOperator::Negation)
        }
        Some(lexer::Token::BitwiseComplement) => {
            token_stream.next();
            Ok(UnaryOperator::BitwiseComplement)
        }
        Some(lexer::Token::LogicalNegation) => {
            token_stream.next();
            Ok(UnaryOperator::LogicalNegation)
        }
        t => Err(format!("Expected a unary operator, found {:?}.", t)),
    }
}

/// Returns an ASTNode::Factor.
/// <factor> ::= <int> | <unary op> <factor> | <lparen> <exp> <rparen>
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_factor(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::Integer(x)) => {
            let expr = ASTNode::Factor(FactorKind::Constant { value: *x });
            token_stream.next();
            Ok(expr)
        }
        Some(lexer::Token::OpenParenthesis) => {
            token_stream.next();
            let inner_factor = parse_expr(token_stream)?;
            expect_token(token_stream, &&lexer::Token::CloseParenthesis)?;
            let factor = ASTNode::Factor(FactorKind::ParenthesizedFactor {
                factor_ptr: Box::from(inner_factor),
            });
            Ok(factor)
        }
        _ => {
            let operator = parse_unary_operator(token_stream)?;
            let inner_factor = parse_factor(token_stream)?;
            let factor = ASTNode::Factor(FactorKind::UnaryOperator {
                operator,
                factor_ptr: Box::new(inner_factor),
            });
            Ok(factor)
        }
        #[allow(unreachable_patterns)]
        c => Err(format!("Cannot parse factor {:?}", c)),
    }
}

/// Returns an ASTNode::Term by parsing the pattern
/// ```
/// { ("*"|"/") <factor> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_many_multiplicative_binary_operators(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    first_factor: ASTNode,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::Asterisk) => {
            token_stream.next();
            let next_factor = parse_factor(token_stream)?;
            let factor = ASTNode::Factor(FactorKind::HigherBinaryOperator {
                operator: HigherBinaryOperator::Multiplication,
                factor1_ptr: Box::from(first_factor),
                factor2_ptr: Box::from(next_factor),
            });
            parse_many_multiplicative_binary_operators(token_stream, factor)
        }
        Some(lexer::Token::Slash) => {
            token_stream.next();
            let next_factor = parse_factor(token_stream)?;
            let factor = ASTNode::Factor(FactorKind::HigherBinaryOperator {
                operator: HigherBinaryOperator::Division,
                factor1_ptr: Box::from(first_factor),
                factor2_ptr: Box::from(next_factor),
            });
            parse_many_multiplicative_binary_operators(token_stream, factor)
        }
        _ => match first_factor {
            ASTNode::Factor(factor) => {
                let term = ASTNode::Term(TermKind::Factor {
                    factor_ptr: Box::from(ASTNode::Factor(factor)),
                });
                Ok(term)
            }
            c => Err(format!("Expected ASTNode::Factor, found {:?}", c)),
        },
        #[allow(unreachable_patterns)]
        c => Err(format!(
            "Cannot parse higher precedence binary operator {:?}",
            c
        )),
    }
}

/// Returns an ASTNode::Term by parsing
/// ```
/// <term> ::= <factor> { ("*"|"/") <factor> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_term(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let first_factor = parse_factor(token_stream)?;
    parse_many_multiplicative_binary_operators(token_stream, first_factor)
}

/// Returns an ASTNode::AdditiveExpression by parsing the pattern
/// ```
/// { ("+"|"-") <term> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_many_additive_binary_operators(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    first_term: ASTNode,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::Plus) => {
            token_stream.next();
            let next_term = parse_term(token_stream)?;
            let term = ASTNode::Term(TermKind::LowerBinaryOperator {
                operator: LowerBinaryOperator::Addition,
                term1_ptr: Box::from(first_term),
                term2_ptr: Box::from(next_term),
            });
            parse_many_additive_binary_operators(token_stream, term)
        }
        Some(lexer::Token::Minus) => {
            token_stream.next();
            let next_term = parse_term(token_stream)?;
            let term = ASTNode::Term(TermKind::LowerBinaryOperator {
                operator: LowerBinaryOperator::Subtraction,
                term1_ptr: Box::from(first_term),
                term2_ptr: Box::from(next_term),
            });
            parse_many_additive_binary_operators(token_stream, term)
        }
        _ => match first_term {
            ASTNode::Term(term) => {
                let expr = ASTNode::AdditiveExpression(AdditiveExpressionKind::Term {
                    term_ptr: Box::from(ASTNode::Term(term)),
                });
                Ok(expr)
            }
            c => Err(format!("Expected ASTNode::Term, found {:?}", c)),
        },
        #[allow(unreachable_patterns)]
        c => Err(format!(
            "Cannot parse lower precedence binary operator {:?}",
            c
        )),
    }
}

/// Returns an ASTNode::AdditiveExpression by parsing
/// ```
/// <additive exp> ::= <term> { ("+"|"-") <term> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_additive_expr(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let first_term = parse_term(token_stream)?;
    parse_many_additive_binary_operators(token_stream, first_term)
}

/// Returns an ASTNode::RelationalExpression by parsing the pattern
/// ```
/// { ("<"|">"|"<="|">=") <additive exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
/// * `first_add_expr` - the first <additive exp>
///
pub fn parse_many_relational_binary_operators(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    first_add_expr: ASTNode,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::LessThan) => {
            token_stream.next();
            let next_add_expr = parse_additive_expr(token_stream)?;
            let add_expr =
                ASTNode::AdditiveExpression(AdditiveExpressionKind::RelationalBinaryOperator {
                    operator: RelationalBinaryOperator::LessThan,
                    add_expr1_ptr: Box::from(first_add_expr),
                    add_expr2_ptr: Box::from(next_add_expr),
                });
            parse_many_relational_binary_operators(token_stream, add_expr)
        }
        Some(lexer::Token::LessThanEquals) => {
            token_stream.next();
            let next_add_expr = parse_additive_expr(token_stream)?;
            let add_expr =
                ASTNode::AdditiveExpression(AdditiveExpressionKind::RelationalBinaryOperator {
                    operator: RelationalBinaryOperator::LessThanEquals,
                    add_expr1_ptr: Box::from(first_add_expr),
                    add_expr2_ptr: Box::from(next_add_expr),
                });
            parse_many_relational_binary_operators(token_stream, add_expr)
        }
        Some(lexer::Token::GreaterThan) => {
            token_stream.next();
            let next_add_expr = parse_additive_expr(token_stream)?;
            let add_expr =
                ASTNode::AdditiveExpression(AdditiveExpressionKind::RelationalBinaryOperator {
                    operator: RelationalBinaryOperator::GreaterThan,
                    add_expr1_ptr: Box::from(first_add_expr),
                    add_expr2_ptr: Box::from(next_add_expr),
                });
            parse_many_relational_binary_operators(token_stream, add_expr)
        }
        Some(lexer::Token::GreaterThanEquals) => {
            token_stream.next();
            let next_add_expr = parse_additive_expr(token_stream)?;
            let add_expr =
                ASTNode::AdditiveExpression(AdditiveExpressionKind::RelationalBinaryOperator {
                    operator: RelationalBinaryOperator::GreaterThanEquals,
                    add_expr1_ptr: Box::from(first_add_expr),
                    add_expr2_ptr: Box::from(next_add_expr),
                });
            parse_many_relational_binary_operators(token_stream, add_expr)
        }
        _ => match first_add_expr {
            ASTNode::AdditiveExpression(additive_expr) => {
                let expr =
                    ASTNode::RelationalExpression(RelationalExpressionKind::AdditiveExpression {
                        add_expr_ptr: Box::from(ASTNode::AdditiveExpression(additive_expr)),
                    });
                Ok(expr)
            }
            c => Err(format!(
                "Expected ASTNode::AdditiveExpression, found {:?}",
                c
            )),
        },
        #[allow(unreachable_patterns)]
        c => Err(format!("Cannot parse relational binary operator {:?}", c)),
    }
}

/// Returns an ASTNode::RelationalExpression by parsing
/// ```
/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_relational_expr(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let first_additive_expr = parse_additive_expr(token_stream)?;
    parse_many_relational_binary_operators(token_stream, first_additive_expr)
}

/// Returns an ASTNode::EqualityExpression by parsing the pattern
/// ```
/// { ("!=" | "==") <relational exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
/// * `first_rel_expr` - the first <relational exp>
///
pub fn parse_many_equality_binary_operators(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    first_rel_expr: ASTNode,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::DoubleEquals) => {
            token_stream.next();
            let next_rel_expr = parse_equality_expr(token_stream)?;
            let add_expr =
                ASTNode::RelationalExpression(RelationalExpressionKind::EqualityBinaryOperator {
                    operator: EqualityBinaryOperator::DoubleEquals,
                    rel_expr1_ptr: Box::from(first_rel_expr),
                    rel_expr2_ptr: Box::from(next_rel_expr),
                });
            parse_many_equality_binary_operators(token_stream, add_expr)
        }
        Some(lexer::Token::BangEquals) => {
            token_stream.next();
            let next_rel_expr = parse_equality_expr(token_stream)?;
            let add_expr =
                ASTNode::RelationalExpression(RelationalExpressionKind::EqualityBinaryOperator {
                    operator: EqualityBinaryOperator::BangEquals,
                    rel_expr1_ptr: Box::from(first_rel_expr),
                    rel_expr2_ptr: Box::from(next_rel_expr),
                });
            parse_many_equality_binary_operators(token_stream, add_expr)
        }
        _ => match first_rel_expr {
            ASTNode::RelationalExpression(rel_expr) => {
                let expr =
                    ASTNode::EqualityExpression(EqualityExpressionKind::RelationalExpression {
                        rel_expr_ptr: Box::from(ASTNode::RelationalExpression(rel_expr)),
                    });
                Ok(expr)
            }
            c => Err(format!(
                "Expected ASTNode::RelationalExpression, found {:?}",
                c
            )),
        },
        #[allow(unreachable_patterns)]
        c => Err(format!("Cannot parse equality binary operator {:?}", c)),
    }
}

/// Returns an ASTNode::EqualityExpression by parsing
/// ```
/// <equality exp> ::= <relational exp> { ("!=" | "==") <relational exp>
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_equality_expr(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let first_relational_expr = parse_relational_expr(token_stream)?;
    parse_many_equality_binary_operators(token_stream, first_relational_expr)
}

/// Returns an ASTNode::LogicalAndExpression by parsing the pattern
/// ```
/// { "&&" <equality exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
/// * `first_eq_expr` - the first <equality exp>
///
pub fn parse_many_logical_and_binary_operators(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    first_eq_expr: ASTNode,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::DoubleAmpersand) => {
            token_stream.next();
            let next_eq_expr = parse_equality_expr(token_stream)?;
            let eq_expr =
                ASTNode::EqualityExpression(EqualityExpressionKind::LogicalAndBinaryOperator {
                    operator: LogicalAndBinaryOperator::DoubleAmpersand,
                    eq_expr1_ptr: Box::from(first_eq_expr),
                    eq_expr2_ptr: Box::from(next_eq_expr),
                });
            parse_many_logical_and_binary_operators(token_stream, eq_expr)
        }
        _ => match first_eq_expr {
            ASTNode::EqualityExpression(eq_expr) => {
                let expr =
                    ASTNode::LogicalAndExpression(LogicalAndExpressionKind::EqualityExpression {
                        eq_expr_ptr: Box::from(ASTNode::EqualityExpression(eq_expr)),
                    });
                Ok(expr)
            }
            c => Err(format!(
                "Expected ASTNode::EqualityExpression, found {:?}",
                c
            )),
        },
        #[allow(unreachable_patterns)]
        c => Err(format!("Cannot parse equality binary operator {:?}", c)),
    }
}

/// Returns an ASTNode::LogicalAndExpression by parsing
/// ```
/// <logical and exp> ::= <equality exp> { "&&" <equality exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_logical_and_expr(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let first_equality_expr = parse_equality_expr(token_stream)?;
    parse_many_logical_and_binary_operators(token_stream, first_equality_expr)
}

/// Returns an ASTNode::LogicalOrExpression by parsing the pattern
/// ```
/// { "||" <logical and exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
/// * `first_land_expr` - the first <logical and exp>
///
pub fn parse_many_logical_or_binary_operators(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
    first_land_expr: ASTNode,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::DoublePipe) => {
            token_stream.next();
            let next_land_expr = parse_logical_and_expr(token_stream)?;
            let land_expr =
                ASTNode::LogicalAndExpression(LogicalAndExpressionKind::LogicalOrBinaryOperator {
                    operator: LogicalOrBinaryOperator::DoublePipe,
                    land_expr1_ptr: Box::from(first_land_expr),
                    land_expr2_ptr: Box::from(next_land_expr),
                });
            parse_many_logical_or_binary_operators(token_stream, land_expr)
        }
        _ => match first_land_expr {
            ASTNode::LogicalAndExpression(land_expr) => {
                let expr =
                    ASTNode::LogicalOrExpression(LogicalOrExpressionKind::LogicalAndExpression {
                        land_expr_ptr: Box::from(ASTNode::LogicalAndExpression(land_expr)),
                    });
                Ok(expr)
            }
            c => Err(format!(
                "Expected ASTNode::LogicalAndExpression, found {:?}",
                c
            )),
        },
        #[allow(unreachable_patterns)]
        c => Err(format!("Cannot parse equality binary operator {:?}", c)),
    }
}

/// Returns an ASTNode::LogicalOrExpression by parsing
/// ```
/// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_logical_or_expr(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let first_land_expr = parse_logical_and_expr(token_stream)?;
    parse_many_logical_or_binary_operators(token_stream, first_land_expr)
}

/// Returns an ASTNode::Expression by parsing
/// ```
/// <exp> ::= <logical-or-exp>
/// ```
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_expr(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let lor_expr = parse_logical_or_expr(token_stream)?;
    let expr = ASTNode::Expression(ExpressionKind::LogicalOrExpression {
        lor_expr_ptr: Box::from(lor_expr),
    });
    Ok(expr)
}

/// Returns an Identifier struct.
/// <id> = str
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_id(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<Identifier, String> {
    match token_stream.peek() {
        Some(lexer::Token::Identifier(string)) => {
            let cstring = string.clone();
            let id = Identifier {
                name: (*cstring).to_string(),
            };
            Ok(id)
        }
        c => Err(format!("Cannot parse id {:?}", c)),
    }
}

/// Returns a ASTNode::Statement.
/// <statement> ::= "return" <exp> ";"
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_stmt(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    // eat a return
    expect_token(token_stream, &&lexer::Token::RETURN)?;
    // look for <exp> SEMICOLON
    let expr = parse_expr(token_stream)?;
    let stmt = ASTNode::Statement(StatementKind::Return {
        expr_ptr: Box::new(expr),
    });
    expect_token(token_stream, &&lexer::Token::Semicolon)?;
    Ok(stmt)
}

/// Returns a ASTNode::Function
/// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_function(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    match token_stream.peek() {
        Some(lexer::Token::INT) => {
            token_stream.next();
            let id = parse_id(token_stream)?;
            token_stream.next();
            // eat "("
            expect_token(token_stream, &&lexer::Token::OpenParenthesis)?;
            // eat ")"
            expect_token(token_stream, &&lexer::Token::CloseParenthesis)?;
            // eat "{"
            expect_token(token_stream, &&lexer::Token::OpenBrace)?;
            // eat <statement>
            let stmt = parse_stmt(token_stream)?;
            let function = ASTNode::Function {
                fn_name: id,
                stmt_ptr: Box::new(stmt),
            };
            // eat "}"
            expect_token(token_stream, &&lexer::Token::CloseBrace)?;
            Ok(function)
        }
        c => Err(format!("Cannot parse function {:?}", c)),
    }
}

/// Returns a ASTNode::Program
/// <program> ::= <function>
///
/// # Arguments
///
/// * `token_stream` - peekable iterator of tokens
///
pub fn parse_program(
    token_stream: &mut std::iter::Peekable<std::slice::Iter<'_, lexer::Token>>,
) -> Result<ASTNode, String> {
    let function = parse_function(token_stream)?;
    let pgrm = ASTNode::Program {
        fn_ptr: Box::new(function),
    };
    Ok(pgrm)
}

/// Returns an abstract syntax tree.
///
/// # Arguments
///
/// * `tokens` - Vector of tokens generated by the lexer.
///
pub fn parse(tokens: Vec<lexer::Token>) -> Result<ASTNode, String> {
    // get a peekable iterator over the tokens
    let mut token_stream = tokens.iter().peekable();
    match token_stream.peek() {
        Some(lexer::Token::INT) => match parse_program(&mut token_stream) {
            Ok(prgm) => Ok(prgm),
            Err(e) => Err(e),
        },
        c => Err(format!("Cannot parse {:?}", c)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;
    use insta::assert_display_snapshot;

    #[test]
    fn test_parse_simple_main() -> Result<(), String> {
        let input = String::from("int main() { return 0; }");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_simple_main_error_0() -> Result<(), String> {
        let input = String::from("int main() { return 0; ");
        let tokens = lexer::lex(&input)?;
        assert_debug_snapshot!(parse(tokens));
        Ok(())
    }

    #[test]
    fn test_parse_simple_main_error_1() -> Result<(), String> {
        let input = String::from("int main() {1;}");
        let tokens = lexer::lex(&input)?;
        assert_debug_snapshot!(parse(tokens));
        Ok(())
    }

    #[test]
    fn test_parse_simple_main_error_2() -> Result<(), String> {
        let input = String::from("main() {return 0;}");
        let tokens = lexer::lex(&input)?;
        assert_debug_snapshot!(parse(tokens));
        Ok(())
    }

    #[test]
    fn test_parse_negation() -> Result<(), String> {
        let input = String::from("int main() { return -1; }");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_unary_operators() -> Result<(), String> {
        let input = String::from("int main() { return ~!-7; }");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_addition() -> Result<(), String> {
        let input = String::from("int main() {return 1+3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_many_additions() -> Result<(), String> {
        let input = String::from("int main() {return 1+2+3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_many_subtractions() -> Result<(), String> {
        let input = String::from("int main() {return 1-2-3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_many_multiplications() -> Result<(), String> {
        let input = String::from("int main() {return 1*2*3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_many_divisions() -> Result<(), String> {
        let input = String::from("int main() {return 1/2/3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_binary_op_precedence() -> Result<(), String> {
        let input = String::from("int main() {return 1+3*4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_binary_op_precedence_2() -> Result<(), String> {
        let input = String::from("int main() {return 1-3/4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_paren_precedence() -> Result<(), String> {
        let input = String::from("int main() {return 1*(2+3);}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_less_than_equal() -> Result<(), String> {
        let input = String::from("int main() {return 2 <= 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_less_than() -> Result<(), String> {
        let input = String::from("int main() {return 2 < 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_greater_than_equal() -> Result<(), String> {
        let input = String::from("int main() {return 2 >= 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_greater_than() -> Result<(), String> {
        let input = String::from("int main() {return 2 > 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_double_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 == 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_bang_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 != 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_logical_and() -> Result<(), String> {
        let input = String::from("int main() {return 2 && 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_logical_or() -> Result<(), String> {
        let input = String::from("int main() {return 2 || 3;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_logical_op_precedence_1() -> Result<(), String> {
        let input = String::from("int main() {return 1 || 0 && 0;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    #[test]
    fn test_parse_logical_op_precedence_2() -> Result<(), String> {
        let input = String::from("int main() {return (1 || 0) && 0;}");
        let tokens = lexer::lex(&input)?;
        let ast = parse(tokens)?;
        assert_display_snapshot!(ast);
        Ok(())
    }

    // #[test]
    // fn test_parse_assignment_1() -> Result<(), String> {
    //     let input = String::from("int main() { int x; return 0; }");
    //     let tokens = lexer::lex(&input)?;
    //     let ast = parse(tokens)?;
    //     assert_display_snapshot!(ast);
    //     Ok(())
    // }

    // #[test]
    // fn test_parse_assignment_2() -> Result<(), String> {
    //     let input = String::from("int main() { int x; x = 10; return x; }");
    //     let tokens = lexer::lex(&input)?;
    //     let ast = parse(tokens)?;
    //     assert_display_snapshot!(ast);
    //     Ok(())
    // }

    // #[test]
    // fn test_parse_assignment_3() -> Result<(), String> {
    //     let input = String::from("int main() { int x = 10; return x; }");
    //     let tokens = lexer::lex(&input)?;
    //     let ast = parse(tokens)?;
    //     assert_display_snapshot!(ast);
    //     Ok(())
    // }

    // #[test]
    // fn test_parse_assignment_4() -> Result<(), String> {
    //     let input = String::from("int main() { int x = 10; x = 12; return x; }");
    //     let tokens = lexer::lex(&input)?;
    //     let ast = parse(tokens)?;
    //     assert_display_snapshot!(ast);
    //     Ok(())
    // }

    // #[test]
    // fn test_parse_assignment_5() -> Result<(), String> {
    //     let input = String::from("int main() { int x = 10; x = x + 12; return x; }");
    //     let tokens = lexer::lex(&input)?;
    //     let ast = parse(tokens)?;
    //     assert_display_snapshot!(ast);
    //     Ok(())
    // }
}
