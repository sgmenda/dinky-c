use crate::parser::symbols;

pub type RegisterName = String;
pub type LabelName = String;
pub type ConstantInteger = u32;

/// Pseudo-assembly instructions
#[derive(Clone, Debug, PartialEq)]
pub enum AsmInstructions {
    ///     .globl  _foo        ; .globl is an assembler directive, it makes this function visible to the linker
    /// _foo:
    FunctionDecl(String),
    ///     rax = constant
    Integer(ConstantInteger),
    ///     ret
    Return,
    ///     rax = -rax
    Negation,
    ///     rax = !rax
    BitwiseComplement,
    ///     rax = (rax == 0)
    LogicalNegation,
    ///     push    r1
    Push(RegisterName),
    ///     pop     r1
    Pop(RegisterName),
    ///     r2 = r1 + r2
    Add(RegisterName, RegisterName),
    ///     r2 = r2 - r1
    ///     rax = r2
    Subtract(RegisterName, RegisterName),
    ///     rax = r1 * rax
    Multiply(RegisterName),
    ///     rax = rax / r1
    Divide(RegisterName),
    ///     rax = (r1 == r2)
    Equals(RegisterName, RegisterName),
    ///     rax = (r1 != r2)
    NotEquals(RegisterName, RegisterName),
    ///     rax = (r1 <= r2)
    LessThanEquals(RegisterName, RegisterName),
    ///     rax = (r1 < r2)
    LessThan(RegisterName, RegisterName),
    ///     rax = (r1 >= r2)
    GreaterThanEquals(RegisterName, RegisterName),
    ///     rax = (r1 > r2)
    GreaterThan(RegisterName, RegisterName),
    ///     if (r1 == constant) jump to label
    JumpIfEqualToNumber(ConstantInteger, RegisterName, LabelName),
    ///     if (r1 != constant) jump to label
    JumpIfNotEqualToNumber(ConstantInteger, RegisterName, LabelName),
    ///     rax = constant
    ///     jump to label
    SetResultToNumberAndJump(ConstantInteger, LabelName),
    ///     rax = (r1 == constant)
    NotEqualToNumber(ConstantInteger, RegisterName),
    /// label:
    InsertLabel(LabelName),
}

pub struct CodeGenerator {
    /// counter is used to assign labels
    counter: u32,
}
impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator { counter: 0 }
    }
    fn get_label(&mut self, prefix: String) -> String {
        self.counter += 1;
        format!("{}_{}", prefix, self.counter)
    }
    pub fn generate_code(&mut self, ast: symbols::ASTNode) -> Result<Vec<AsmInstructions>, String> {
        match ast {
            symbols::ASTNode::Program { fn_ptr } => self.generate_code(*fn_ptr),
            symbols::ASTNode::Function { fn_name, stmt_ptr } => {
                let mut decl_code = vec![AsmInstructions::FunctionDecl(fn_name.name)];
                match self.generate_code(*stmt_ptr) {
                    Ok(stmt_code) => {
                        decl_code.extend(stmt_code);
                        Ok(decl_code)
                    }
                    Err(e) => Err(e),
                }
            }
            symbols::ASTNode::Statement(stmt_kind) => match stmt_kind {
                symbols::StatementKind::Return { expr_ptr } => {
                    match self.generate_code(*expr_ptr) {
                        Ok(factor_code) => {
                            let return_code = vec![AsmInstructions::Return];
                            let mut factor_code = factor_code;
                            factor_code.extend(return_code);
                            Ok(factor_code)
                        }
                        Err(e) => Err(e),
                    }
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!("Unrecognized Statement {:?}", stmt_kind)),
            },
            symbols::ASTNode::Expression(expr_kind) => match expr_kind {
                symbols::ExpressionKind::LogicalOrExpression { lor_expr_ptr } => {
                    self.generate_code(*lor_expr_ptr)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!("Unrecognized Expression {:?}", expr_kind)),
            },
            symbols::ASTNode::LogicalOrExpression(lor_expr_kind) => match lor_expr_kind {
                symbols::LogicalOrExpressionKind::LogicalAndExpression { land_expr_ptr } => {
                    self.generate_code(*land_expr_ptr)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!("Unrecognized Expression {:?}", lor_expr_kind)),
            },
            symbols::ASTNode::LogicalAndExpression(land_expr_kind) => match land_expr_kind {
                symbols::LogicalAndExpressionKind::EqualityExpression { eq_expr_ptr } => {
                    self.generate_code(*eq_expr_ptr)
                }
                symbols::LogicalAndExpressionKind::LogicalOrBinaryOperator {
                    operator,
                    land_expr1_ptr,
                    land_expr2_ptr,
                } => {
                    let mut land_expr_code = vec![];
                    let land_expr1_code = self.generate_code(*land_expr1_ptr)?;
                    let land_expr2_code = self.generate_code(*land_expr2_ptr)?;

                    assert_eq!(operator, symbols::LogicalOrBinaryOperator::DoublePipe);

                    let clause2_label = self.get_label("_clause2".to_string());
                    let end_label = self.get_label("_end".to_string());

                    // This puts the result of expr1 in rax
                    land_expr_code.extend(land_expr1_code);
                    // If the result of the first expression is false, jump to _clause2
                    land_expr_code.extend(vec![AsmInstructions::JumpIfEqualToNumber(
                        0,
                        "rax".to_string(),
                        clause2_label.clone(),
                    )]);
                    // Otherwise, the result of the first expression was true, and we can return true.
                    land_expr_code.extend(vec![AsmInstructions::SetResultToNumberAndJump(
                        1,
                        end_label.clone(),
                    )]);
                    land_expr_code.extend(vec![AsmInstructions::InsertLabel(clause2_label)]);
                    // This puts the result of expr2 in rax
                    land_expr_code.extend(land_expr2_code);
                    // Compare rax to 0
                    land_expr_code.extend(vec![AsmInstructions::NotEqualToNumber(
                        0,
                        "rax".to_string(),
                    )]);
                    land_expr_code.extend(vec![AsmInstructions::InsertLabel(end_label)]);
                    Ok(land_expr_code)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!(
                    "Unrecognized LogicalAndExpression {:?}",
                    land_expr_kind
                )),
            },
            symbols::ASTNode::EqualityExpression(eq_expr_kind) => match eq_expr_kind {
                symbols::EqualityExpressionKind::RelationalExpression { rel_expr_ptr } => {
                    self.generate_code(*rel_expr_ptr)
                }
                symbols::EqualityExpressionKind::LogicalAndBinaryOperator {
                    operator,
                    eq_expr1_ptr,
                    eq_expr2_ptr,
                } => {
                    let mut land_expr_code = vec![];
                    let eq_expr1_code = self.generate_code(*eq_expr1_ptr)?;
                    let eq_expr2_code = self.generate_code(*eq_expr2_ptr)?;

                    assert_eq!(operator, symbols::LogicalAndBinaryOperator::DoubleAmpersand);

                    let clause2_label = self.get_label("_clause2".to_string());
                    let end_label = self.get_label("_end".to_string());

                    // This puts the result of expr1 in rax
                    land_expr_code.extend(eq_expr1_code);
                    // If the result of the first expression is true (ie non-zero), jump to _clause2
                    land_expr_code.extend(vec![AsmInstructions::JumpIfNotEqualToNumber(
                        0,
                        "rax".to_string(),
                        clause2_label.clone(),
                    )]);
                    // Otherwise, the result of the first expression was false, and we can return false.
                    // TODO: We can do a naked jump here, we are promised that the result register is 0.
                    land_expr_code.extend(vec![AsmInstructions::SetResultToNumberAndJump(
                        0,
                        end_label.clone(),
                    )]);
                    land_expr_code.extend(vec![AsmInstructions::InsertLabel(clause2_label)]);
                    // This puts the result of expr2 in rax
                    land_expr_code.extend(eq_expr2_code);
                    // Compare rax to 0
                    land_expr_code.extend(vec![AsmInstructions::NotEqualToNumber(
                        0,
                        "rax".to_string(),
                    )]);
                    land_expr_code.extend(vec![AsmInstructions::InsertLabel(end_label)]);
                    Ok(land_expr_code)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!(
                    "Unrecognized EqualityExpression {:?}",
                    eq_expr_kind
                )),
            },
            symbols::ASTNode::RelationalExpression(rel_expr_kind) => match rel_expr_kind {
                symbols::RelationalExpressionKind::AdditiveExpression { add_expr_ptr } => {
                    self.generate_code(*add_expr_ptr)
                }
                symbols::RelationalExpressionKind::EqualityBinaryOperator {
                    operator,
                    rel_expr1_ptr,
                    rel_expr2_ptr,
                } => {
                    let mut rel_expr_code = vec![];
                    let rel_expr1_code = self.generate_code(*rel_expr1_ptr)?;
                    let rel_expr2_code = self.generate_code(*rel_expr2_ptr)?;
                    rel_expr_code.extend(rel_expr1_code);
                    rel_expr_code.extend(vec![AsmInstructions::Push("rax".to_string())]);
                    rel_expr_code.extend(rel_expr2_code);
                    rel_expr_code.extend(vec![AsmInstructions::Pop("rcx".to_string())]);
                    match operator {
                        symbols::EqualityBinaryOperator::DoubleEquals => {
                            rel_expr_code.extend(vec![AsmInstructions::Equals(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                        symbols::EqualityBinaryOperator::BangEquals => {
                            rel_expr_code.extend(vec![AsmInstructions::NotEquals(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                    }
                    Ok(rel_expr_code)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!(
                    "Unrecognized RelationalExpression {:?}",
                    rel_expr_kind
                )),
            },
            symbols::ASTNode::AdditiveExpression(add_expr_kind) => match add_expr_kind {
                symbols::AdditiveExpressionKind::Term { term_ptr } => self.generate_code(*term_ptr),
                symbols::AdditiveExpressionKind::RelationalBinaryOperator {
                    operator,
                    add_expr1_ptr,
                    add_expr2_ptr,
                } => {
                    let mut rel_expr_code = vec![];
                    let add_expr1_code = self.generate_code(*add_expr1_ptr)?;
                    let add_expr2_code = self.generate_code(*add_expr2_ptr)?;
                    rel_expr_code.extend(add_expr1_code);
                    rel_expr_code.extend(vec![AsmInstructions::Push("rax".to_string())]);
                    rel_expr_code.extend(add_expr2_code);
                    rel_expr_code.extend(vec![AsmInstructions::Pop("rcx".to_string())]);
                    match operator {
                        symbols::RelationalBinaryOperator::LessThanEquals => {
                            rel_expr_code.extend(vec![AsmInstructions::LessThanEquals(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                        symbols::RelationalBinaryOperator::LessThan => {
                            rel_expr_code.extend(vec![AsmInstructions::LessThan(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                        symbols::RelationalBinaryOperator::GreaterThanEquals => {
                            rel_expr_code.extend(vec![AsmInstructions::GreaterThanEquals(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                        symbols::RelationalBinaryOperator::GreaterThan => {
                            rel_expr_code.extend(vec![AsmInstructions::GreaterThan(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                    }
                    Ok(rel_expr_code)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!(
                    "Unrecognized AdditiveExpression {:?}",
                    add_expr_kind
                )),
            },
            symbols::ASTNode::Term(term_kind) => match term_kind {
                symbols::TermKind::Factor { factor_ptr } => self.generate_code(*factor_ptr),
                symbols::TermKind::LowerBinaryOperator {
                    operator,
                    term1_ptr,
                    term2_ptr,
                } => {
                    let mut term_code = vec![];
                    let term1_code = self.generate_code(*term1_ptr)?;
                    let term2_code = self.generate_code(*term2_ptr)?;
                    term_code.extend(term1_code);
                    term_code.extend(vec![AsmInstructions::Push("rax".to_string())]);
                    term_code.extend(term2_code);
                    term_code.extend(vec![AsmInstructions::Pop("rcx".to_string())]);
                    match operator {
                        symbols::LowerBinaryOperator::Addition => {
                            term_code.extend(vec![AsmInstructions::Add(
                                "rcx".to_string(),
                                "rax".to_string(),
                            )]);
                        }
                        symbols::LowerBinaryOperator::Subtraction => {
                            term_code.extend(vec![AsmInstructions::Subtract(
                                "rax".to_string(),
                                "rcx".to_string(),
                            )]);
                        }
                    }
                    Ok(term_code)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!("Unrecognized factor {:?}", term_kind)),
            },
            symbols::ASTNode::Factor(factor_kind) => match factor_kind {
                symbols::FactorKind::ParenthesizedFactor { factor_ptr } => {
                    self.generate_code(*factor_ptr)
                }
                symbols::FactorKind::Constant { value: x } => Ok(vec![AsmInstructions::Integer(x)]),
                symbols::FactorKind::UnaryOperator {
                    operator,
                    factor_ptr,
                } => match operator {
                    symbols::UnaryOperator::Negation => {
                        let mut postfix = vec![AsmInstructions::Negation];
                        let mut factor_code = self.generate_code(*factor_ptr)?;
                        factor_code.append(&mut postfix);
                        Ok(factor_code)
                    }
                    symbols::UnaryOperator::BitwiseComplement => {
                        let mut postfix = vec![AsmInstructions::BitwiseComplement];
                        let mut factor_code = self.generate_code(*factor_ptr)?;
                        factor_code.append(&mut postfix);
                        Ok(factor_code)
                    }
                    symbols::UnaryOperator::LogicalNegation => {
                        let mut postfix = vec![AsmInstructions::LogicalNegation];
                        let mut factor_code = self.generate_code(*factor_ptr)?;
                        factor_code.append(&mut postfix);
                        Ok(factor_code)
                    }
                    #[allow(unreachable_patterns)]
                    _ => Err(format!("Unrecognized Unary Operator {:?}", operator)),
                },
                symbols::FactorKind::HigherBinaryOperator {
                    operator,
                    factor1_ptr,
                    factor2_ptr,
                } => {
                    let mut factor_code = vec![];
                    let factor1_code = self.generate_code(*factor1_ptr)?;
                    let factor2_code = self.generate_code(*factor2_ptr)?;
                    factor_code.extend(factor2_code);
                    factor_code.extend(vec![AsmInstructions::Push("rax".to_string())]);
                    factor_code.extend(factor1_code);
                    factor_code.extend(vec![AsmInstructions::Pop("rcx".to_string())]);
                    match operator {
                        symbols::HigherBinaryOperator::Multiplication => {
                            factor_code.extend(vec![AsmInstructions::Multiply("rcx".to_string())]);
                        }
                        symbols::HigherBinaryOperator::Division => {
                            factor_code.extend(vec![AsmInstructions::Divide("rcx".to_string())]);
                        }
                    }
                    Ok(factor_code)
                }
                #[allow(unreachable_patterns)]
                _ => Err(format!("Unrecognized Factor {:?}", factor_kind)),
            },
            #[allow(unreachable_patterns)]
            _ => Err(format!("Unrecognized ASTNode {:?}", ast)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer;
    use crate::parser::parser;
    use insta::assert_debug_snapshot;

    #[test]
    fn test_codegen_simple_main() -> Result<(), String> {
        let input = String::from("int main() {return 0;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_negation() -> Result<(), String> {
        let input = String::from("int main() {return -110;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_logical_negation() -> Result<(), String> {
        let input = String::from("int main() {return !10;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }
    #[test]
    fn test_codegen_main_addition() -> Result<(), String> {
        let input = String::from("int main() {return 1+2;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_multiplication() -> Result<(), String> {
        let input = String::from("int main() {return 2*4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_division() -> Result<(), String> {
        let input = String::from("int main() {return 2/4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_double_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 == 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_bang_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 != 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_less_than_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 <= 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_less_than() -> Result<(), String> {
        let input = String::from("int main() {return 2 < 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_greater_than_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 >= 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_greater_than() -> Result<(), String> {
        let input = String::from("int main() {return 2 > 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_logical_or() -> Result<(), String> {
        let input = String::from("int main() {return 2 || 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }

    #[test]
    fn test_codegen_main_logical_and() -> Result<(), String> {
        let input = String::from("int main() {return 2 && 4;}");
        let tokens = lexer::lex(&input)?;
        let ast = parser::parse(tokens)?;
        assert_debug_snapshot!(CodeGenerator::new().generate_code(ast));
        Ok(())
    }
}
