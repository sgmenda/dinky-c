use crate::codegen::codegen;

pub fn generate_x86_64(asm_list: Vec<codegen::AsmInstructions>) -> Result<String, String> {
    let mut output = String::new();
    for asm in asm_list {
        match asm {
            codegen::AsmInstructions::FunctionDecl(name) => {
                output.push_str(format!("\t.globl _{}\n_{}:\n", name, name).as_str())
            }
            codegen::AsmInstructions::Integer(x) => {
                output.push_str(format!("\tmov\trax, {}\n", x).as_str())
            }
            codegen::AsmInstructions::Return => output.push_str("\tret\n"),
            codegen::AsmInstructions::Negation => output.push_str("\tneg\trax\n"),
            codegen::AsmInstructions::BitwiseComplement => output.push_str("\tnot\trax\n"),
            codegen::AsmInstructions::LogicalNegation => {
                let generated_asm = "\tcmp\trax, 0\n\
                                     \tmov\trax, 0\n\
                                     \tsete\tal\n";
                output.push_str(generated_asm);
            }
            codegen::AsmInstructions::Push(register) => {
                output.push_str(format!("\tpush\t{}\n", register).as_str())
            }
            codegen::AsmInstructions::Pop(register) => {
                output.push_str(format!("\tpop\t{}\n", register).as_str())
            }
            codegen::AsmInstructions::Add(r1, r2) => {
                output.push_str(format!("\tadd\t{}, {}\n", r2, r1).as_str())
            }
            codegen::AsmInstructions::Subtract(r1, r2) => output.push_str(
                format!(
                    "\tsub\t{}, {}\n\
                     \tmov\trax, {}\n",
                    r2, r1, r2
                )
                .as_str(),
            ),
            // For now, we will assume that there is no overflow.
            codegen::AsmInstructions::Multiply(r1) => {
                output.push_str(format!("\timul\t{}\n", r1).as_str())
            }
            // cqo sign-extends the quad-word rax to the octa-word rdx:rax
            codegen::AsmInstructions::Divide(r1) => output.push_str(
                format!(
                    "\tcqo\n\
                     \tidiv\t{}\n",
                    r1,
                )
                .as_str(),
            ),
            codegen::AsmInstructions::Equals(r1, r2) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsete\tal\n",
                    r1, r2
                )
                .as_str(),
            ),
            codegen::AsmInstructions::NotEquals(r1, r2) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsetne\tal\n",
                    r1, r2
                )
                .as_str(),
            ),
            codegen::AsmInstructions::LessThan(r1, r2) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsetl\tal\n",
                    r2, r1
                )
                .as_str(),
            ),
            codegen::AsmInstructions::LessThanEquals(r1, r2) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsetle\tal\n",
                    r2, r1
                )
                .as_str(),
            ),
            codegen::AsmInstructions::GreaterThan(r1, r2) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsetg\tal\n",
                    r2, r1
                )
                .as_str(),
            ),
            codegen::AsmInstructions::GreaterThanEquals(r1, r2) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsetge\tal\n",
                    r2, r1
                )
                .as_str(),
            ),
            codegen::AsmInstructions::JumpIfEqualToNumber(number, register, label) => output
                .push_str(
                    format!(
                        "\tcmp\t{}, {}\n\
                         \tje\t{}\n",
                        register, number, label
                    )
                    .as_str(),
                ),
            codegen::AsmInstructions::JumpIfNotEqualToNumber(number, register, label) => output
                .push_str(
                    format!(
                        "\tcmp\t{}, {}\n\
                         \tjne\t{}\n",
                        register, number, label
                    )
                    .as_str(),
                ),
            codegen::AsmInstructions::SetResultToNumberAndJump(number, label) => output.push_str(
                format!(
                    "\tmov\trax, {}\n\
                     \tjmp\t{}\n",
                    number, label
                )
                .as_str(),
            ),
            codegen::AsmInstructions::NotEqualToNumber(constant, register) => output.push_str(
                format!(
                    "\tcmp\t{}, {}\n\
                     \tmov\trax, 0\n\
                     \tsetne\tal\n",
                    register, constant
                )
                .as_str(),
            ),
            codegen::AsmInstructions::InsertLabel(label) => {
                output.push_str(format!("{}:\n", label).as_str())
            }
            #[allow(unreachable_patterns)]
            _ => return Err(format!("Cannot generate x86_64 for {:?}", asm)),
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;
    use crate::parser;
    use insta::assert_display_snapshot;

    #[test]
    fn test_x86_64_simple_main() -> Result<(), String> {
        let input = String::from("int main() {return 1;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_negation() -> Result<(), String> {
        let input = String::from("int main() {return -1;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_logical_negation() -> Result<(), String> {
        let input = String::from("int main() {return !1;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_addition() -> Result<(), String> {
        let input = String::from("int main() {return 1+2;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_subtraction() -> Result<(), String> {
        let input = String::from("int main() {return 1-2;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_multiplication() -> Result<(), String> {
        let input = String::from("int main() {return 2*4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_division() -> Result<(), String> {
        let input = String::from("int main() {return 2/4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_double_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 == 4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_bang_equals() -> Result<(), String> {
        let input = String::from("int main() {return 2 != 4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_less_than() -> Result<(), String> {
        let input = String::from("int main() {return 2 < 4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_greater_than() -> Result<(), String> {
        let input = String::from("int main() {return 2 > 4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_logical_or() -> Result<(), String> {
        let input = String::from("int main() {return 1 || 0;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }

    #[test]
    fn test_x86_64_main_logical_and() -> Result<(), String> {
        let input = String::from("int main() {return 2 && 4;}");
        let tokens = lexer::lexer::lex(&input)?;
        let ast = parser::parser::parse(tokens)?;
        let asm_list = codegen::CodeGenerator::new().generate_code(ast)?;
        let x86_64_asm = generate_x86_64(asm_list)?;
        assert_display_snapshot!(x86_64_asm);
        Ok(())
    }
}
