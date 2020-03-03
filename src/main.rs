mod codegen;
mod lexer;
mod parser;
mod x86_64_gen;

use std::env;
use std::fs;
use std::process::Command;

/// Compiles C source.
///
/// # Arguments
///
/// * `file_name` - name of C source file.
///
/// # Example
///
/// ```
/// compile_file("hello_world.c");
/// ```
fn compile_file(file_name: &str) -> Result<(), String> {
    // Get source code from file.
    let input_string = fs::read_to_string(file_name).expect("Unable to read file");

    // Compile source code
    let tokens = lexer::lexer::lex(&input_string)?;
    let ast = parser::parser::parse(tokens)?;
    let asm_list = codegen::codegen::CodeGenerator::new().generate_code(ast)?;
    let x86_64_asm = x86_64_gen::x86_64_gen::generate_x86_64(asm_list)?;

    // Create file names
    // Suppose file name is of the form [name].c
    // asm file name: [name].c.s
    // executable file name: [name]
    let mut program_asm_name = file_name.to_string();
    program_asm_name.push_str(".s");
    let mut program_exe_name = file_name.to_string();
    program_exe_name.truncate(program_exe_name.len() - 2);

    // Write object code to file and create executable (link. load) using gcc.
    fs::write(&program_asm_name, x86_64_asm).expect("Unable to write file");
    let _ = Command::new("gcc")
        .arg(&program_asm_name)
        .arg("-o")
        .arg(&program_exe_name)
        .arg("-masm=intel")
        .output()
        .expect("failed to execute process");
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    // Ensure that we have two args: [dinky c executable path] and [file name]
    if args.len() != 2 {
        panic!(
            "Expected 2 arguments, got {} instead.\n Args: {:?}",
            args.len(),
            args
        );
    }
    match compile_file(&args[1]) {
        Ok(()) => {}
        Err(e) => panic!("Failed to compile:\n{:?}", e),
    }
}
