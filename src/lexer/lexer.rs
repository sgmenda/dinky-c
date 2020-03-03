use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// (
    OpenParenthesis,
    /// )
    CloseParenthesis,
    /// ;
    Semicolon,
    /// int
    INT,
    /// return
    RETURN,
    Identifier(String),
    /// internally, we'll be using signed 64 bit numbers, but constants are
    /// assumed to be 32 bit.
    Integer(u32),
    /// -
    Minus,
    /// ~
    BitwiseComplement,
    /// !
    LogicalNegation,
    /// +
    Plus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// &&
    DoubleAmpersand,
    /// ||
    DoublePipe,
    /// ==
    DoubleEquals,
    /// !=
    BangEquals,
    /// LessThan
    LessThan,
    /// LessThanEquals
    LessThanEquals,
    /// GreaterThan
    GreaterThan,
    /// GreaterThanEquals
    GreaterThanEquals,
    /// =
    Equals,
}

/// HashMap for keywords. Maps keyword strings to the corresponding tokens.
type KeywordMap = HashMap<String, Token>;

/// Creates a KeywordMap for use in lexing.
fn create_keyword_map() -> KeywordMap {
    let mut keyword_map = KeywordMap::new();
    keyword_map.insert(String::from("int"), Token::INT);
    keyword_map.insert(String::from("return"), Token::RETURN);
    keyword_map
}

/// HashMap for operators. Maps operator strings to the corresponding tokens.
type OperatorMap = HashMap<String, Token>;

/// Creates an OperatorMap for use in lexing.
fn create_operator_map() -> OperatorMap {
    let mut operator_map = OperatorMap::new();
    operator_map.insert(String::from("&&"), Token::DoubleAmpersand);
    operator_map.insert(String::from("||"), Token::DoublePipe);
    operator_map.insert(String::from("=="), Token::DoubleEquals);
    operator_map.insert(String::from("!="), Token::BangEquals);
    operator_map.insert(String::from("<"), Token::LessThan);
    operator_map.insert(String::from("<="), Token::LessThanEquals);
    operator_map.insert(String::from(">"), Token::GreaterThan);
    operator_map.insert(String::from(">="), Token::GreaterThanEquals);
    operator_map.insert(String::from("!"), Token::LogicalNegation);
    operator_map.insert(String::from("-"), Token::Minus);
    operator_map.insert(String::from("~"), Token::BitwiseComplement);
    operator_map.insert(String::from("+"), Token::Plus);
    operator_map.insert(String::from("*"), Token::Asterisk);
    operator_map.insert(String::from("/"), Token::Slash);
    operator_map.insert(String::from("="), Token::Equals);
    operator_map
}

/// Lexes a word (keyword or identifier), returns nothing.
///
/// # Arguments
///
/// * `input` - peekable iterator corresponding to the lexer's current location in the input.
///   Moves this vector forward.
/// * `output` - vector of `Token`s that have been created. Adds `Token`s to this vector.
///
fn consume_word(
    input: &mut std::iter::Peekable<std::str::Chars<'_>>,
    output: &mut Vec<Token>,
    keyword_map: &KeywordMap,
) -> Result<(), String> {
    let mut word = String::new();
    while let Some(c) = input.peek() {
        match c {
            'A'..='z' | '0'..='9' => {
                // if alphanumeric, add to word.
                word.push(*c);
                input.next();
            }
            _ => {
                // if not alphanumeric, then break.
                break;
            }
        }
    }
    // check if the word is a keyword using the keyword_map
    match keyword_map.get(&word) {
        Some(t) => {
            let ct = t.clone();
            output.push(ct);
        }
        _ => {
            output.push(Token::Identifier(word));
        }
    }
    Ok(())
}

/// Lexes an operator, returns nothing.
///
/// Rust does not have bi-directional iterators, so this is a little hard to
/// implement (if it had bi-directional iters, we could consume as much as we
/// can, then then iter-- if the word is unrecognized.)
///
/// # Arguments
///
/// * `input` - peekable iterator corresponding to the lexer's current location in the input.
///   Moves this vector forward.
/// * `output` - vector of `Token`s that have been created. Adds `Token`s to this vector.
///
fn consume_operator(
    input: &mut std::iter::Peekable<std::str::Chars<'_>>,
    output: &mut Vec<Token>,
    operator_map: &OperatorMap,
) -> Result<(), String> {
    let mut word = String::new();
    while let Some(c) = input.peek() {
        // Add to word.
        word.push(*c);
        match operator_map.get(&word) {
            Some(_) => {
                // if it is a valid operator, then iter++ and continue
                input.next();
            }
            None => {
                // if it is not a valid operator, then output last valid
                // operator except in the case of `=`, `|`, or `&` (these are
                // prefixes of valid operators.)
                if (word == '|'.to_string()) | (word == '&'.to_string()) | (word == '='.to_string())
                {
                    input.next();
                    continue;
                }
                word.pop();
                match operator_map.get(&word) {
                    Some(t) => {
                        output.push(t.clone());
                        return Ok(());
                    }
                    None => {
                        return Err(format!(
                            "Unexpected word \"{}\" when consuming operator",
                            word
                        ));
                    }
                }
            }
        }
    }
    Ok(())
}

/// Lexes a number, returns nothing.
///
/// # Arguments
///
/// * `input` - peekable iterator corresponding to the lexer's current location in the input.
///   Moves this vector forward.
/// * `output` - vector of `Token`s that have been created. Adds `Token`s to this vector.
///
fn consume_number(
    input: &mut std::iter::Peekable<std::str::Chars<'_>>,
    output: &mut Vec<Token>,
) -> Result<(), String> {
    let mut number: u32 = 0;
    // try to convert the next character into a digit base 10
    while let Some(c) = input.peek() {
        match c.to_digit(10) {
            None => {
                // if to_digit returns None then the peek is not a digit base 10, so we break.
                break;
            }
            Some(digit) => {
                // if to_digit returns a number, we update our number and advance input.
                number = (number * 10) + digit;
                input.next();
            }
        }
    }
    output.push(Token::Integer(number));
    Ok(())
}

/// Returns a token stream.
///
/// # Arguments
///
/// * `source` - String that holds the input to the compiler
///
pub fn lex(source: &str) -> Result<Vec<Token>, String> {
    let mut output = Vec::new();
    // get a peekable iterator over the chars in source
    let mut input = source.chars().peekable();

    // create the keyword map
    let keyword_map = create_keyword_map();

    // create the operator map
    let operator_map = create_operator_map();

    while let Some(c) = input.peek() {
        match c {
            '{' => {
                output.push(Token::OpenBrace);
                input.next();
            }
            '}' => {
                output.push(Token::CloseBrace);
                input.next();
            }
            '(' => {
                output.push(Token::OpenParenthesis);
                input.next();
            }
            ')' => {
                output.push(Token::CloseParenthesis);
                input.next();
            }
            ';' => {
                output.push(Token::Semicolon);
                input.next();
            }
            '&' | '|' | '=' | '!' | '<' | '>' | '/' | '*' | '+' | '~' | '-' => {
                consume_operator(&mut input, &mut output, &operator_map)?;
            }
            '0'..='9' => {
                consume_number(&mut input, &mut output)?;
            }
            'A'..='z' => {
                consume_word(&mut input, &mut output, &keyword_map)?;
            }
            ' ' | '\n' | '\t' => {
                input.next();
            }
            _ => {
                return Err(format!("Cannot parse \'{}\'", c));
            }
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;
    #[test]
    fn test_integer_token_comparison() {
        assert_eq!(Token::Integer(100), Token::Integer(100));
    }
    #[test]
    fn test_identifier_token_comparison() {
        assert_eq!(
            Token::Identifier(String::from("100")),
            Token::Identifier(String::from("100"))
        );
    }
    #[test]
    fn test_lex_open_paren() -> Result<(), String> {
        let input = String::from("(");
        let expected = vec![Token::OpenParenthesis];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_a_bunch_of_paren() -> Result<(), String> {
        let input = String::from("(){}");
        let expected = vec![
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::CloseBrace,
        ];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_simple_number() -> Result<(), String> {
        let input = String::from("1");
        let expected = vec![Token::Integer(1)];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_complicated_number() -> Result<(), String> {
        let input = String::from("1234567890");
        let expected = vec![Token::Integer(1234567890)];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_simple_token() -> Result<(), String> {
        let input = String::from("int");
        let expected = vec![Token::INT];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_simple_identifier() -> Result<(), String> {
        let input = String::from("abc");
        let expected = vec![Token::Identifier(String::from("abc"))];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_return_negation() {
        let input = String::from("return -1;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_bitwise_complement() {
        let input = String::from("return ~2;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_logical_negation() {
        let input = String::from("return !3;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_many_unary_ops() {
        let input = String::from("return ~!~~-!3;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_addition() {
        let input = String::from("return 1+3;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_subtraction() {
        let input = String::from("return 3-1;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_multiplication() {
        let input = String::from("return 3*3;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_return_division() {
        let input = String::from("return 10/5;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_double_ampersand() {
        let input = String::from("1 && 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_double_pipe() {
        let input = String::from("1 || 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_double_equals() {
        let input = String::from("1 == 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_bang_equals() {
        let input = String::from("1 != 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_less_than() {
        let input = String::from("1 < 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_less_than_equal() {
        let input = String::from("1 <= 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_greater_than() {
        let input = String::from("1 > 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_greater_than_equal() {
        let input = String::from("1 >= 0");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_multicharacter_operator_many() {
        let input = String::from("return 1 || 0 && 2;");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_assignment() {
        let input = String::from("int main() { int x = 10; x = x + 12; return x; }");
        assert_debug_snapshot!(lex(&input));
    }
    #[test]
    fn test_lex_complex_identifier() -> Result<(), String> {
        let input = String::from("a1b2c3");
        let expected = vec![Token::Identifier(String::from("a1b2c3"))];
        match lex(&input) {
            Ok(x) => {
                assert_eq!(&x, &expected);
                Ok(())
            }
            Err(e) => Err(e),
        }
    }
    #[test]
    fn test_lex_composition_0() -> Result<(), String> {
        let input = String::from("int a1b2c");
        let expected = vec![Token::INT, Token::Identifier(String::from("a1b2c"))];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_composition_1() -> Result<(), String> {
        let input = String::from("int a1b2c(){\n\treturn;\n}");
        let expected = vec![
            Token::INT,
            Token::Identifier(String::from("a1b2c")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::RETURN,
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
    #[test]
    fn test_lex_composition_2() -> Result<(), String> {
        let input = String::from("int main(){\n\treturn 0;\n}");
        let expected = vec![
            Token::INT,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::RETURN,
            Token::Integer(0),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let x = lex(&input)?;
        assert_eq!(&x, &expected);
        Ok(())
    }
}
