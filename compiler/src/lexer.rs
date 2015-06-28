extern crate regex;

use token::{Token};
use self::regex::Regex;
use std::io::{Read,Seek,SeekFrom,ErrorKind};

#[derive(Debug,PartialEq,Eq)]
pub enum LexerError {
    Unrecognized(String),
    EndOfStream,
    StreamError(ErrorKind),
}

pub struct Lexer<'a, T: 'a> {
    input: &'a mut T,
    buffer: String,
    line: u32,
    lexpos: u32,
}

fn is_single_char_token(ch: char) -> bool {
    match ch {
        ':' | ';' | '[' | ']' | '(' | ')' | '{' | '}' | ',' | '=' | '!' | '-'
            | '+' | '*' | '<' | '>' => true,
        _ => false,
    }
}

fn is_first_half_of_double_char_token(ch: char) -> bool {
    match ch {
        '<' | '>' | '=' | '!' => true,
        _ => false,
    }
}

fn is_double_char_token(first: char, second: char) -> bool {
    match (first, second) {
        ('<', '=') | ('>', '=') | ('=', '=') | ('!', '=') => true,
        _ => false,
    }
}

fn gen_token(buffer: &str, line: u32, lexpos: u32)
            -> Result<Token, LexerError> {
    use token::TokenCategory as Cat;
    let gen = |c: Cat| { Ok(Token::new(c, line, lexpos)) };

    match buffer {
        ":" => gen(Cat::COLON),
        ";" => gen(Cat::SEMICOLON),
        "[" => gen(Cat::LBRACKET),
        "]" => gen(Cat::RBRACKET),
        "(" => gen(Cat::LPAREN),
        ")" => gen(Cat::RPAREN),
        "{" => gen(Cat::LBRACE),
        "}" => gen(Cat::RBRACE),
        "," => gen(Cat::COMMA),
        "var" => gen(Cat::VARKEYWORD),
        "none" => gen(Cat::NONEDECLARATOR),
        "bool" => gen(Cat::BOOLDECLARATOR),
        "int32" => gen(Cat::INT32DECLARATOR),
        "double" => gen(Cat::DOUBLEDECLARATOR),
        "uchar" => gen(Cat::UCHARDECLARATOR),
        "list" => gen(Cat::LISTDECLARATOR),
        "string" => gen(Cat::STRINGDECLARATOR),
        "fn" => gen(Cat::FUNCTIONKEYWORD),
        "true" => gen(Cat::BOOLLIT(true)),
        "false" => gen(Cat::BOOLLIT(false)),
        "=" => gen(Cat::ASSIGNOPERATOR),
        "!" => gen(Cat::NEGOPERATOR),
        "and" => gen(Cat::ANDOPERATOR),
        "or" => gen(Cat::OROPERATOR),
        "-" => gen(Cat::MINUSSIGN),
        "+" => gen(Cat::PLUSOPERATOR),
        "*" => gen(Cat::TIMESOPERATOR),
        "<" => gen(Cat::LTOPERATOR),
        ">" => gen(Cat::GTOPERATOR),
        "<=" => gen(Cat::LTEOPERATOR),
        ">=" => gen(Cat::GTEOPERATOR),
        "==" => gen(Cat::EQOPERATOR),
        "!=" => gen(Cat::NEOPERATOR),
        "return" => gen(Cat::RETURN),
        "if" => gen(Cat::IFKEYWORD),
        "else" => gen(Cat::ELSEKEYWORD),
        "while" => gen(Cat::WHILEKEYWORD),
        "for" => gen(Cat::FORKEYWORD),
        "until" => gen(Cat::UNTILKEYWORD),
        _ => {
            let m = |r| -> bool {
                Regex::new(r).unwrap().is_match(buffer)
            };

            if m("^[a-zA-Z_][a-zA-Z_0-9]*$") {
                gen(Cat::IDENTIFIER(buffer.to_string()))
            } else if m("^[0-9]+\\.[0-9]+$") {
                match buffer.parse() {
                    Ok(d) => gen(Cat::DOUBLELIT(d)),
                    _ => Err(LexerError::Unrecognized(buffer.to_string()))
                }
            } else if m("^[0-9]+$") {
                match buffer.parse() {
                    Ok(i) => gen(Cat::INT32LIT(i)),
                    _ => Err(LexerError::Unrecognized(buffer.to_string()))
                }
            } else if m("^U[+][0-9]{4}$") {
                if let Ok(codepoint)
                    = u32::from_str_radix(&buffer.to_string().chars().skip(2)
                                          .collect::<String>(), 16) {
                    gen(Cat::UCHARLIT(codepoint))
                } else {
                    Err(LexerError::Unrecognized(buffer.to_string()))
                }
            } else if m("^'[^']'$") {
                gen(Cat::UCHARLIT(buffer.to_string().as_bytes()[1] as u32))
            } else {
                Err(LexerError::Unrecognized(buffer.to_string()))
            }
        },
    }
}

impl<'a, T: Read + Seek> Lexer<'a, T> {
    pub fn new(input: &'a mut T) -> Self {
        Lexer {
            input: input,
            buffer: String::new(),
            line: 1,
            lexpos: 0,
        }
    }

    #[allow(non_snake_case)]
    pub fn nextToken(&mut self) -> Result<Token, LexerError> {
        if self.buffer.len() > 0 {
            return gen_token(&self.buffer, self.line, self.lexpos);
        }

        while let Some(Ok(ch)) = self.input.bytes().next() {
            self.lexpos += 1;

            let mut ch = ch as char;

            // cross-platform handling for end of line {{{
            if self.buffer.chars().last() == Some('\r') {
                self.buffer.pop();

                self.line += 1;
                self.lexpos = 0;

                if ch != '\n' {
                    if let Err(e) = self.input.seek(SeekFrom::Current(-1)) {
                        return Err(LexerError::StreamError(e.kind()));
                    }
                }

                ch = '\n';
            } else if ch == '\n' {
                self.line += 1;
                self.lexpos = 0;
            } else if ch == '\r' {
                self.buffer.push('\r');
                continue;
            }
            // }}}

            // special char lit handling {{{
            if let Some(front) = self.buffer.chars().nth(0) {
                if front == '\'' {
                    self.buffer.push(ch);

                    if ch != '\'' {
                        continue;
                    }

                    let t = gen_token(&self.buffer, self.line, self.lexpos);
                    self.buffer.clear();
                    return t;
                }
            } else if ch == '\'' {
                self.buffer.push(ch);
                continue;
            }
            // }}}

            if self.buffer.len() == 1 {
                let front = self.buffer.chars().nth(0).unwrap();
                if is_first_half_of_double_char_token(front) {
                    if is_double_char_token(front, ch) {
                        self.buffer.push(ch);
                        let t = gen_token(&self.buffer, self.line, self.lexpos);
                        self.buffer.clear();
                        return t;
                    } else if is_single_char_token(front) {
                        let t = gen_token(&self.buffer, self.line, self.lexpos);
                        self.buffer.clear();
                        if let Err(e) = self.input.seek(SeekFrom::Current(-1)) {
                            return Err(LexerError::StreamError(e.kind()));
                        }
                        return t;
                    }
                }
            }

            if self.buffer.len() > 0
                && (is_first_half_of_double_char_token(ch)
                    || is_single_char_token(ch)) {
                let t = gen_token(&self.buffer, self.line, self.lexpos);
                self.buffer.clear();
                if let Err(e) = self.input.seek(SeekFrom::Current(-1)) {
                    return Err(LexerError::StreamError(e.kind()));
                }
                return t;
            }

            if self.buffer.len() == 0 {
                if is_first_half_of_double_char_token(ch) {
                    self.buffer.push(ch);
                    continue;
                } else if is_single_char_token(ch) {
                    return gen_token(&String::from_utf8(vec![ch as u8])
                                     .unwrap(),
                                     self.line, self.lexpos);
                }
            }

            if ch.is_whitespace() {
                if self.buffer.len() > 0 {
                    let line = self.line - if ch == '\n' { 1 } else { 0 };
                    let t = gen_token(&self.buffer, line, self.lexpos);
                    self.buffer.clear();
                    return t;
                }
            } else {
                self.buffer.push(ch)
            }
        }

        if self.buffer.len() > 0 {
            let ret = gen_token(&self.buffer, self.line, self.lexpos);
            self.buffer.clear();
            ret
        } else {
            Err(LexerError::EndOfStream)
        }
    }
}

#[cfg(test)]
mod tests {
    use token::{Token};
    use super::{LexerError,gen_token,Lexer};
    use std::io::Cursor;

    #[test]
    fn gen_token_test() {
        use token::TokenCategory as Cat;
        assert_eq!(gen_token("", 0, 0),
                   Err(LexerError::Unrecognized("".to_string())));
        assert_eq!(gen_token(":", 0, 0),
                   Ok(Token::new(Cat::COLON, 0, 0)));
        assert_eq!(gen_token(";", 0, 0),
                   Ok(Token::new(Cat::SEMICOLON, 0, 0)));
        assert_eq!(gen_token("[", 0, 0),
                   Ok(Token::new(Cat::LBRACKET, 0, 0)));
        assert_eq!(gen_token("]", 0, 0),
                   Ok(Token::new(Cat::RBRACKET, 0, 0)));
        assert_eq!(gen_token("(", 0, 0),
                   Ok(Token::new(Cat::LPAREN, 0, 0)));
        assert_eq!(gen_token(")", 0, 0),
                   Ok(Token::new(Cat::RPAREN, 0, 0)));
        assert_eq!(gen_token("{", 0, 0),
                   Ok(Token::new(Cat::LBRACE, 0, 0)));
        assert_eq!(gen_token("}", 0, 0),
                   Ok(Token::new(Cat::RBRACE, 0, 0)));
        assert_eq!(gen_token(",", 0, 0),
                   Ok(Token::new(Cat::COMMA, 0, 0)));
        assert_eq!(gen_token("var", 0, 0),
                   Ok(Token::new(Cat::VARKEYWORD, 0, 0)));
        assert_eq!(gen_token("none", 0, 0),
                   Ok(Token::new(Cat::NONEDECLARATOR, 0, 0)));
        assert_eq!(gen_token("bool", 0, 0),
                   Ok(Token::new(Cat::BOOLDECLARATOR, 0, 0)));
        assert_eq!(gen_token("int32", 0, 0),
                   Ok(Token::new(Cat::INT32DECLARATOR, 0, 0)));
        assert_eq!(gen_token("double", 0, 0),
                   Ok(Token::new(Cat::DOUBLEDECLARATOR, 0, 0)));
        assert_eq!(gen_token("uchar", 0, 0),
                   Ok(Token::new(Cat::UCHARDECLARATOR, 0, 0)));
        assert_eq!(gen_token("list", 0, 0),
                   Ok(Token::new(Cat::LISTDECLARATOR, 0, 0)));
        assert_eq!(gen_token("string", 0, 0),
                   Ok(Token::new(Cat::STRINGDECLARATOR, 0, 0)));
        assert_eq!(gen_token("fn", 0, 0),
                   Ok(Token::new(Cat::FUNCTIONKEYWORD, 0, 0)));
        assert_eq!(gen_token("true", 0, 0),
                   Ok(Token::new(Cat::BOOLLIT(true), 0, 0)));
        assert_eq!(gen_token("false", 0, 0),
                   Ok(Token::new(Cat::BOOLLIT(false), 0, 0)));
        assert_eq!(gen_token("=", 0, 0),
                   Ok(Token::new(Cat::ASSIGNOPERATOR, 0, 0)));
        assert_eq!(gen_token("!", 0, 0),
                   Ok(Token::new(Cat::NEGOPERATOR, 0, 0)));
        assert_eq!(gen_token("and", 0, 0),
                   Ok(Token::new(Cat::ANDOPERATOR, 0, 0)));
        assert_eq!(gen_token("or", 0, 0),
                   Ok(Token::new(Cat::OROPERATOR, 0, 0)));
        assert_eq!(gen_token("-", 0, 0),
                   Ok(Token::new(Cat::MINUSSIGN, 0, 0)));
        assert_eq!(gen_token("+", 0, 0),
                   Ok(Token::new(Cat::PLUSOPERATOR, 0, 0)));
        assert_eq!(gen_token("*", 0, 0),
                   Ok(Token::new(Cat::TIMESOPERATOR, 0, 0)));
        assert_eq!(gen_token("<", 0, 0),
                   Ok(Token::new(Cat::LTOPERATOR, 0, 0)));
        assert_eq!(gen_token(">", 0, 0),
                   Ok(Token::new(Cat::GTOPERATOR, 0, 0)));
        assert_eq!(gen_token("<=", 0, 0),
                   Ok(Token::new(Cat::LTEOPERATOR, 0, 0)));
        assert_eq!(gen_token(">=", 0, 0),
                   Ok(Token::new(Cat::GTEOPERATOR, 0, 0)));
        assert_eq!(gen_token("==", 0, 0),
                   Ok(Token::new(Cat::EQOPERATOR, 0, 0)));
        assert_eq!(gen_token("!=", 0, 0),
                   Ok(Token::new(Cat::NEOPERATOR, 0, 0)));
        assert_eq!(gen_token("return", 0, 0),
                   Ok(Token::new(Cat::RETURN, 0, 0)));
        assert_eq!(gen_token("if", 0, 0),
                   Ok(Token::new(Cat::IFKEYWORD, 0, 0)));
        assert_eq!(gen_token("else", 0, 0),
                   Ok(Token::new(Cat::ELSEKEYWORD, 0, 0)));
        assert_eq!(gen_token("while", 0, 0),
                   Ok(Token::new(Cat::WHILEKEYWORD, 0, 0)));
        assert_eq!(gen_token("for", 0, 0),
                   Ok(Token::new(Cat::FORKEYWORD, 0, 0)));
        assert_eq!(gen_token("until", 0, 0),
                   Ok(Token::new(Cat::UNTILKEYWORD, 0, 0)));
        assert_eq!(gen_token("foobar", 0, 0),
                   Ok(Token::new(Cat::IDENTIFIER("foobar".to_string()), 0, 0)));
        assert_eq!(gen_token("3.1415", 0, 0),
                   Ok(Token::new(Cat::DOUBLELIT(3.1415), 0, 0)));
        assert_eq!(gen_token("42", 0, 0),
                   Ok(Token::new(Cat::INT32LIT(42), 0, 0)));
        assert_eq!(gen_token("U+0020", 0, 0),
                   Ok(Token::new(Cat::UCHARLIT(0x20), 0, 0)));
        assert_eq!(gen_token("' '", 0, 0),
                   Ok(Token::new(Cat::UCHARLIT(0x20), 0, 0)));
        assert_eq!(gen_token("##", 0, 0),
                   Err(LexerError::Unrecognized("##".to_string())));
    }


    #[test]
    fn tokenizer_test() {
        use token::TokenCategory as Cat;

        let mut source = Cursor::new("var out: string;
push(out, 0, 'H');\r
push(out, 1, 'e');\rpush(out, 2, 'l');
push(out, 3, 'l');
push(out, 4, 'o');
push(out, 5, ' ');
push(out, 6, 'W');
push(out, 7, 'o');
push(out, 8, 'r');
push(out, 9, 'l');
push(out, 10, 'd');
println(out);
".as_bytes());
        let mut lex = Lexer::new(&mut source);

        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::VARKEYWORD, 1, 4)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 1, 8)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COLON, 1, 9)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::STRINGDECLARATOR, 1, 17)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 1, 18)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 2, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 2, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 2, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 2, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(0), 2, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 2, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('H' as u32), 2, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 2, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 2, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 3, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 3, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 3, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 3, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(1), 3, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 3, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('e' as u32), 3, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 3, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 3, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 4, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 4, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 4, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 4, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(2), 4, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 4, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('l' as u32), 4, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 4, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 4, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 5, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 5, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 5, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 5, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(3), 5, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 5, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('l' as u32), 5, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 5, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 5, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 6, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 6, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 6, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 6, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(4), 6, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 6, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('o' as u32), 6, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 6, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 6, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 7, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 7, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 7, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 7, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(5), 7, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 7, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT(' ' as u32), 7, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 7, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 7, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 8, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 8, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 8, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 8, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(6), 8, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 8, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('W' as u32), 8, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 8, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 8, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 9, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 9, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 9, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 9, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(7), 9, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 9, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('o' as u32), 9, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 9, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 9, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 10, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 10, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 10, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 10, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(8), 10, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 10, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('r' as u32), 10, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 10, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 10, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 11, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 11, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 11, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 11, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(9), 11, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 11, 15)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('l' as u32), 11, 19)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 11, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 11, 21)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("push".to_string()), 12, 5)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 12, 6)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 12, 10)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 12, 11)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::INT32LIT(10), 12, 15)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::COMMA, 12, 16)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::UCHARLIT('d' as u32), 12, 20)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 12, 21)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 12, 22)));

        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("println".to_string()), 13,
                                 8)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::LPAREN, 13, 9)));
        assert_eq!(lex.nextToken(),
                   Ok(Token::new(Cat::IDENTIFIER("out".to_string()), 13, 13)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::RPAREN, 13, 14)));
        assert_eq!(lex.nextToken(), Ok(Token::new(Cat::SEMICOLON, 13, 15)));

        assert_eq!(lex.nextToken(), Err(LexerError::EndOfStream));
    }
}
