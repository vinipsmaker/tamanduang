#[derive(Debug,Clone,PartialEq)]
pub enum TokenCategory {
    /// ```text
    /// ":"
    /// ```
    COLON,
    /// ```text
    /// ";"
    /// ```
    SEMICOLON,
    /// ```text
    /// "["
    /// ```
    LBRACKET,
    /// ```text
    /// "]"
    /// ```
    RBRACKET,
    /// ```text
    /// "("
    /// ```
    LPAREN,
    /// ```text
    /// ")"
    /// ```
    RPAREN,
    /// ```text
    /// "{"
    /// ```
    LBRACE,
    /// ```text
    /// "}"
    /// ```
    RBRACE,
    /// ```text
    /// ","
    /// ```
    COMMA,
    /// ```text
    /// (LETTER | "_") { LETTER | "_" | DIGIT }
    /// ```
    IDENTIFIER(String),
    /// ```text
    /// "var"
    /// ```
    VARKEYWORD,
    /// ```text
    /// "none"
    /// ```
    NONEDECLARATOR,
    /// ```text
    /// "bool"
    /// ```
    BOOLDECLARATOR,
    /// ```text
    /// "int32"
    /// ```
    INT32DECLARATOR,
    /// ```text
    /// "double"
    /// ```
    DOUBLEDECLARATOR,
    /// ```text
    /// "uchar"
    /// ```
    UCHARDECLARATOR,
    /// ```text
    /// "list"
    /// ```
    LISTDECLARATOR,
    /// ```text
    /// "string"
    /// ```
    STRINGDECLARATOR,
    /// ```text
    /// "fn"
    /// ```
    FUNCTIONKEYWORD,
    /// ```text
    /// "true" | "false"
    /// ```
    BOOLLIT(bool),
    /// ```text
    /// DIGIT { DIGIT }
    /// ```
    INT32LIT(i32),
    /// ```text
    /// DIGIT { DIGIT } "." DIGIT { DIGIT }
    /// ```
    DOUBLELIT(f64),
    /// ```text
    /// ("U+" DIGIT DIGIT DIGIT DIGIT) | ("'" (ALL_CHARS - "'") "'")
    /// ```
    UCHARLIT(u32),
    /// ```text
    /// "="
    /// ```
    ASSIGNOPERATOR,
    /// ```text
    /// "!"
    /// ```
    NEGOPERATOR,
    /// ```text
    /// "and"
    /// ```
    ANDOPERATOR,
    /// ```text
    /// "or"
    /// ```
    OROPERATOR,
    /// ```text
    /// "-"
    /// ```
    MINUSSIGN,
    /// ```text
    /// "+"
    /// ```
    PLUSOPERATOR,
    /// ```text
    /// "*"
    /// ```
    TIMESOPERATOR,
    /// ```text
    /// "<"
    /// ```
    LTOPERATOR,
    /// ```text
    /// ">"
    /// ```
    GTOPERATOR,
    /// ```text
    /// "<="
    /// ```
    LTEOPERATOR,
    /// ```text
    /// ">="
    /// ```
    GTEOPERATOR,
    /// ```text
    /// "=="
    /// ```
    EQOPERATOR,
    /// ```text
    /// "!="
    /// ```
    NEOPERATOR,
    /// ```text
    /// "return"
    /// ```
    RETURN,
    /// ```text
    /// "if"
    /// ```
    IFKEYWORD,
    /// ```text
    /// "else"
    /// ```
    ELSEKEYWORD,
    /// ```text
    /// "while"
    /// ```
    WHILEKEYWORD,
    /// ```text
    /// "for"
    /// ```
    FORKEYWORD,
    /// ```text
    /// "until"
    /// ```
    UNTILKEYWORD,
}

#[derive(Debug)]
pub struct Token {
    category: TokenCategory,
    line: u32,
    lexpos: u32,
}

impl Token {
    pub fn new(category: TokenCategory, line: u32, lexpos: u32) -> Token {
        Token {
            category: category,
            line: line,
            lexpos: lexpos
        }
    }

    pub fn category(&self) -> TokenCategory {
        self.category.clone()
    }
}

impl PartialEq<Token> for Token {
    fn eq(&self, rhs: &Token) -> bool {
        return (&self.category, self.line, self.lexpos)
            == (&rhs.category, rhs.line, rhs.lexpos);
    }
}
