use std::str::CharIndices;

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
  
    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
  
    // Literals.
    Identifier, String, Number,
  
    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
  
    Eof, Error
}

#[rustfmt::skip]
macro_rules! T {
    ('(') => {$crate::scanner::TokenKind::LeftParen};
    (')') => {$crate::scanner::TokenKind::RightParen};
    ('{') => {$crate::scanner::TokenKind::LeftBrace};
    ('}') => {$crate::scanner::TokenKind::RightBrace};
    (,) => {$crate::scanner::TokenKind::Comma};
    (.) => {$crate::scanner::TokenKind::Dot};
    (-) => {$crate::scanner::TokenKind::Minus};
    (+) => {$crate::scanner::TokenKind::Plus};
    (;) => {$crate::scanner::TokenKind::Semicolon};
    (/) => {$crate::scanner::TokenKind::Slash};
    (*) => {$crate::scanner::TokenKind::Star};
    (!) => {$crate::scanner::TokenKind::Bang};
    (!=) => {$crate::scanner::TokenKind::BangEqual};
    (=) => {$crate::scanner::TokenKind::Equal};
    (==) => {$crate::scanner::TokenKind::EqualEqual};
    (>) => {$crate::scanner::TokenKind::Greater};
    (>=) => {$crate::scanner::TokenKind::GreaterEqual};
    (<) => {$crate::scanner::TokenKind::Less};
    (<=) => {$crate::scanner::TokenKind::LessEqual};
    (ident) => {$crate::scanner::TokenKind::Identifier};
    (str) => {$crate::scanner::TokenKind::String};
    (number) => {$crate::scanner::TokenKind::Number};
    (and) => {$crate::scanner::TokenKind::And};
    (class) => {$crate::scanner::TokenKind::Class};
    (else) => {$crate::scanner::TokenKind::Else};
    (false) => {$crate::scanner::TokenKind::False};
    (fun) => {$crate::scanner::TokenKind::Fun};
    (for) => {$crate::scanner::TokenKind::For};
    (if) => {$crate::scanner::TokenKind::If};
    (nil) => {$crate::scanner::TokenKind::Nil};
    (or) => {$crate::scanner::TokenKind::Or};
    (print) => {$crate::scanner::TokenKind::Print};
    (return) => {$crate::scanner::TokenKind::Return};
    (super) => {$crate::scanner::TokenKind::Super};
    (this) => {$crate::scanner::TokenKind::This};
    (true) => {$crate::scanner::TokenKind::True};
    (var) => {$crate::scanner::TokenKind::Var};
    (while) => {$crate::scanner::TokenKind::While};
    (break) => {$crate::scanner::TokenKind::Break};
    (EOF) => {$crate::scanner::TokenKind::Eof};
    (ERR) => {$crate::scanner::TokenKind::Error};
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub line: u32,
}

impl<'a> Token<'a> {
    pub fn dummy() -> Self {
        Self {
            kind: T![EOF],
            lexeme: "",
            line: 0,
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    chars: CharIndices<'a>,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            chars: source.char_indices(),
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.chars.offset();

        let Some(c) = self.advance() else {
            return self.make_token(T![EOF])
        };

        match c {
            '(' => self.make_token(T!['(']),
            ')' => self.make_token(T![')']),
            '{' => self.make_token(T!['{']),
            '}' => self.make_token(T!['}']),
            ';' => self.make_token(T![;]),
            ',' => self.make_token(T![,]),
            '.' => self.make_token(T![.]),
            '-' => self.make_token(T![-]),
            '+' => self.make_token(T![+]),
            '/' => self.make_token(T![/]),
            '*' => self.make_token(T![*]),
            '!' if self.consume('=') => self.make_token(T![!=]),
            '!' => self.make_token(T![!]),
            '=' if self.consume('=') => self.make_token(T![==]),
            '=' => self.make_token(T![=]),
            '<' if self.consume('=') => self.make_token(T![<=]),
            '<' => self.make_token(T![<]),
            '>' if self.consume('=') => self.make_token(T![>=]),
            '>' => self.make_token(T![>]),
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|(_, c)| c)
    }

    fn peek(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n).map(|(_, c)| c)
    }

    fn consume(&mut self, expected: char) -> bool {
        if self.peek(0) == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek(0) {
                Some(' ' | '\r' | '\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                }
                Some('/') if self.peek(1) == Some('/') => {
                    while !matches!(self.peek(0), Some('\n') | None) {
                        self.advance();
                    }
                }
                _ => return,
            };
        }
    }

    fn lexme(&self) -> &'a str {
        &self.source[self.start..self.chars.offset()]
    }

    fn make_token(&self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            lexeme: self.lexme(),
            line: self.line,
        }
    }

    fn error_token(&self, msg: &'static str) -> Token<'a> {
        Token {
            kind: T![ERR],
            lexeme: msg,
            line: self.line,
        }
    }

    fn string(&mut self) -> Token<'a> {
        loop {
            match self.advance() {
                Some('"') => break,
                Some('\n') => self.line += 1,
                None => return self.error_token("Unterminated string."),
                _ => {}
            }
        }

        self.make_token(T![str])
    }

    fn is_digit(&self, c: Option<char>) -> bool {
        matches!(c, Some('0'..='9'))
    }

    fn is_alpha(&self, c: Option<char>) -> bool {
        matches!(c, Some('a'..='z' | 'A'..='Z' | '_'))
    }

    fn number(&mut self) -> Token<'a> {
        while self.is_digit(self.peek(0)) {
            self.advance();
        }

        if self.peek(0) == Some('.') && self.is_digit(self.peek(1)) {
            self.advance();

            while self.is_digit(self.peek(0)) {
                self.advance();
            }
        }

        self.make_token(T![number])
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.is_alpha(self.peek(0)) || self.is_digit(self.peek(0)) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenKind {
        match self.lexme().as_bytes()[0] {
            b'a' => self.check_keyword(1, "nd", T![and]),
            b'c' => self.check_keyword(1, "lass", T![class]),
            b'e' => self.check_keyword(1, "lse", T![else]),
            b'i' => self.check_keyword(1, "f", T![if]),
            b'n' => self.check_keyword(1, "il", T![nil]),
            b'o' => self.check_keyword(1, "r", T![or]),
            b'p' => self.check_keyword(1, "rint", T![print]),
            b'r' => self.check_keyword(1, "eturn", T![return]),
            b's' => self.check_keyword(1, "uper", T![super]),
            b'v' => self.check_keyword(1, "ar", T![var]),
            b'w' => self.check_keyword(1, "hile", T![while]),
            b'f' if self.lexme().len() > 1 => match self.lexme().as_bytes()[1] {
                b'a' => self.check_keyword(2, "lse", T![false]),
                b'o' => self.check_keyword(2, "r", T![for]),
                b'u' => self.check_keyword(2, "n", T![fun]),
                _ => T![ident],
            },
            b't' if self.lexme().len() > 1 => match self.lexme().as_bytes()[1] {
                b'h' => self.check_keyword(2, "is", T![this]),
                b'r' => self.check_keyword(2, "ue", T![true]),
                _ => T![ident],
            },
            _ => T![ident],
        }
    }

    fn check_keyword(&self, start: usize, expected: &str, kind: TokenKind) -> TokenKind {
        if self.lexme().as_bytes().get(start..start + expected.len()) == Some(expected.as_bytes()) {
            kind
        } else {
            T![ident]
        }
    }

    // TODO: bench and compare against above code
    fn identifier_type2(&self) -> TokenKind {
        match self.lexme().as_bytes() {
            b"and" => T![and],
            b"class" => T![class],
            b"else" => T![else],
            b"if" => T![if],
            b"nil" => T![nil],
            b"or" => T![or],
            b"print" => T![print],
            b"return" => T![return],
            b"super" => T![super],
            b"var" => T![var],
            b"while" => T![while],
            b"false" => T![false],
            b"for" => T![for],
            b"fun" => T![fun],
            b"this" => T![this],
            b"true" => T![true],
            _ => T![ident],
        }
    }
}
