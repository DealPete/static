use std::fmt;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub labels: Vec<usize>,
    pub data: Vec<u16>
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Expr),
    Command(Command),
    IfThen(Comparison, Command)
}

#[derive(Debug)]
pub enum Expr {
    String(String)
}

#[derive(Debug)]
pub enum Command {
    PRINT(Vec<Expr>),
    INPUT(String, Var),
    GOTO(u16)
}

#[derive(Debug)]
pub enum Comparison {
    LessThan(Expr, Expr),
    GreaterThan(Expr, Expr),
    Equal(Expr, Expr),
    NotEqual(Expr, Expr)
}

#[derive(Debug)]
pub enum Var {
    Int(String),
    String(String)
}

#[derive(Debug)]
pub enum Constant {
    Int(i16),
    Float(f32),
    String(Vec<u8>)
}

pub type LexProg = Vec<LexLine>;

pub struct RawLine {
    pub number: u16,
    pub bytes: Vec<u8>
}
    
#[derive(Debug)]
pub struct LexLine {
    pub number: u16,
    pub lexemes: Vec<Lexeme>
}

impl LexLine {
    pub fn new(number: u16) -> LexLine {
        LexLine {
            number: number,
            lexemes: Vec::new()
        }
    }
}

pub enum Lexeme {
    OpenParen,
    CloseParen,
    SemiColon,
    Colon,
    Comma,
    Operator(Op),
    Int(u16),
    Float(f32),
    Keyword(u8),
    String(Vec<u8>),
    StrVar(String),
    NumVar(String),
    IntVar(String),
    Remark(Vec<u8>),
    Junk(Vec<u8>)
}

#[derive(Debug)]
pub enum Op {
    Add,
    Mult,
    Sub,
    Div
}

impl fmt::Debug for Lexeme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Lexeme::OpenParen => String::from("("),
            Lexeme::CloseParen => String::from(")"),
            Lexeme::SemiColon => String::from(";"),
            Lexeme::Colon => String::from(":"),
            Lexeme::Comma => String::from(","),
            Lexeme::Operator(ref op) => format!("{:?}", op),
            Lexeme::Int(integer) => format!("{}", integer),
            Lexeme::Float(float) => format!("{}", float),
            Lexeme::Keyword(byte) => keyword(byte).unwrap(),
            Lexeme::String(ref bytes) => match from_petscii(bytes) {
                Ok(string) => format!("\"{}\"", string),
                Err(_) => format!("String: {:?}", bytes)
            },
            Lexeme::StrVar(ref string) => format!("{}$", string),
            Lexeme::IntVar(ref string) => format!("{}%", string),
            Lexeme::NumVar(ref string) => string.clone(),
            Lexeme::Remark(ref bytes) => match from_petscii(bytes) {
                Ok(string) => format!("REM {}", string),
                Err(_) => format!("Remark: {:?}", bytes)
            },
            Lexeme::Junk(ref bytes) => format!("Junk: {:?}", bytes)
        })
    }
}

pub fn keyword(byte: u8) -> Option<String> {
    Some(String::from(match byte {
        0x80 => "END",
        0x81 => "FOR",
        0x82 => "NEXT",
        0x83 => "DATA",
        0x84 => "INPUT#",
        0x85 => "INPUT",
        0x86 => "DIM",
        0x87 => "READ",
        0x88 => "LET",
        0x89 => "GOTO",
        0x8A => "RUN",
        0x8B => "IF",
        0x8C => "RESTORE",
        0x8D => "GOSUB",
        0x8E => "RETURN",
        0x8F => "REM",
        0x90 => "STOP",
        0x91 => "ON",
        0x92 => "WAIT",
        0x93 => "LOAD",
        0x94 => "SAVE",
        0x95 => "VERIFY",
        0x96 => "DEF",
        0x97 => "POKE",
        0x98 => "PRINT#",
        0x99 => "PRINT",
        0x9A => "CONT",
        0x9B => "LIST",
        0x9C => "CLR",
        0x9D => "CMD",
        0x9E => "SYS",
        0x9F => "OPEN",
        0xA0 => "CLOSE",
        0xA1 => "GET",
        0xA2 => "NEW",
        0xA3 => "TAB(",
        0xA4 => "TO",
        0xA5 => "FN",
        0xA6 => "SPC(",
        0xA7 => "THEN",
        0xA8 => "NOT",
        0xA9 => "STEP",
        0xAA => "+",
        0xAB => "−",
        0xAC => "*",
        0xAD => "/",
        0xAE => "↑",
        0xAF => "AND",
        0xB0 => "OR",
        0xB1 => ">",
        0xB2 => "=",
        0xB3 => "<",
        0xB4 => "SGN",
        0xB5 => "INT",
        0xB6 => "ABS",
        0xB7 => "USR",
        0xB8 => "FRE",
        0xB9 => "POS",
        0xBA => "SQR",
        0xBB => "RND",
        0xBC => "LOG",
        0xBD => "EXP",
        0xBE => "COS",
        0xBF => "SIN",
        0xC0 => "TAN",
        0xC1 => "ATN",
        0xC2 => "PEEK",
        0xC3 => "LEN",
        0xC4 => "STR$",
        0xC5 => "VAL",
        0xC6 => "ASC",
        0xC7 => "CHR$",
        0xC8 => "LEFT$",
        0xC9 => "RIGHT$",
        0xCA => "MID$",
        0xCB => "GO",
        _ => return None
    }))
}

fn from_petscii(bytes: &Vec<u8>) -> Result<String, String> {
    match String::from_utf8(bytes.clone()) {
        Ok(string) => Ok(string),
        Err(utf8error) => Err(format!("{}", utf8error))
    }
}

pub fn get_word_le(buffer : &[u8], offset: usize) -> u16 {
    let word = buffer[offset + 1] as u16;
    (word << 8) + buffer[offset] as u16
}
