use std::fmt;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub labels: HashMap<usize, u16>,
    pub data: Vec<u16>
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Var, Expr),
    Command(Command),
    IfThen(Expr, Vec<Statement>),
    Remark,
    Dummy
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Var(Var),
    BinaryOp(Op, Box<Expr>, Box<Expr>),
    Function(u8, Vec<Expr>),
    Dummy
}

#[derive(Debug)]
pub enum Command {
    CLR,
    GET(Var),
    GOSUB(u16),
    GOTO(u16),
    INPUT(Option<Literal>, Vec<Var>),
    PRINT(Vec<PrintItem>),
    SYS(Expr)
}

#[derive(Debug)]
pub enum PrintItem {
    StrExpr(Expr),
    TAB(Expr),
    SPC(Expr),
    SemiColon,
    Comma
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Var {
    Num(String),
    String(String),
    Int(String)
}

#[derive(Debug)]
pub enum Literal {
    Int(isize),
    Float(f32),
    String(Vec<u8>)
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for (index, statement) in self.statements.iter().enumerate() {
            if let Some(line_number) = self.labels.get(&index) {
                output.push_str(&format!("{}:", line_number));
            }
            output.push_str(&format!("\t{}\n", statement));
        }

        write!(f, "{}", output)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Assignment(ref var, ref expr) => write!(f, "{} = {}", var, expr),
            Statement::Command(ref command) => write!(f, "{}", command),
            Statement::IfThen(ref expr, ref statements) => write!(f, "{}",
                format_if_then(expr, statements, 1)),
            Statement::Remark => write!(f, "REM ..."),
            Statement::Dummy => write!(f, "(dummy)")
        }
    }
}

fn format_if_then(expr: &Expr, statements: &Vec<Statement>, depth: usize) -> String {
    let mut output = String::new();    
    output.push_str(format!("IF {} THEN:", expr).as_str());
    for statement in statements {
        output.push('\n');
        match *statement {
            Statement::IfThen(ref sub_expr, ref sub_statements) =>
                output.push_str(format_if_then(sub_expr, sub_statements, depth + 1).as_str()),
            _ => {
                for _i in 0..depth+1 {
                    output.push('\t');
                }
                output.push_str(format!("{}", statement).as_str())
            }
        }
    }

    output
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Command::CLR => write!(f, "CLR"),
            Command::GET(ref variable) => write!(f, "GET {}", variable),
            Command::INPUT(ref prompt, ref vars) => {
                let mut output = match *prompt {
                    None => String::from("INPUT "),
                    Some(ref prompt) => format!("INPUT {}; ", prompt)
                };

                for var in vars {
                    output.push_str(format!("{}, ", var).as_str());
                }
                write!(f, "{}", output)
            },
            Command::GOTO(line) => write!(f, "GOTO {}", line),
            Command::GOSUB(line) => write!(f, "GOSUB {}", line),
            Command::PRINT(ref printitems) => {
                let mut output = String::from("PRINT ");
                for item in printitems {
                    output.push_str(format!("{}", item).as_str());
                }
                write!(f, "{}", output)
            },
            Command::SYS(ref line) => write!(f, "SYS {}", line)
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Literal(ref literal) => write!(f, "{}", literal),
            Expr::Var(ref var) => write!(f, "{}", var),
            Expr::BinaryOp(ref op, ref expr1, ref expr2) =>
                write!(f, "({} {} {})", expr1, op, expr2),
            Expr::Function(index, ref exprs) => {
                let mut expressions = Vec::new();
                for expr in exprs {
                    expressions.push(format!("{}", expr));
                }
                write!(f, "{}({})", Lexeme::Keyword(index), expressions.join(", "))
            },
            Expr::Dummy => write!(f, "(dummy)")
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Num(ref string) => write!(f, "{}", string),
            Var::String(ref string) => write!(f, "{}$", string),
            Var::Int(ref string) => write!(f, "{}%", string)
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Literal::Int(int) => write!(f, "{}", int),
            Literal::Float(float) => write!(f, "{}", float),
            Literal::String(ref bytes) => match from_petscii(bytes) {
                Ok(string) => write!(f, "\"{}\"", string),
                Err(_) => write!(f, "String: {:?}", bytes)
            }
        }
    }
}

impl fmt::Display for PrintItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            PrintItem::StrExpr(ref expr) => format!("{}", expr),
            PrintItem::TAB(ref tabs) => format!("TAB({})", tabs),
            PrintItem::SPC(ref spaces) => format!("SPC({})", spaces),
            PrintItem::SemiColon => String::from("; "),
            PrintItem::Comma => String::from(", ")
        })
    }
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

#[derive(Debug)]
pub enum Lexeme {
    OpenParen,
    CloseParen,
    SemiColon,
    Colon,
    Comma,
    Operator(Op),
    Keyword(u8),
    Var(Var),
    Literal(Literal),
    Remark(Vec<u8>),
    Junk(Vec<u8>)
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Mult,
    Sub,
    Div,
    Exp,
    And,
    Or,
    LessThan,
    Equals,
    GreaterThan
}

impl fmt::Display for Lexeme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Lexeme::OpenParen => String::from("("),
            Lexeme::CloseParen => String::from(")"),
            Lexeme::SemiColon => String::from(";"),
            Lexeme::Colon => String::from(":"),
            Lexeme::Comma => String::from(","),
            Lexeme::Operator(ref op) => format!("{}", op),
            Lexeme::Var(ref variable) => format!("{}", variable),
            Lexeme::Literal(ref literal) => format!("{}", literal),
            Lexeme::Keyword(byte) => keyword(byte).unwrap(),
            Lexeme::Remark(ref bytes) => match from_petscii(bytes) {
                Ok(string) => format!("REM {}", string),
                Err(_) => format!("Remark: {:?}", bytes)
            },
            Lexeme::Junk(ref bytes) => format!("Junk: {:?}", bytes)
        })
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", String::from(match *self {
            Op::Add => "+",
            Op::Mult => "*",
            Op::Sub => "-",
            Op::Div => "/",
            Op::Exp => "^",
            Op::And => "AND",
            Op::Or => "OR",
            Op::LessThan => "<",
            Op::Equals => "=",
            Op::GreaterThan => ">"
        }))
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
