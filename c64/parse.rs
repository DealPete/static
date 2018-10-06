use defs::*;
use std::collections::HashMap;
use std::vec::IntoIter;

pub fn parse(lexical_program : LexProg) -> Program {
    let mut line_map = HashMap::new();

    for (index, line) in lexical_program.iter().enumerate() {
        line_map.insert(line.number, index);
    }

    let mut statements = Vec::new();
    let mut labels = Vec::new();
    let mut data = Vec::new();

    for line in lexical_program {
        let mut iterator = line.lexemes.into_iter();
        loop {
            match iterator.next() {
                None => break,
                Some(lexeme) => match lexeme {
                    Lexeme::Keyword(byte) => match byte {
                        0x99 => statements.push(parse_PRINT(&iterator)),
                        _ => ()
                    },
                    _ => ()
                }
            }
        }
    }

    Program {
        statements: statements,
        labels: labels,
        data: data
    }
}

fn parse_PRINT(iterator: &IntoIter<Lexeme>) -> Statement {
    Statement::Command(Command::PRINT(Vec::new()))
}
