use defs::*;
use std::collections::HashMap;

pub fn parse(lexical_program : LexProg) -> Program {
    let mut line_map = HashMap::new();

    let mut statements = Vec::new();
    let mut labels = Vec::new();
    let mut data = Vec::new();

    for line in lexical_program {
        let mut tokens = line.lexemes;
        line_map.insert(line.number, statements.len());
        tokens.reverse();

        while tokens.len() > 0 {
            match parse_statement(&mut tokens) {
                Err(err) => {
                    println!("Parsing error on line {}", line.number);
                    panic!("{}", err);
                },
                Ok((line_labels, statement)) => {
                    for label in line_labels {
                        labels.push(label);
                    }
                    statements.push(statement);
                }
            }
        }
    }

    let mut final_labels = HashMap::new();
    for label in labels {
        final_labels.insert(*line_map.get(&label)
            .expect(format!("line {} not found in program", label).as_str())
        , label);
    }

    Program {
        statements: statements,
        labels: final_labels,
        data: data
    }
}

fn parse_statement(tokens: &mut Vec<Lexeme>) -> Result<(Vec<u16>, Statement), String> {
    match tokens.pop() {
        None => Err(String::from("Expected statement")),
        Some(lexeme) => match lexeme {
            Lexeme::Colon => parse_statement(tokens),
            Lexeme::Keyword(byte) => match byte {
                0x0...0x7f | 0xcc...0xff =>
                    Err(String::from("Improper keyword")),
                0x83 => Ok((Vec::new(), Statement::Dummy)),
                0x89 => parse_GOTO(tokens),
                0x8b => parse_IF_THEN(tokens),
                0x8d => parse_GOSUB(tokens),
                _ => Ok((Vec::new(), parse_keyword_statement(byte, tokens)?)),
            },
            Lexeme::Remark(_) => Ok((Vec::new(), Statement::Remark)),
            Lexeme::Var(variable) =>
                Ok((Vec::new(), parse_assignment(variable, tokens)?)),
            _ => Ok((Vec::new(), Statement::Dummy))
        }
    }
}

fn parse_keyword_statement(byte: u8, tokens: &mut Vec<Lexeme>) -> Result<Statement, String> {
    match byte {
        0x85 => parse_INPUT(tokens),
        0x99 => parse_PRINT(tokens),
        0x9c => Ok(Statement::Command(Command::CLR)),
        0x9e => Ok(Statement::Command(Command::SYS(parse_expression(tokens)?))),
        0xa1 => parse_GET(tokens),
        0xb4...0xca => Err(format!("Function {} can't stand alone as statement.",
            Lexeme::Keyword(byte))),
        _ => Ok(Statement::Dummy)
    }
}

fn parse_assignment(variable: Var, tokens: &mut Vec<Lexeme>) -> Result<Statement, String> {
    if let Some(Lexeme::Operator(Op::Equals)) = tokens.pop() {
        Ok(Statement::Assignment(variable, parse_expression(tokens)?))
    } else {
        return Err(String::from("next lexeme should be '='"))
    }
}

fn parse_expression(tokens: &mut Vec<Lexeme>) -> Result<Expr, String> {
    if let Some(lexeme) = tokens.pop() {
        match lexeme {
            Lexeme::Var(variable) => Ok(parse_op(Expr::Var(variable), tokens)?),
            Lexeme::Literal(literal) => Ok(parse_op(Expr::Literal(literal), tokens)?),
            Lexeme::Keyword(keyword) => Ok(parse_function(keyword, tokens)?),
        _ => Ok(Expr::Dummy)
        }
    } else {
        Err(String::from("expected expression"))
    }
}

fn parse_function(index: u8, tokens: &mut Vec<Lexeme>) -> Result<Expr, String> {
    let mut expressions = Vec::new();
    loop {
        match tokens.pop() {
            Some(Lexeme::OpenParen) => {
                expressions.push(parse_expression(tokens)?);
                match tokens.pop() {
                    Some(Lexeme::CloseParen) =>
                        return Ok(parse_op(Expr::Function(index, expressions), tokens)?),
                    Some(Lexeme::Comma) => (),
                    _ => return {
                        Err(format!("Expected ')' for {}(", Lexeme::Keyword(index)))
                    }
                }
            },
            _ => return 
                Err(format!("Expected '(' after {}", Lexeme::Keyword(index)))
        }
    }
}

fn parse_op(expression: Expr, tokens: &mut Vec<Lexeme>) -> Result<Expr, String> {
    match tokens.pop() {
        None => Ok(expression),
        Some(Lexeme::Operator(operator)) =>
            Ok(Expr::BinaryOp(operator, Box::new(expression),
                Box::new(parse_expression(tokens)?))),
        Some(non_operator) => {
            tokens.push(non_operator);
            Ok(expression)
        }
    }
}

#[allow(non_snake_case)]
fn parse_GET(tokens: &mut Vec<Lexeme>) -> Result<Statement, String> {
    match tokens.pop() {
        Some(Lexeme::Var(variable)) =>
            Ok(Statement::Command(Command::GET(variable))),
        _ => Err(String::from("expected variable about GET."))
    }
}

#[allow(non_snake_case)]
fn parse_GOTO(tokens: &mut Vec<Lexeme>) -> Result<(Vec<u16>, Statement), String> {
    match tokens.pop() {
        Some(Lexeme::Literal(Literal::Int(int))) =>
            Ok((vec!(int as u16), Statement::Command(Command::GOTO(int as u16)))),
        _ => Err(String::from("expected int literal after GOTO."))
    }
}

#[allow(non_snake_case)]
fn parse_GOSUB(tokens: &mut Vec<Lexeme>) -> Result<(Vec<u16>, Statement), String> {
    let token = tokens.pop();
    match token {
        Some(Lexeme::Literal(Literal::Int(int))) =>
            Ok((vec!(int as u16), Statement::Command(Command::GOSUB(int as u16)))),
        _ => Err(String::from("expected int literal after GOSUB."))
    }
}

#[allow(non_snake_case)]
fn parse_IF_THEN(tokens: &mut Vec<Lexeme>) -> Result<(Vec<u16>, Statement), String> {
    let conditional_expression = parse_expression(tokens)?;
    let mut statements = Vec::new();
    let mut new_labels = Vec::new();

    match tokens.pop() {
        Some(Lexeme::Keyword(0xa7)) => {         // 0xa7 = NEXT
            while tokens.len() > 0 {
                let (labels, statement) = parse_statement(tokens)?;
                statements.push(statement);
                for label in labels {
                    new_labels.push(label);
                }
            }
            Ok((new_labels, Statement::IfThen(conditional_expression, statements)))
        },
        Some(Lexeme::Keyword(0x89)) => {
            let (labels, statement) = parse_GOTO(tokens)?;
            statements.push(statement);
            for label in labels {
                new_labels.push(label);
            }
            Ok((new_labels, Statement::IfThen(conditional_expression, statements)))
        },
        _ => Err(String::from("expected THEN"))
    }
}

#[allow(non_snake_case)]
fn parse_INPUT(tokens: &mut Vec<Lexeme>) -> Result<Statement, String> {
    let mut prompt = None;
    let mut reading_variables = false;
    let mut variables = Vec::new();

    loop {
        let next_lexeme = tokens.pop();
        match next_lexeme {
            None => break,
            Some(Lexeme::SemiColon) => (),
            Some(Lexeme::Comma) => (),
            Some(Lexeme::Literal(Literal::String(string))) => {
                if reading_variables {
                    return Err(String::from("unexpected prompt"))
                }
                prompt = Some(Literal::String(string));
                reading_variables = true;
            },
            Some(Lexeme::Colon) => break,
            Some(Lexeme::Var(variable)) => {
                reading_variables = true;
                variables.push(variable)
            },
            _ => return Err(String::from(
                "unexpected lexeme at end of INPUT statement"))
        }
    }

    Ok(Statement::Command(Command::INPUT(prompt, variables)))
}

#[allow(non_snake_case)]
fn parse_PRINT(tokens: &mut Vec<Lexeme>) -> Result<Statement, String> {
    let mut items = Vec::new();

    loop {
        let next_lexeme = tokens.pop();
        match next_lexeme {
            None => break,
            Some(Lexeme::SemiColon) => items.push(PrintItem::SemiColon),
            Some(Lexeme::Comma) => items.push(PrintItem::Comma),
            Some(Lexeme::Colon) => break,
            Some(Lexeme::Keyword(index)) => {
                let expression = parse_expression(tokens)?;
                match tokens.pop() {
                    Some(Lexeme::CloseParen) =>
                        match index {
                            0xa3 => items.push(PrintItem::TAB(expression)),
                            0xa6 => items.push(PrintItem::SPC(expression)),
                            _ => return Err(format!("Didn't expect keyword {}",
                            Lexeme::Keyword(index)))
                        }
                    _ => return
                        Err(format!("Expected ')' for {}(", Lexeme::Keyword(index)))
                }
            },
            Some(lexeme) => {
                tokens.push(lexeme);
                items.push(PrintItem::StrExpr(parse_expression(tokens)?))
            }
        }
    }

    Ok(Statement::Command(Command::PRINT(items)))
}
