use defs::*;
use std::collections::HashSet;
use std::collections::HashMap;

pub enum Sign {
    Top,
    Minus,
    Zero,
    Plus,
    Bottom
}

type State = HashMap<Var, Sign>;

pub fn analyse(program: Program) {
    let mut variables = HashSet::new();
    for statement in program.statements {
        match statement {
            Statement::Assignment(variable, _) => { variables.insert(variable); },
            Statement::Command(Command::INPUT(_, vars)) =>
                for variable in vars {
                    variables.insert(variable);
                },
            _ => ()
        }
    }

    println!("{:?}", variables);
}
