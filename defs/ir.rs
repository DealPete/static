//use defs::main::InstructionTrait;

struct Instruction {
    mnemonic: Mnemonic,
    op1: Option<Operand>,
    op2: Option<Operand>,
    op3: Option<Operand>
}

enum Operand {
    Bit(bool),
    Byte(u8),
    Word(u16),
    Flag(bool),
    Reg8(usize),
    Reg16(usize),
    Node(usize)
}

enum Mnemonic {
    ADD, SUB, MUL, DIV, MOD, BSH, AND, OR, XOR,
    LDM, STR, STM, CMPZ, JMPZ, CALL, RET, ERROR
}
