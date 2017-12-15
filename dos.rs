use analyse;
use defs::*;
use state::*;

// We assume the PSP is loaded at 0x75a and the program at 0x76a.
// This is what DOSBOX uses as a default.
const PSP_SEGMENT: u16 = 0x75a;
const PROGRAM_SEGMENT: u16 = 0x76a;

pub struct DOS {
}

impl<'program> Context<'program> for DOS {
    fn simulate_int(&self, state: State<'program>, inst: &Instruction) -> State<'program> {
        state
    }

    fn does_int_always_end_program(&self, inst_index: usize, program: &Program) -> bool {
        let values = vec!(0, 0x4c).into_iter().collect();
        return analyse::reg8_at_is_always_in(Register::AH, inst_index, values, program, self);
    }

    fn does_int_end_program(&self, instruction: &Instruction) -> bool {
        if let Some(Operand::Imm8(func)) = instruction.op1 {
            return func == 0x20;
        }
        panic!("Expected first operand of INT to be imm8");
    }

    fn is_int_loose_end(&self, instruction: &Instruction) -> bool {
        if let Some(Operand::Imm8(func)) = instruction.op1 {
            return func == 0x21;
        }
        panic!("Expected first operand of INT to be imm8");
    }
}

pub fn initial_state<'program>(file_buffer: Vec<u8>, load_module: &'program LoadModule) -> State<'program> {
    let mut state = State::new(load_module)
        .set_reg16(Register::DS, Word::new(PSP_SEGMENT))
        .set_reg16(Register::ES, Word::new(PSP_SEGMENT));

    if file_buffer[0..2] == [0x4d, 0x5a] {
        state.cs = get_word(&file_buffer, 0x16).wrapping_add(PROGRAM_SEGMENT);
        state.ip = get_word(&file_buffer, 0x14);
        state.set_reg16(Register::SS,
            Word::new(get_word(&file_buffer, 0xe).wrapping_add(PROGRAM_SEGMENT)))
            .set_reg16(Register::SP, Word::new(get_word(&file_buffer, 0x10)))
    } else {
        state.cs = PSP_SEGMENT;
        state.ip = 0x100;
        state.set_reg16(Register::SS, Word::new(PSP_SEGMENT))
            .set_reg16(Register::SP, Word::new(0xfffe))
    }
}

pub fn load_module(file_buffer: &[u8]) -> LoadModule {
    let mut buffer = Vec::new();
    let header_size = if file_buffer[0..2] == [0x4d, 0x5a] {
        16*get_word(&file_buffer, 0x08) as usize
    } else {
        0
    };
    for i in header_size..file_buffer.len() {
        buffer.push(file_buffer[i]);
    }
    LoadModule {
        file_offset: header_size,
        memory_segment: PROGRAM_SEGMENT,
        buffer: buffer
    }
}

