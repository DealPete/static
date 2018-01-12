use defs::*;
use x86::arch::*;
use x86::state::{State, Flag};

// We assume the PSP is loaded at 0x75a and the program at 0x76a.
// This is what DOSBOX uses as a default.
const PSP_SEGMENT: u16 = 0x75a;
const PROGRAM_SEGMENT: u16 = 0x76a;

pub struct DOS {
    load_module: LoadModule
}

impl DOS {
    pub fn new(file_buffer: &[u8]) -> DOS {
        let mut buffer = Vec::new();
        let header_size = if file_buffer[0..2] == [0x4d, 0x5a] {
            16*get_word_le(&file_buffer, 0x08) as usize
        } else {
            0
        };
        for i in header_size..file_buffer.len() {
            buffer.push(file_buffer[i]);
        }
        DOS {
            load_module: LoadModule {
                file_offset: header_size,
                memory_segment: PROGRAM_SEGMENT,
                buffer: buffer
            }
        }
    }

    pub fn initial_state<'a>(&'a self, file_buffer: &[u8]) -> State<'a> {
        let mut state = State::new(&self.load_module)
            .set_reg16(Register::DS, Word::new(PSP_SEGMENT))
            .set_reg16(Register::ES, Word::new(PSP_SEGMENT));

        if file_buffer[0..2] == [0x4d, 0x5a] {
            state.cs = get_word_le(&file_buffer, 0x16).wrapping_add(PROGRAM_SEGMENT);
            state.ip = get_word_le(&file_buffer, 0x14);
            state.set_reg16(Register::SS,
                Word::new(get_word_le(&file_buffer, 0xe).wrapping_add(PROGRAM_SEGMENT)))
                .set_reg16(Register::SP, Word::new(get_word_le(&file_buffer, 0x10)))
        } else {
            state.cs = PSP_SEGMENT;
            state.ip = 0x100;
            state.set_reg16(Register::SS, Word::new(PSP_SEGMENT))
                .set_reg16(Register::SP, Word::new(0xfffe))
        }
    }
}

impl<'a> Context<State<'a>, Instruction> for DOS {
    fn entry_offset(&self, state: &State<'a>) -> usize {
        16 * state.cs as usize
            + state.ip as usize
            - 16 * self.load_module.memory_segment as usize
    }

    fn next_inst_offset(&self, state: &State<'a>) -> usize {
        let address = state.next_inst_address();
        address - 16 * self.load_module.memory_segment as usize
    }

    fn simulate_system_call(&self, state: State<'a>, inst: Instruction) -> Option<State<'a>> {
        match inst.op1 {
            Some(Operand::Imm8(func)) => {
                match func {
                    0x10 => Some(simulate_int10(state)),
                    0x20 => None,
                    0x21 => simulate_int21(state),
                    _ => Some(state)
                }
            },
            _ => panic!("Expected first operand of INT to be imm8")
        }
    }
}

pub struct LoadModule {
    pub file_offset: usize,
    pub memory_segment: u16,
    pub buffer: Vec<u8>
}

fn simulate_int10<'a>(state: State<'a>) -> State<'a> {
    let ah = state.unpack_reg8(Register::AH)
        .expect("can't call INT 10 with non-deterministic AH.");
    match ah {
        0x00...0x02 | 0x05...0x07 => state,
        0x03 => state
            .set_reg8(Register::DH, Byte::AnyValue)
            .set_reg8(Register::DL, Byte::AnyValue)
            .set_reg8(Register::CH, Byte::AnyValue)
            .set_reg8(Register::CL, Byte::AnyValue),
        _ => state
    }
}

fn simulate_int21<'a>(state: State<'a>) -> Option<State<'a>> {
    let ah = state.unpack_reg8(Register::AH)
        .expect("can't call INT 21 with non-deterministic AH.");
    match ah {
        0x00 | 0x4c => None,
        0x06 => {
            if state.get_reg8(Register::DL).can_be(0xff) {
                Some(state.set_reg8(Register::AL, Byte::AnyValue)
                    .set_flag(Flag::Zero, Bit::TrueAndFalse))
            } else {
                let dl = state.get_reg8(Register::DL);
                Some(state.set_reg8(Register::AL, dl))
            }
        },
        0x09 => Some(state),
        _ => Some(state)
    }
}

