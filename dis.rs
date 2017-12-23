use defs::*;

pub fn decode_instruction(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
	let code = buffer[offset];
	if code < 0x40 && code % 8 < 6 {
		decode_regular_inst(&buffer, offset)
	} else { 
		match code {
			0x06 | 0x07 | 0x0e | 0x16 | 0x17 | 0x1e | 0x1f =>
				Ok(decode_push_pop_segreg(code)),
            0x26 | 0x2e | 0x36 | 0x3e =>
                decode_with_segment_prefix(&buffer, offset),
            0x27 | 0x2f | 0x37 | 0x3f =>
                decode_bcd_inst(code),
            0x40...0x5f => decode_inc_dec_push_pop_reg(&buffer, offset),
			0x70...0x7f | 0xe0...0xe3 | 0xeb =>
                Ok(decode_jump_imm8(&buffer, offset)),
            0x80...0x83 => Ok(decode_imm_inst(&buffer, offset)),
            0x84...0x87 => decode_test_xchg(&buffer, offset),
			0x88...0x8b => decode_regular_inst(&buffer, offset),
            0x8c | 0x8e => decode_move_segment_reg(&buffer, offset),
            0x8d => decode_lea(&buffer, offset),
            0x90 => decode_nop(),
            0x91...0x97 => decode_xchg_ax(code),
            0xa0...0xa3 => Ok(decode_mov_moffset(&buffer, offset)),
            0xa4...0xa7 | 0xaa...0xaf => decode_string_op(&buffer, offset),
            0xa8...0xa9 => decode_test_ax(&buffer, offset),
			0xb0...0xbf => Ok(decode_mov_imm_reg(&buffer, offset)),
            0xc2 | 0xc3 | 0xca | 0xcb => Ok(decode_ret(&buffer, offset)),
            0xc6 | 0xc7 => Ok(decode_mov_imm_reg_mem(&buffer, offset)),
			0xcd => Ok(decode_int(&buffer, offset)),
            0xd0...0xd3 => decode_shift_rotate(&buffer, offset),
            0xe4...0xe7 | 0xec...0xef => decode_in_out(&buffer, offset),
            0xe8 | 0xe9 => Ok(decode_call_jump_rel16(&buffer, offset)),
            0xf2 | 0xf3 => decode_with_rep_prefix(&buffer, offset),
            0xf6 | 0xf7 => decode_f6_f7(&buffer, offset),
            0xf8...0xfd => decode_clear_set_flags(code),
			0xfe => Ok(decode_inc_dec_imm(&buffer, offset)),
            0xff => decode_ff(&buffer, offset),
			_ => Err(format!("Couldn't read instruction at 0x{:x}", offset))
		}
	}
}

fn decode_with_segment_prefix(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    match decode_instruction(&buffer, offset + 1) {
        Err(err) => return Err(err),
        Ok(mut inst) => {
            let seg_prefix = match buffer[offset] {
                0x26 => Register::ES,
                0x2e => Register::CS,
                0x36 => Register::SS,
                0x3e => Register::DS,
                _ => return Err(String::from("Improper prefix"))
            };
            inst.op1 = match inst.op1 {
                Some(Operand::Pointer(pointer)) =>
                    Some(Operand::Pointer(pointer.set_segment(seg_prefix))),
                operand => operand
            };
            inst.op2 = match inst.op2 {
                Some(Operand::Pointer(pointer)) =>
                    Some(Operand::Pointer(pointer.set_segment(seg_prefix))),
                operand => operand
            };
            inst.length += 1;
            return Ok(inst);
        }
    }
}

fn decode_with_rep_prefix(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    match decode_instruction(&buffer, offset + 1) {
        Err(err) => return Err(err),
        Ok(mut inst) => {
            inst.rep_prefix = match buffer[offset] {
                0xf2 => Some(Mnemonic::REPNZ),
                0xf3 => Some(Mnemonic::REPZ),
                _ => return Err(String::from("Improper prefix"))
            };
            inst.length += 1;
            return Ok(inst);
        }
    }
}

fn decode_regular_inst(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
	let code = buffer[offset];

	let mut inst = Instruction::new(
		match code / 8 {
			0 => Mnemonic::ADD,
			1 => Mnemonic::OR,
			2 => Mnemonic::ADC,
			3 => Mnemonic::SBB,
			4 => Mnemonic::AND,
			5 => Mnemonic::SUB,
			6 => Mnemonic::XOR,
			7 => Mnemonic::CMP,
			0x11 => Mnemonic::MOV,
			_ => return Err(format!("irregular instruction at 0x{:x}", offset))
		}
	);
			
	match code & 7 {
		0...3 =>
			{
			let (op1, op2, length) = decode_mod_rm(buffer, offset + 1, code & 1, code & 2);
			inst.op1 = op1;
			inst.op2 = op2;
			inst.length = length + 1;
			},
		4 =>
			{
			inst.op1 = Some(Operand::Register8(Register::AL));
			inst.op2 = Some(Operand::Imm8(buffer[offset + 1] as i8));
			inst.length = 2;
			},
		5 => 
			{
			inst.op1 = Some(Operand::Register16(Register::AX));
			inst.op2 = Some(Operand::Imm16(get_word(&buffer, offset + 1) as i16));
			inst.length = 3;
			},
		_ => panic!("Improper code type")
	}

	return Ok(inst);
}

fn decode_push_pop_segreg(code: u8) -> Instruction {
	let mut inst = Instruction::new(match code & 1 {
		0 => Mnemonic::PUSH,
		_ => Mnemonic::POP
	});
	inst.op1 = Some(match code & 0x18 {
		0x00 => Operand::Register16(Register::ES),
		0x08 => Operand::Register16(Register::CS),
		0x10 => Operand::Register16(Register::SS),
		0x18 => Operand::Register16(Register::DS),
		_ => panic!("unknown segment register")
	});
	inst.length = 1;
	return inst;
}

fn decode_bcd_inst(code: u8) -> Result<Instruction, String> {
    let mut inst = Instruction::new(match code {
        0x27 => Mnemonic::DAA,
        0x2f => Mnemonic::DAS,
        0x37 => Mnemonic::AAA,
        0x3f => Mnemonic::AAS,
        _ => panic!("not a bcd instruction")
    });
    inst.length = 1;
    return Ok(inst);
}
        
fn decode_inc_dec_push_pop_reg(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let mut inst = Instruction::new(match buffer[offset] {
        0x40...0x47 => Mnemonic::INC,
        0x48...0x4f => Mnemonic::DEC,
        0x50...0x57 => Mnemonic::PUSH,
        0x58...0x5f => Mnemonic::POP,
        _ => return Err(String::from("Not a inc dec push pop instruction!"))
    });
    inst.op1 = Some(Operand::Register16(reg16(buffer[offset] & 7)));
    inst.length = 1;
    return Ok(inst);
}

fn decode_jump_imm8(buffer: &[u8], offset: usize) -> Instruction {
	let mut inst = Instruction::new(match buffer[offset] {
		0x70 => Mnemonic::JO,
		0x71 => Mnemonic::JNO,
		0x72 => Mnemonic::JB,
		0x73 => Mnemonic::JNB,
		0x74 => Mnemonic::JZ,
		0x75 => Mnemonic::JNZ,
		0x76 => Mnemonic::JBE,
		0x77 => Mnemonic::JNBE,
		0x78 => Mnemonic::JS,
		0x79 => Mnemonic::JNS,
		0x7a => Mnemonic::JP,
		0x7b => Mnemonic::JNP,
		0x7c => Mnemonic::JL,
		0x7d => Mnemonic::JNL,
		0x7e => Mnemonic::JLE,
		0x7f => Mnemonic::JNLE,
        0xe0 => Mnemonic::LOOPNZ,
        0xe1 => Mnemonic::LOOPZ,
        0xe2 => Mnemonic::LOOP,
        0xe3 => Mnemonic::JCXZ,
		0xeb => Mnemonic::JMP,
		_ => panic!("Not a jump instruction!")
	});
	inst.op1 = Some(Operand::Imm8(buffer[offset + 1] as i8));
	inst.length = 2;
    return inst;
}

fn decode_imm_inst(buffer: &[u8], offset: usize) -> Instruction {
	let code = buffer[offset];
	let mut inst = Instruction::new(
		match buffer[offset + 1] & 0x38 {
			0x00 => Mnemonic::ADD,
			0x08 => Mnemonic::OR,
            0x10 => Mnemonic::ADC,
            0x18 => Mnemonic::SBB,
            0x20 => Mnemonic::AND,
            0x28 => Mnemonic::SUB,
            0x30 => Mnemonic::XOR,
            0x38 => Mnemonic::CMP,
            _ => panic!("Improper extension")
        }
	);
	
    let (op, _, length) = decode_mod_rm(buffer, offset + 1, code & 1, 0);
    inst.op1 = op;
    if code == 0x81 {
        inst.op2 = Some(Operand::Imm16(get_word(&buffer, offset + 1 + length as usize) as i16));
        inst.length = length + 3;
    } else {
        inst.op2 = Some(Operand::Imm8(buffer[offset + 1 + length as usize] as i8));
        inst.length = length + 2;
    }
    return inst;
}

fn decode_test_xchg(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = buffer[offset];
    let mut inst = Instruction::new(match code & 2 {
        0 => Mnemonic::TEST,
        _ => Mnemonic::XCHG
    });
    let (op1, op2, length) = decode_mod_rm(buffer, offset + 1, code & 1, 0);
    inst.op1 = op1;
    inst.op2 = op2;
    inst.length = length + 1;
    return Ok(inst);
}

fn decode_move_segment_reg(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = buffer[offset];
    let mut inst = Instruction::new(Mnemonic::MOV);
    let (op, _, length) = decode_mod_rm(buffer, offset + 1, 1, 0);
    let segment = Some(Operand::Register16(seg_reg(buffer[offset + 1] & 0x38)));
    match code {
        0x8c => {
            inst.op1 = op;
            inst.op2 = segment;
            },
        0x8e => {
            inst.op1 = segment;
            inst.op2 = op;
            },
        _ => panic!("incorrect opcode")
    }
    inst.length = length + 1;
    return Ok(inst);
}

fn decode_lea(buffer: &[u8], offset:usize) -> Result<Instruction, String> {
    let mut inst = Instruction::new(Mnemonic::LEA);
    let (op1, op2, length) = decode_mod_rm(buffer, offset + 1, 1, 2);
    inst.op1 = op1;
    inst.op2 = op2;
    inst.length = 1 + length;
    return Ok(inst);
}

fn decode_nop() -> Result<Instruction, String> {
    let mut inst = Instruction::new(Mnemonic::NOP);
    inst.length = 1;
    return Ok(inst);
}

fn decode_xchg_ax(code: u8) -> Result<Instruction, String> {
    let mut inst = Instruction::new(Mnemonic::XCHG);
    inst.op1 = Some(Operand::Register16(reg16(code & 7)));
    inst.op2 = Some(Operand::Register16(Register::AX));
    inst.length = 1;
    return Ok(inst);
}

fn decode_mov_moffset(buffer: &[u8], offset: usize) -> Instruction {
    let code = buffer[offset];
    let size = code & 1;
    let mut inst = Instruction::new(Mnemonic::MOV);
    let op1 = Some(if size != 0 {
        Operand::Register8(Register::AL)
    } else {
        Operand::Register16(Register::AX)
    });
    let op2 = Some(Operand::Pointer(
        Pointer::new(size, PtrType::Disp16(get_word(buffer, offset + 1)))));
    if code & 2 != 0 {
        inst.op1 = op2;
        inst.op2 = op1;
    } else {
        inst.op1 = op1;
        inst.op2 = op2;
    }
    inst.length = 3;
    return inst;
}

fn decode_string_op(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = buffer[offset];
    let mut inst = Instruction::new(match code {
        0xa4 => Mnemonic::MOVSB,
        0xa5 => Mnemonic::MOVSW,
        0xa6 => Mnemonic::CMPSB,
        0xa7 => Mnemonic::CMPSW,
        0xaa => Mnemonic::STOSB,
        0xab => Mnemonic::STOSW,
        0xac => Mnemonic::LODSB,
        0xad => Mnemonic::LODSW,
        0xae => Mnemonic::SCASB,
        0xaf => Mnemonic::SCASW,
        _ => return Err(String::from("Not a string instruction!"))
    });
    inst.length = 1;
    return Ok(inst);
}

fn decode_test_ax(buffer: &[u8], offset:usize) -> Result<Instruction, String> {
    let code = buffer[offset];
    let size = code & 1;
    let mut inst = Instruction::new(Mnemonic::TEST);
    inst.op1 = match code {
        0xa8 => Some(Operand::Register8(Register::AL)),
        0xa9 => Some(Operand::Register16(Register::AX)),
        _ => panic!("wrong opcode")
    };
    inst.op2 = Some(match size {
        0 => Operand::Imm8(buffer[offset + 1] as i8),
        _ => Operand::Imm16(get_word(buffer, offset + 1) as i16),
    });
    inst.length = 2 + size;
    return Ok(inst);
}

fn decode_mov_imm_reg(buffer: &[u8], offset: usize) -> Instruction {
	let code = buffer[offset];
	let mut inst = Instruction::new(Mnemonic::MOV);
	match code & 8 {
		0 =>
			{
			inst.op1 = Some(Operand::Register8(reg8(code & 7)));
			inst.op2 = Some(Operand::Imm8(buffer[offset + 1] as i8));
			inst.length = 2;
			},
		_ =>
			{
			inst.op1 = Some(Operand::Register16(reg16(code & 7)));
			inst.op2 = Some(Operand::Imm16(get_word(&buffer, offset + 1) as i16));
			inst.length = 3;
			}
	}
	return inst;
}

fn decode_ret(buffer: &[u8], offset: usize) -> Instruction {
    let mut inst = Instruction::new(Mnemonic::RET);
    if buffer[offset] & 1 == 0 {
        inst.op1 = Some(Operand::Imm16(buffer[offset + 1] as i16));
        inst.length = 3;
    } else {
        inst.length = 1;
    }
    return inst;
}

fn decode_mov_imm_reg_mem(buffer: &[u8], offset: usize) -> Instruction {
    let mut inst = Instruction::new(Mnemonic::MOV);
    let size = buffer[offset] & 1;
    let (op, _, mod_rm_length) = decode_mod_rm(buffer, offset + 1, size, 0);
    inst.op1 = op;
    match size {
        0 =>
            {
            inst.op2 = Some(Operand::Imm8(buffer[offset + 1 + mod_rm_length as usize] as i8));
            inst.length = 1 + mod_rm_length + 1;
            },
        _ =>
            {
            inst.op2 = Some(Operand::Imm16(get_word(buffer, offset + 1 + mod_rm_length as usize) as i16));
            inst.length = 1 + mod_rm_length + 2;
            }
    }
    return inst;
}

fn decode_int(buffer: &[u8], offset: usize) -> Instruction {
	let mut inst = Instruction::new(Mnemonic::INT);
	inst.op1 = Some(Operand::Imm8(buffer[offset + 1] as i8));
	inst.length = 2;
	return inst;
}

fn decode_shift_rotate(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = buffer[offset];
    let mut inst = Instruction::new(match buffer[offset + 1] & 0x38 {
        0x00 => Mnemonic::ROL,
        0x08 => Mnemonic::ROR,
        0x10 => Mnemonic::RCL,
        0x18 => Mnemonic::RCR,
        0x20 => Mnemonic::SHL,
        0x28 => Mnemonic::SHR,
        0x30 => Mnemonic::SAL,
        0x38 => Mnemonic::SAR,
        _ => panic!("incorrect extension code!")
    });
    let (op, _, length) = decode_mod_rm(buffer, offset + 1, code & 1, 0);
    inst.op1 = op;
    inst.op2 = Some(if code & 2 == 0 {
        Operand::Imm8(0x1)
    } else {
        Operand::Register8(Register::CL)
    });
    inst.length = 1 + length;
    return Ok(inst);
}

fn decode_in_out(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = buffer[offset];

    let mut inst = Instruction::new(
        if code & 2 == 0 {
            Mnemonic::IN
        } else {
            Mnemonic::OUT
        }
    );
    
    let dest = if code & 1 == 0 {
        Operand::Register8(Register::AL)
    } else {
        Operand::Register16(Register::AX)
    };

    let (source, length) = if code & 8 == 0 {
        (Operand::Imm8(buffer[offset + 1] as i8), 2)
    } else {
        (Operand::Register16(Register::DX), 1)
    };

    inst.op1 = if code & 2 == 0 { Some(dest) } else { Some(source) };
    inst.op2 = if code & 2 == 0 { Some(source) } else { Some(dest) };
    inst.length = length;

    return Ok(inst);
}

fn decode_call_jump_rel16(buffer: &[u8], offset: usize) -> Instruction {
    let mut inst = Instruction::new(
        match buffer[offset] {
            0xe8 => Mnemonic::CALL,
            _ => Mnemonic::JMP
        }
    );
    inst.op1 = Some(Operand::Imm16(get_word(buffer, offset + 1) as i16));
    inst.length = 3;
    return inst;
}

fn decode_f6_f7(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = buffer[offset];
    let extension = (buffer[offset + 1] & 0x38) >> 3;
    let (op, _, r_length) = decode_mod_rm(buffer, offset + 1, code & 1, 0);
    let mut inst;
    match extension {
        0 | 1 =>
            {
            inst = Instruction::new(Mnemonic::TEST);
            inst.op1 = op;
            if code & 1 == 0 {
                inst.op2 = Some(Operand::Imm8(buffer[offset + 1] as i8));
                inst.length = 2 + r_length;
            } else {
                inst.op2 = Some(Operand::Imm16(get_word(buffer, offset + 1 + r_length as usize) as i16));
                inst.length = 3 + r_length;
            }
            },
        _ =>
            {
            inst = Instruction::new(match extension {
                2 => Mnemonic::NOT,
                3 => Mnemonic::NEG,
                4 => Mnemonic::MUL,
                5 => Mnemonic::IMUL,
                6 => Mnemonic::DIV,
                7 => Mnemonic::IDIV,
                _ => panic!("improper extension!")
            });
            inst.op1 = op;
            inst.length = 1 + r_length;
            }
    }
    return Ok(inst);
}

fn decode_clear_set_flags(code: u8) -> Result<Instruction, String> {
    let mut inst = Instruction::new(
        match code {
            0xf8 => Mnemonic::CLC,
            0xf9 => Mnemonic::STC,
            0xfa => Mnemonic::CLI,
            0xfb => Mnemonic::STI,
            0xfc => Mnemonic::CLD,
            0xfd => Mnemonic::STD,
            _ => panic!("not a clear set instruction")
        }
    );
    inst.length = 1;
    return Ok(inst);
}

fn decode_inc_dec_imm(buffer: &[u8], offset: usize) -> Instruction {
	let mut inst = Instruction::new(
		match buffer[offset + 1] & 0x38 {
			0 => Mnemonic::INC,
			_ => Mnemonic::DEC
		}
	);
	
	let (op, _, length) = decode_mod_rm(buffer, offset + 1, buffer[offset] & 1, 0);
	inst.op1 = op;
	inst.length = 1 + length;
	return inst;
}

fn decode_ff(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let extension = (buffer[offset + 1] & 0x38) >> 3;
    let (op, _, length) = decode_mod_rm(buffer, offset + 1, 1, 0);
    let mut inst = Instruction::new(
        match extension {
            0x00 => Mnemonic::INC,
            0x01 => Mnemonic::DEC,
            0x02 | 0x03 => Mnemonic::CALL,
            0x04 | 0x05 => Mnemonic::JMP,
            0x06 => Mnemonic::PUSH,
            _ => panic!("wrong extension code for op 0xff")
        }
    );
    match extension {
        0x3 | 0x5 => return Err(format!("indirect branch at {:x}", offset)),
        _ => ()
    }
    inst.op1 = op;
    inst.length = length + 1;
    return Ok(inst);
}

fn decode_mod_rm(buffer: &[u8], offset: usize, size: u8, dir: u8) -> (Option<Operand>, Option<Operand>, u8) {
    let reg = |op| {
        match size {
            0 => Operand::Register8(reg8(op)),
            1 => Operand::Register16(reg16(op)),
            _ => panic!("Improper size")
        }
    };
	let byte = buffer[offset];
	let mode = byte >> 6;
	let op1 = (byte & 0x38) >> 3;
	let op2 = byte & 7;
    let (r_op1, r_op2, r_length) = match mode {
        0 => match op2
            {
            0 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::RegReg(Register::BX, Register::SI))), 1),
            1 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::RegReg(Register::BX, Register::DI))), 1),
            2 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::RegReg(Register::BP, Register::SI))), 1),
            3 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::RegReg(Register::BP, Register::DI))), 1),
            4 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::Reg(Register::SI))), 1),
            5 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::Reg(Register::DI))), 1),
            6 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::Disp16(get_word(&buffer, offset + 1)))), 3),
            7 => (reg(op1), Operand::Pointer(
                Pointer::new(size, PtrType::Reg(Register::BX))), 1),
            _ => panic!("Unknown r/m bits!")
            },
        1 => (reg(op1), match op2 {
            0 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp8(Register::BX, Register::SI, buffer[offset + 1]))),
            1 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp8(Register::BX, Register::DI, buffer[offset + 1]))),
            2 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp8(Register::BP, Register::SI, buffer[offset + 1]))),
            3 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp8(Register::BP, Register::DI, buffer[offset + 1]))),
            4 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp8(Register::SI, buffer[offset + 1]))),
            5 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp8(Register::DI, buffer[offset + 1]))),
            6 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp8(Register::BP, buffer[offset + 1]))),
            7 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp8(Register::BX, buffer[offset + 1]))),
            _ => panic!("Unknown r/m bits!")
            }, 2),
        2 => (reg(op1), match op2 {
            0 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp16(Register::BX, Register::SI, get_word(&buffer, offset + 1)))),
            1 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp16(Register::BX, Register::DI, get_word(&buffer, offset + 1)))),
            2 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp16(Register::BP, Register::SI, get_word(&buffer, offset + 1)))),
            3 => Operand::Pointer(Pointer::new(size,
                PtrType::RegRegDisp16(Register::BP, Register::DI, get_word(&buffer, offset + 1)))),
            4 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp16(Register::SI, get_word(&buffer, offset + 1)))),
            5 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp16(Register::DI, get_word(&buffer, offset + 1)))),
            6 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp16(Register::BP, get_word(&buffer, offset + 1)))),
            7 => Operand::Pointer(Pointer::new(size,
                PtrType::RegDisp16(Register::BX, get_word(&buffer, offset + 1)))),
            _ => panic!("Unknown r/m bits!")
            }, 3),
        3 => (reg(op1), reg(op2), 1),
        _ => panic!("Unknown mode!")
    };
    if dir == 0 {
        (Some(r_op2), Some(r_op1), r_length)
    } else {
        (Some(r_op1), Some(r_op2), r_length)
    }
}

fn reg8(reg: u8) -> Register {
	match reg {
		0 => Register::AL,
		1 => Register::CL,
		2 => Register::DL,
		3 => Register::BL,
		4 => Register::AH,
		5 => Register::CH,
		6 => Register::DH,
		7 => Register::BH,
		_ => panic!("Incorrect register")
	}
}

fn reg16(reg: u8) -> Register {
	match reg {
		0 => Register::AX,
		1 => Register::CX,
		2 => Register::DX,
		3 => Register::BX,
		4 => Register::SP,
		5 => Register::BP,
		6 => Register::SI,
		7 => Register::DI,
		_ => panic!("Incorrect register")
	}
}

fn seg_reg(reg: u8) -> Register {
    match reg {
        0x00 => Register::ES,
        0x08 => Register::CS,
        0x10 => Register::SS,
        0x18 => Register::DS,
        0x20 => Register::FS,
        0x28 => Register::GS,
        _ => panic!("incorrect segment register")
    }
}
