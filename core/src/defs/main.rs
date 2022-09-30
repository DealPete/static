use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub trait Architecture<I: InstructionTrait> : Copy + Clone {
    fn decode_instruction(&self, buffer: &[u8], offset: usize) -> Result<I, String>;
    fn print_listing(listing: &Listing<I>);
}

pub trait SimulatorTrait<S: StateTrait<S>, I: InstructionTrait> {
    fn simulate_next_instruction(&self, state: S, instruction: I) -> SimResult<S>;
    fn simulate_system_call(&self, state: S, inst: I) -> Option<S>;
    fn next_inst_offset(state: &S) -> usize;
}

pub trait StateTrait<S: StateTrait<S>> : Clone + fmt::Display {
    fn union(self, state: S) -> S;
    fn is_subset(&self, state: &S) -> bool;
    fn combine(&self, state: &S) -> CombineResult<S>;
    fn debug_string(&self) -> String;
}

pub trait InstructionTrait : Copy + Clone + fmt::Display {
    fn length(&self) -> usize;
    fn is_call(&self) -> bool;
    fn is_return(&self) -> bool;
    fn is_rel_branch(&self) -> bool;
    fn writes_memory(&self) -> bool;

    // successors(instruction, offset) ->
    //  (targets, calls, branch, indeterminate)
    // successors computes the instruction's successors from
    // its optype and operands, assuming it is found at offset in the
    // file buffer.
    //
    // "targets" contains the non-call successor offsets.
    // "calls" contains the successor offsets that are function calls.
    // "branch" is true if the instruction ends a basic block.
    //
    // "indeterminate" is true if the set of successor offsets
    // couldn't be determined without knowledge of program state.
    // (e.g. because instruction is an indirect jump or an interrupt
    // that might end the program).

    fn successors(&self, offset: usize) -> (Vec<usize>, Vec<usize>, bool, bool);
}

// Meta-instructions to help with decoding self-modifying code:
//
// Inst(I):
// wraps a regular instruction. This is only meta-instruction used
// for the basic recursive descent algorithm.
//
// Jmp(target_node):
// Unconditional jump to target_node.
//
// JmpTrue(target_node, flag_num):
// Jump to target_node if meta flag flag_num is set.
//
// SetFlag(flag_num):
// Sets meta flag flag_num

#[derive(Clone)]
pub enum Meta<I: InstructionTrait> {
    Inst(I),
    Jmp(usize),
    JmpTrue(usize, usize),
    SetFlag(usize)
}

impl<I: InstructionTrait> Meta<I> {
    pub fn unwrap(&self) -> I {
        match *self {
            Meta::Inst(instruction) => instruction,
            _ => panic!("can't unwrap meta-instruction")
        }
    }
}

impl<I: InstructionTrait> fmt::Display for Meta<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Meta::Inst(instruction) => write!(f, "{}", instruction),
            Meta::Jmp(node) => write!(f, "<JMP NODE {}>", node),
            Meta::JmpTrue(node, flag) => write!(f, "<JMPF {}, F{}>", node, flag),
            Meta::SetFlag(flag) => write!(f, "<SETF {}>", flag)
        }
    }
}

#[derive(Clone)]
pub struct Listing<I: InstructionTrait> {
    pub entry_offset: usize,
    pub highest_offset: usize,
    pub instructions: HashMap<usize, Meta<I>>,
    labels: HashSet<usize>,
    indeterminates: HashSet<usize>,
    iter_offset: usize
}

impl<I: InstructionTrait> Listing<I> {
    pub fn new() -> Listing<I> {
        Listing {
            entry_offset: 0,
            highest_offset: 0,
            instructions: HashMap::<usize, Meta<I>>::new(),
            labels: HashSet::new(),
            indeterminates: HashSet::new(),
            iter_offset: 0
        }
    }

    pub fn with_entry(entry_offset: usize) -> Listing<I> {
        Listing {
            entry_offset: entry_offset,
            highest_offset: entry_offset, 
            instructions: HashMap::new(),
            labels: HashSet::new(),
            indeterminates: HashSet::new(),
            iter_offset: 0
        }
    }

    pub fn add(&mut self, offset: usize, instruction: I) {
        if offset > self.highest_offset {
            self.highest_offset = offset;
        }

        self.instructions.insert(offset, Meta::Inst(instruction));
    }

    pub fn get(&self, offset: usize) -> Option<&Meta<I>> {
        self.instructions.get(&offset)
    }

    pub fn add_label(&mut self, offset: usize) {
        self.labels.insert(offset);
    }

    pub fn add_indeterminate(&mut self, offset: usize) {
        self.indeterminates.insert(offset);
    }

    pub fn is_labelled(&self, offset: usize) -> bool {
        self.labels.contains(&offset)
    }
    
    pub fn is_indeterminate(&self, offset: usize) -> bool {
        self.indeterminates.contains(&offset)
    }
}

impl<I: InstructionTrait> Iterator for Listing<I> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        loop {
            if self.iter_offset > self.highest_offset {
                self.iter_offset = 0;
                return None;
            }

            self.iter_offset += 1;
            if let Some(_) = self.get(self.iter_offset - 1) {
                return Some(self.iter_offset - 1);
            }
        }
    }
}



pub enum SimResult<S: StateTrait<S>> {
    Error(S, String),
    End,
    State(S),
    Branch(Vec<S>, Vec<usize>)
}

pub enum CombineResult<S: StateTrait<S>> {
    Subset,
    Superset,
    Uncombinable,
    ExtendSelf(S),
    Combination(S)
}

pub fn get_word_le(buffer : &[u8], offset: usize) -> u16 {
    let word = buffer[offset + 1] as u16;
    (word << 8) + buffer[offset] as u16
}

pub fn get_word_be(buffer : &[u8], offset: usize) -> u16 {
    let word = buffer[offset] as u16;
    (word << 8) + buffer[offset + 1] as u16
}

pub fn add_rel8(address: usize, rel: i8) -> usize {
    if rel < 0 {
        address - (-(rel as isize) as usize)
    } else {
        address + (rel as usize)
    }
}

pub fn add_rel16(address: usize, rel: i16) -> usize {
    if rel < 0 {
        address - ((-rel) as usize)
    } else {
        address + (rel as usize)
    }
}
