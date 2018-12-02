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

    // successors(instruction, offset) -> (next, jumps, indeterminate)
    // successors computes the instruction's successors from
    // its optype and operands, assuming it is found at offset in the
    // file buffer.
    //
    // "next" normally contains zero or one offsets representing the
    // next instruction in normal control flow. May contain more
    // instructions if the targets shouldn't be labelled.
    //
    // "jumps" is a vector of possible jump targets.
    //
    // "indeterminate" is true if the set of successor offsets
    // couldn't be determined without knowledge of program state.
    // (e.g. because instruction is an indirect jump or an interrupt
    // that might end the program).

    fn successors(&self, offset: usize) -> (Vec<usize>, Vec<usize>, bool);
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
    pub entry_offsets: Vec<usize>,
    pub highest_offset: usize,
    pub instructions: HashMap<usize, Meta<I>>,
    labels: HashSet<usize>,
    indeterminates: HashSet<usize>
}

impl<I: InstructionTrait> Listing<I> {
    pub fn new() -> Listing<I> {
        Listing {
            entry_offsets: Vec::new(),
            highest_offset: 0,
            instructions: HashMap::<usize, Meta<I>>::new(),
            labels: HashSet::new(),
            indeterminates: HashSet::new()
        }
    }

    pub fn with_entry(entry_offset: usize) -> Listing<I> {
        Listing {
            entry_offsets: vec!(entry_offset),
            highest_offset: entry_offset,
            instructions: HashMap::new(),
            labels: HashSet::new(),
            indeterminates: HashSet::new()
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
