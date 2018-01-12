use graph::FlowGraph;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Add;

pub trait Architecture<S: StateTrait<S>, I: InstructionTrait> : Copy + Clone {
    fn decode_instruction(&self, buffer: &[u8], offset: usize) -> Result<I, String>;
    fn simulate_next_instruction<C: Context<S, I>>(&self, state: S, context: &C, instruction: I) -> SimResult<S>;
}

pub trait Context<S: StateTrait<S>, I: InstructionTrait> {
    fn simulate_system_call(&self, state: S, inst: I) -> Option<S>;
    fn entry_offset(&self, state: &S) -> usize;
    fn next_inst_offset(&self, state: &S) -> usize;
}

pub trait StateTrait<S: StateTrait<S>> : Clone + fmt::Display {
    fn union(self, state: S) -> S;
    fn is_subset(&self, state: &S) -> bool;
}

pub trait InstructionTrait : Copy + Clone + fmt::Display {
    fn is_return(&self) -> bool;
    fn is_rel_branch(&self) -> bool;
}

pub struct Analysis<State: StateTrait<State>, Instruction: InstructionTrait> {
    pub entry_offset: usize,
    pub highest_offset: usize,
    pub flow_graph: FlowGraph<State>,
    pub instructions: HashMap<usize, Instruction>
}

pub enum SimResult<S: StateTrait<S>> {
    End,
    State(S),
    Branch((Vec<S>, Vec<usize>))
}

pub fn get_word_le(buffer : &[u8], offset: usize) -> u16 {
    let word = buffer[offset + 1] as u16;
    (word << 8) + buffer[offset] as u16
}

pub fn get_word_be(buffer: &[u8], offset: usize) -> u16 {
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

#[derive(Clone)]
pub enum Value {
    Word(Word),
    Byte(Byte)
}

impl Value {
    pub fn is_subset(&self, value: &Value) -> bool {
        match self {
            &Value::Word(ref word1) => match value {
                &Value::Word(ref word2) => word1.is_subset(&word2),
                _ => panic!("can't compare words and bytes.")
            },
            &Value::Byte(ref byte1) => match value {
                &Value::Byte(ref byte2) => byte1.is_subset(&byte2),
                _ => panic!("can't compore bytes and words.")
            }
        }
    }

    pub fn union(self, value: Value) -> Value {
        match self {
            Value::Word(word1) => match value {
                Value::Word(word2) => Value::Word(word1.union(word2)),
                _ => panic!("can't combine words and bytes.")
            },
            Value::Byte(byte1) => match value {
                Value::Byte(byte2) => Value::Byte(byte1.union(byte2)),
                _ => panic!("can't combine bytes and words.")
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Byte(ref byte) => write!(f, "{}", byte),
            &Value::Word(ref word) => write!(f, "{}", word)
        }
    }
}

#[derive(Clone)]
pub enum Word {
    Undefined,
    AnyValue,
    Int(HashSet<u16>),
    Bytes(Byte, Byte),
}

impl Word {
    pub fn new(word: u16) -> Word {
        let mut word_set = HashSet::new();
        word_set.insert(word);
        Word::Int(word_set)
    }

    pub fn union(self, word: Word) -> Word {
        if let Word::Bytes(byte1, byte2) = word {
            Word::Bytes(byte1.union(self.split_low()),
                byte2.union(self.split_high()))
        } else if let Word::Bytes(byte1, byte2) = self { 
            Word::Bytes(byte1.union(word.split_low()),
                byte2.union(word.split_high()))
        } else {
            match self {
                Word::Undefined => Word::Undefined,
                Word::AnyValue => Word::AnyValue,
                Word::Int(set1) => match word {
                    Word::Undefined => Word::Undefined,
                    Word::AnyValue => Word::AnyValue,
                    Word::Int(set2) => Word::Int(set1.union(&set2).cloned().collect()),
                    _ => panic!("invalid word")
                    },
                _ => panic!("invalid word")
            }
        }
    }

    pub fn is_subset(&self, word: &Word) -> bool {
        if let Word::Bytes(ref byte1, ref byte2) = *word {
            self.split_low().is_subset(byte1) &&
            self.split_high().is_subset(byte2)
        } else if let Word::Bytes(ref byte1, ref byte2) = *self {
            byte1.is_subset(&word.split_low()) &&
            byte2.is_subset(&word.split_high())
        } else {
            match *self {
                Word::Undefined => match *word {
                    Word::Undefined | Word::AnyValue => true,
                    _ => false
                },
                Word::AnyValue => match *word {
                    Word::AnyValue => true,
                    _ => false
                },
                Word::Int(ref set1) => match *word {
                    Word::Undefined => false,
                    Word::AnyValue => true,
                    Word::Int(ref set2) => set1.is_subset(&set2),
                    _ => panic!("shouldn't be here")
                },
                _ => panic!("shouldn't be here")
            }
        }
    }

    pub fn split_low(&self) -> Byte {
        match *self {
            Word::Undefined => Byte::Undefined,
            Word::AnyValue => Byte::AnyValue,
            Word::Int(ref words) =>
                Byte::Int({
                    let mut set = HashSet::new();
                    for word in words.iter() {
                        set.insert(*word as u8);
                    };
                    set
                }),
            Word::Bytes(ref byte_low, _) => byte_low.clone()
        }
    }

    pub fn split_high(&self) -> Byte {
        match *self {
            Word::Undefined => Byte::Undefined,
            Word::AnyValue => Byte::AnyValue,
            Word::Int(ref words) =>
                Byte::Int({
                    let mut set = HashSet::new();
                    for word in words {
                        set.insert((*word >> 8) as u8);
                    };
                    set
                }),
            Word::Bytes(_, ref byte_high) => byte_high.clone()
        }
    }

    pub fn can_be(&self, word: u16) -> bool {
        match *self {
            Word::Undefined => panic!("testing undefined word"),
            Word::AnyValue => true,
            Word::Int(ref set) => set.contains(&word),
            Word::Bytes(ref bytel, ref byteh) =>
                bytel.can_be((word % 0x100) as u8)
                && byteh.can_be((word / 0x100) as u8)
        }
    }

    pub fn compare(&self, word: &Word) -> (bool, bool) {
        if let Word::Undefined = *self {
            panic!("testing undefined word");
        }
        if let Word::Undefined = *word {
            panic!("testing undefined word");
        }
        if let Word::Bytes(ref bytel, ref byteh) = *self {
            return bytel.clone().combine(byteh.clone()).compare(word);
        }
        if let Word::Bytes(ref bytel, ref byteh) = *word {
            return bytel.clone().combine(byteh.clone()).compare(self);
        }
        match *self {
            Word::AnyValue => (true, false),
            Word::Int(ref set1) => match *word {
                Word::AnyValue => (true, true),
                Word::Int(ref set2) => {
                    let intersect = set1.intersection(set2).collect::<Vec<&u16>>().len();
                    (intersect > 0, intersect > 1
                        && (set1.len() != 1 || set2.len() != 1)
                    )
                },
                _ => panic!("shouldn't be here")
            },
            _ => panic!("shouldn't be here")
        }
    }
        

    pub fn compare_u16(&self, word: u16) -> (bool, bool) {
        match *self {
            Word::Undefined => panic!("testing undefined word"),
            Word::AnyValue => (true, false),
            Word::Int(ref set) => (set.contains(&word),
                set.contains(&word) || set.len() > 1),
            Word::Bytes(ref bytel, ref byteh) =>
                bytel.clone().combine(byteh.clone()).compare_u16(word)
        }
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Word::Undefined => String::from("????"),
            Word::AnyValue => String::from("****"),
            Word::Int(ref set) =>
                if set.len() == 1 {
                    format!("{:04x}", set.iter().collect::<Vec<&u16>>()[0])
                } else {
                    String::from("{--}")
                },
            Word::Bytes(ref reg1, ref reg2) => format!("{}{}", reg2, reg1)
        })
    }
}

impl Add<Word> for Word {
    type Output = Word;
    fn add(self, rhs: Word) -> Word {
        let word1 = if let Word::Bytes(bytel, byteh) = self {
            bytel.combine(byteh)
        } else {
            self.clone()
        };
        let word2 = if let Word::Bytes(bytel, byteh) = rhs {
            bytel.combine(byteh)
        } else {
            rhs.clone()
        };
        match word1 {
            Word::Undefined => Word::Undefined,
            Word::AnyValue => match word2 {
                    Word::Undefined => Word::Undefined,
                    _ => Word::AnyValue
            },
            Word::Int(set1) => match word2 {
                Word::Undefined => Word::Undefined,
                Word::AnyValue => Word::AnyValue,
                Word::Int(set2) => {
                    let mut set = HashSet::new();
                    for word1 in set1 {
                        for word2 in set2.clone() {
                            set.insert(word1 + word2);
                        }
                    };
                    Word::Int(set)
                },
                _ => panic!("shouldn't be here")
            },
            _ => panic!("shouldn't be here")
        }
    }
}

#[derive(Clone)]
pub enum Byte {
    Undefined,
    AnyValue,
    Int(HashSet<u8>),
}

impl Byte {
    pub fn new(byte: u8) -> Byte {
        let mut byte_set = HashSet::new();
        byte_set.insert(byte);
        Byte::Int(byte_set)
    }

    pub fn to_word(self) -> Word {
        match self {
            Byte::Undefined => Word::Undefined,
            Byte::AnyValue => Word::AnyValue,
            Byte::Int(set) => {
                let mut words = HashSet::new();
                for byte in set {
                    words.insert(byte as u16);
                }
                Word::Int(words)
            }
        }
    }

    pub fn combine(self, byte: Byte) -> Word {
        match self {
            Byte::Undefined => Word::Undefined,
            Byte::AnyValue => match byte {
                Byte::Undefined => Word::Undefined,
                Byte::AnyValue => Word::AnyValue,
                Byte::Int(seth) => {
                    let mut words = HashSet::new();
                    for bytel in 0..256 {
                        for byteh in seth.iter() {
                            words.insert(bytel + ((*byteh as u16) << 8));
                        }
                    }
                    Word::Int(words)
                }
            },
            Byte::Int(setl) => match byte {
                Byte::Undefined => Word::Undefined,
                Byte::AnyValue => Word::AnyValue,
                Byte::Int(seth) => {
                    let mut words = HashSet::new();
                    for bytel in setl {
                        for byteh in seth.iter() {
                            words.insert(bytel as u16 + ((*byteh as u16) << 8));
                        }
                    }
                    Word::Int(words)
                }
            }
        }
    }

    pub fn union(self, byte: Byte) -> Byte {
        match self {
            Byte::Undefined => Byte::Undefined,
            Byte::AnyValue => Byte::AnyValue,
            Byte::Int(set1) => match byte {
                Byte::Undefined => Byte::Undefined,
                Byte::AnyValue => Byte::AnyValue,
                Byte::Int(set2) => Byte::Int(set1.union(&set2).cloned().collect())
            }
        }
    }

    pub fn is_subset(&self, byte: &Byte) -> bool {
        match *self {
            Byte::Undefined => match *byte {
                Byte::Undefined | Byte::AnyValue => true,
                _ => false
            },
            Byte::AnyValue => match *byte {
                Byte::AnyValue => true,
                _ => false
            },
            Byte::Int(ref set1) => match *byte {
                Byte::Undefined => false,
                Byte::AnyValue => true,
                Byte::Int(ref set2) => set1.is_subset(&set2)
            }
        }
    }

    pub fn can_be(&self, byte: u8) -> bool {
        match *self {
            Byte::Undefined => panic!("testing undefined word"),
            Byte::AnyValue => true,
            Byte::Int(ref set) => set.contains(&byte),
        }
    }

    pub fn compare_u8(&self, byte: u8) -> (bool, bool) {
        match *self {
            Byte::Undefined => panic!("testing undefined word"),
            Byte::AnyValue => (true, false),
            Byte::Int(ref set) => (set.contains(&byte),
                set.contains(&byte) || set.len() > 1),
        }
    }
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Byte::Undefined => String::from("??"),
            Byte::AnyValue => String::from("**"),
            Byte::Int(ref set) =>
                if set.len() == 1 {
                    format!("{:02x}", set.iter().collect::<Vec<&u8>>()[0])
                } else {
                    String::from("{}")
                },
        })
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Bit {
    Undefined,
    True,
    False,
    TrueAndFalse
}

impl Bit {
    pub fn new() -> Bit {
        Bit::Undefined
    }

    pub fn union(self, bit: Bit) -> Bit {
        match self {
            Bit::Undefined => bit,
            Bit::True => match bit {
                Bit::Undefined => Bit::True,
                Bit::False => Bit::TrueAndFalse,
                _ => bit
            },
            Bit::False => match bit {
                Bit::Undefined => Bit::False,
                Bit::True => Bit::TrueAndFalse,
                _ => bit
            },
            Bit::TrueAndFalse => Bit::TrueAndFalse
        }
    }

    pub fn is_subset(&self, bit: Bit) -> bool {
        match *self {
            Bit::Undefined => match bit {
                Bit::Undefined | Bit::TrueAndFalse => true,
                _ => false
            },
            Bit::TrueAndFalse => bit == Bit::TrueAndFalse,
            _ => *self == bit
        }
    }

    pub fn has_truth_value(&self, truth_value: bool) -> bool {
        match truth_value {
            true => *self == Bit::True || *self == Bit::TrueAndFalse,
            false => *self == Bit::False || *self == Bit::TrueAndFalse
        }
    }

    pub fn add_true(&mut self) {
        *self = match *self {
            Bit::Undefined | Bit::True =>
                Bit::True,
            Bit::False | Bit::TrueAndFalse
                => Bit::TrueAndFalse
        }
    }

    pub fn add_false(&mut self) {
        *self = match *self {
            Bit::Undefined | Bit::False =>
                Bit::False,
            Bit::True | Bit::TrueAndFalse
                => Bit::TrueAndFalse
        }
    }

    pub fn set(self, predicate: bool) -> Bit {
        match predicate {
            true => Bit::True,
            false => Bit::False
        }
    }
}

impl fmt::Display for Bit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Bit::Undefined => "?",
            Bit::True => "T",
            Bit::False => "F",
            Bit::TrueAndFalse => "*"
        })
    }
}
