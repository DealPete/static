use std::fmt::LowerHex;

pub trait RangeTrait<T: Inc<T> + Ord + Copy + LowerHex, R: RangeTrait<T, R>> {
    fn new() -> R;

    fn from_value(value: T) -> R {
        R::from_range(value, value)
    }

    fn from_range(min: T, max: T) -> R;
    fn insert(self, value: T) -> R;
    fn insert_range(self, min: T, max: T) -> R;
    fn contains(&self, value: T) -> bool;
    fn display(&self) -> String;
    fn combine(self, range: R) -> R;
}

pub type RangeVec<T> = Vec<Range<T>>;

pub type Byte = RangeVec<u8>;
/*pub type Word = RangeVec<u16>;
pub type DWord = RangeVec<u32>;
pub type QWord = RangeVec<u64>;*/
pub type USize = RangeVec<usize>;

impl<T: Inc<T> + Ord + Copy + LowerHex> RangeTrait<T, RangeVec<T>> for RangeVec<T> {
    fn new() -> RangeVec<T> {
        Vec::new()
    }

    fn from_range(min: T, max: T) -> RangeVec<T> {
        vec!(Range { min, max })
    }

    fn insert(self, value: T) -> RangeVec<T> {
        self.insert_range(value, value)
    }

    fn insert_range(self, min: T, max: T) -> RangeVec<T> {
        self.combine(RangeVec::from_range(min, max))
    }

    fn contains(&self, value: T) -> bool {
        for range in self.iter() {
            if value < range.min {
                return false;
            }

            if value < range.max {
                return true;
            }
        }

        false
    }

    fn display(&self) -> String {
        if self.len() == 0 {
            return "[]".into();
        }

        let mut output = String::from("");

        for index in 0..self.len() {
            let range = &self[index];

            output.push_str(
                if range.min == range.max {
                    format!("{:x}", range.min)
                } else {
                    format!("{:x}...{:x}", range.min, range.max)
                }.as_str()
            );

            if index < self.len()-1 {
                output.push_str(", ");
            }
        }

        output
    }

    fn combine(self, range: RangeVec<T>) -> RangeVec<T> {
        if self.len() == 0 {
            return range;
        }

        if range.len() == 0 {
            return self;
        }

        let mut new_vec = RangeVec::new();

        let (mut si, mut ri, mut min, mut max) =
            if self[0].min < range[0].min {
                (1, 0, self[0].min, self[0].max)
            } else {
                (0, 1, range[0].min, range[0].max)
            };

        while si < self.len() || ri < range.len() {
            let on_self = if ri >= range.len() {
                true
            } else if si >= self.len() {
                false
            } else {
                self[si].min < range[ri].min
            };

            let next_range;
            
            if on_self {
                next_range = self[si].clone();
                si += 1;
            } else {
                next_range = range[ri].clone();
                ri += 1;
            }
            
            if max.inc() < next_range.min {
                new_vec.push(Range::new(min, max));
                min = next_range.min;
                max = next_range.max;
            } else if next_range.max > max {
                max = next_range.max;
            }
        }

        new_vec.push(Range::new(min, max));

        new_vec
    }
}

#[derive(Clone)]
pub struct Range<T> {
    min: T,
    max: T
}

impl<T> Range<T> {
    fn new(min: T, max: T) -> Range<T> {
        Range {
            min: min,
            max: max
        }
    }
}

pub trait Inc<T: Inc<T>> {
    fn inc(self) -> T;
}

impl Inc<u8> for u8 {
    fn inc(self) -> u8 {
        self.saturating_add(1)
    }
}

impl Inc<usize> for usize {
    fn inc(self) -> usize {
        self.saturating_add(1)
    }
}
