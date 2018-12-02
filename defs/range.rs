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

pub trait RangeTrait<T: Ord, R: RangeTrait<T, R>> {
    fn from_value(value: T) -> R;
    fn from_range(min: T, max: T) -> R;
    fn insert_range(self, min: T, max: T) -> R;
    fn combine(self, range: R) -> R;
}

type RangeVec<T> = Vec<Range<T>>;

impl RangeTrait<u8, RangeVec<u8>> for RangeVec<u8> {
    fn from_value(value: u8) -> RangeVec<u8> {
        RangeVec::from_range(value, value)
    }

    fn from_range(min: u8, max: u8) -> RangeVec<u8> {
        vec!(Range::new(min, max))
    }

    fn insert_range(self, min: u8, max: u8) -> RangeVec<u8> {
        self.combine(RangeVec::from_range(min, max))
    }

    fn combine(self, range: RangeVec<u8>) -> RangeVec<u8> {
        let mut new_vec = RangeVec::new();
        let (mut si, mut ri) = (0, 0);
        let (mut min, mut max) = (0, 0);
        let mut new_range = true;

        while si < self.len() && ri < range.len() {
            let on_self = if ri >= range.len() {
                true
            } else if si >= self.len() {
                false
            } else {
                self[si].min < range[ri].min
            };

            let next_range = if on_self {
                si += 1; self[si].clone()
            } else {
                ri += 1; range[ri].clone()
            };
            
            if new_range == true {
                min = next_range.min;
                max = next_range.max;
                new_range = false;
            } else if max + 1 < next_range.min {
                new_vec.push(Range::new(min, max));
                new_range = true;
            } else {
                max = next_range.max;
            }
        }

        new_vec
    }
}

pub type Byte = RangeVec<u8>;
//pub type Word = RangeVec<u16>;
//pub type DWord = RangeVec<u32>;
//pub type QWord = RangeVec<u64>;
