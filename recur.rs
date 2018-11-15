use defs::main::*;

pub fn recursive_descent<I, A>(file_buffer: &Vec<u8>, architecture: A, entry_offset: usize) -> Listing<I>
    where I: InstructionTrait,
          A: Architecture<I>
{
    let mut listing = Listing::with_entry(entry_offset);

    let mut unexplored = Vec::new();
    unexplored.push(entry_offset);

    while let Some(offset) = unexplored.pop() {
        if let None = listing.get(offset) {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => {
                    println!("{}", err);
                    return listing;
                }
            };

            let (offsets, labels, indeterminate) = inst.successors(offset);

            for offset in offsets {
                if offset < file_buffer.len() {
                    unexplored.push(offset);
                }
            }

            for label in labels {
                listing.add_label(label);
            }

            if indeterminate {
                listing.add_indeterminate(offset);
            }

            if offset > listing.highest_offset {
                listing.highest_offset = offset
            }

            listing.add(offset, inst);
        }
    }

    listing
}
