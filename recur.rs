use defs::*;

pub fn recursive_descent<I: InstructionTrait, A: Architecture<I>>(file_buffer: &Vec<u8>, architecture: A, entry_offset: usize) -> Listing<I> {
    let mut listing = Listing::new(entry_offset);

    let mut unexplored = Vec::new();
    unexplored.push(entry_offset);

    while let Some(offset) = unexplored.pop() {
        if let None = listing.get(offset) {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => panic!(err)
            };

            let (addresses, labels, indeterminate) = architecture.successors(inst, offset);

            for address in addresses {
                unexplored.push(address);
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
