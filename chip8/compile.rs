pub mod analyse;
pub mod defs;
pub mod graph;
pub mod chip8;
mod c8analyzer;
mod c8compiler;

fn main() {
    use std::env;
    use std::io::Read;
    use std::io::Write;
    use std::fs::File;
    use std::path::Path;

    let mut args = env::args();

    let input_file = match args.nth(1) {
        None => {
            println!("usage: recompile <file-to-recompile>");
            return;
        },
        Some(file) => file
    };

    let old_shift_behavior = match args.next() {
        Some(arg) => arg == "--old-shift",
        _ => false
    };
    
    let analyzer = c8analyzer::Analyzer {
        old_shift_behavior
    };

    let compiler = c8compiler::Compiler {
        old_shift_behavior
    };

    let mut file = File::open(input_file.clone()).expect(
        "Failed to open file.");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect(
        "Failed to read into buffer.");

    let path = Path::new(&input_file);
    let stem = path.file_stem()
        .expect("argument has no stem").to_str().unwrap();

    let result = analyse::analyse(
        &buffer, chip8::arch::Chip8 {},
        &analyzer, 0);

    match result {
        Ok(graph) => {
            let mut file = File::create("code.c")
                .expect("Couldn't create output file.");

            let source = match compiler.source_string(graph, stem, buffer) {
                Err(err) => panic!(err),
                Ok(source) => source
            };
            
            file.write_all(source.as_bytes())
                .expect("Couldn't write output file.");

            let mut makefile = File::create("makefile")
                .expect("Couldn't create makefile.");

            makefile.write_all(c8compiler::Compiler::makefile().as_bytes())
                .expect("Couldn't write makefile.");
        },
        Err(error) => println!("{}", error)
    }
}

