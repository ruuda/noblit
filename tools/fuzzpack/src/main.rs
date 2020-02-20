extern crate walkdir;
extern crate fst;

use std::env;
use std::fs;
use std::io;

fn load_corpus(corpus_path: &str) -> Vec<Vec<u8>> {
    let mut fnames = Vec::new();
    for opt_entry in walkdir::WalkDir::new(corpus_path) {
        let entry = opt_entry.expect("Failed to walk dir");
        if entry.file_type().is_file() {
            fnames.push(entry.into_path());
        }
    }

    let mut corpus = Vec::new();
    for (i, fname) in fnames.iter().enumerate() {
        if i % 32 == 0 {
            eprint!("\r[{} / {}] {:?}", i + 1, fnames.len(), fname);
        }
        let entry = fs::read(fname).expect("Failed to read entry.");
        corpus.push(entry);
    }
    eprintln!();

    corpus 
}

fn main() {
    let corpus_path = env::args().nth(1).expect("Expected corpus path arg.");
    let mut corpus = load_corpus(&corpus_path);
    println!("{} files in corpus.", corpus.len());
    corpus.sort();

    let stdout = io::stdout();
    let mut builder = fst::SetBuilder::new(stdout.lock()).expect("Failed to open writer.");
    for entry in &corpus {
        builder.insert(entry).expect("Failed to insert entry.");
    }
    builder.finish().expect("Failed to finalize builder.");
}
