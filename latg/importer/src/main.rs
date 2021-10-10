#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]
#![feature(assert_matches)]

extern crate serde;

pub mod file_schema;
mod load_content;

fn main() {
    println!("Hello, world!");
}
