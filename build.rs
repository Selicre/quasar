use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("codegen.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());
    println!("cargo:rerun-if-changed=build.rs");

    let mut map = phf_codegen::Map::new();
    populate(&mut map);
    writeln!(&mut file,
        "static OPCODES: phf::Map<[u8;4], u8> = \n{};\n",
        map.build()
    ).unwrap();
}

enum AddressingMode {
    Implied,
    Immediate,
    DirectPage,
    DpX,
    DpY,
    DpInd,
    DpIndX,
    DpIndY,
    DpIndLong,
    DpIndLongY,
    Stack,
    StackY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    AbsInd,
    AbsIndX,
    AbsIndLong,
    AbsLong,
    AbsLongX,
    Relative,
    RelativeWord,
    BlockMove
}
fn populate(map: &mut phf_codegen::Map<[u8;4]>) {
    /*
    use AddressingMode::*;
    macro_rules! entries {
        ($val:expr, $($addr:expr => $offset:expr),*) => {{
            let [a,b,c] = $val;
            $(
                map.entry([a,b,c,$addr as u8], &format!("{}", $offset));
            )*
        }}
    }
    let mut implied = entries! { |offset|
        Implied => offset
    };
    let mut relative = entries! { |offset|
        Relative => offset
    };
    let mut arith = entries! { |offset|
        DpIndX =>     0x01 + offset,
        Stack =>      0x03 + offset,
        DirectPage => 0x05 + offset,
        DpIndLong =>  0x07 + offset,
        Immediate =>  0x09 + offset,
        Absolute =>   0x0D + offset,
        AbsLong =>    0x0F + offset,
        DpIndY =>     0x11 + offset,
        DpInd =>      0x12 + offset,
        StackY =>     0x13 + offset,
        DpX =>        0x15 + offset,
        DpIndLongY => 0x17 + offset,
        AbsoluteY =>  0x19 + offset,
        AbsoluteX =>  0x1D + offset,
        AbsLongX =>   0x1F + offset
    };
    let mut common_implied = entries! { |implied, offset|
        Implied =>    implied,
        DirectPage => 0x06 + offset,
        Absolute =>   0x0E + offset,
        DpX =>        0x16 + offset,
        AbsoluteX =>  0x1E + offset
    };*/
}
