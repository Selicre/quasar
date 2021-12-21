use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("codegen.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());
    println!("cargo:rerun-if-changed=build.rs");

    let mut map = HashMap::new();
    let mut mnemonics = HashSet::new();
    populate(&mut map, &mut mnemonics);

    mnemonics.remove(b"rep");    // grumble

    writeln!(&mut file, "pub fn lower_to_16(op: u8) -> Option<u8> {{ match op {{").unwrap();
    for (k,v) in map.iter() {
        if let Some(c) = lower_to_16(k[3]) {
            let mut r = *k;
            r[3] = c;
            if let Some(out) = map.get(&r) {
                writeln!(&mut file,
                    "    {} => Some({}),", v, out).unwrap();
            }
        }
    }
    writeln!(&mut file, "    _ => None\n}} }}").unwrap();

    let mut map2 = phf_codegen::Map::new();
    for (k,v) in map {
        map2.entry(k,&format!("{}", v));
    }

    let mut mnemonics2 = phf_codegen::Set::new();
    for v in mnemonics {
        mnemonics2.entry(v);
    }
    writeln!(&mut file,
        "pub static OPCODES: phf::Map<[u8;4], u8> = \n{};\n",
        map2.build()
    ).unwrap();
    writeln!(&mut file,
        "pub static MNEMONICS: phf::Set<[u8;3]> = \n{};\n",
        mnemonics2.build()
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

fn lower_to_16(mode: u8) -> Option<u8> {
    // this fn is technically unsafe, but who cares, this is a build script
    // that said don't pass anything invalid here or you will blow up
    let c = unsafe { std::mem::transmute(mode) };
    match c {
        AddressingMode::AbsLong => Some(AddressingMode::Absolute as u8),
        AddressingMode::AbsLongX => Some(AddressingMode::AbsoluteX as u8),
        _ => None
    }
}

fn populate(map: &mut HashMap<[u8;4], u8>, mnemonics: &mut HashSet<[u8;3]>) {
    use AddressingMode::*;

    macro_rules! kinds {
        ($val:expr, $($addr:expr => $offset:expr),*) => {{
            let [a,b,c] = *$val;
            $(
                map.insert([a,b,c,$addr as u8], $offset);
                mnemonics.insert([a,b,c]);
            )*
        }}
    }
    macro_rules! list {
        ($str:ident, $var:pat in { $($e:expr => $val:expr),*  $(,)?} $block:expr) => {
            let data = vec![$(($e, $val)),*];
            for ($str,$var) in data {
                $block
            }
        };
    }
    macro_rules! list_implied {
        ($($val:expr => $offset:expr),*) => {
            $({
                let [a,b,c] = *$val;
                map.insert([a,b,c,Implied as u8], $offset);
            })*
        }
    }
    macro_rules! list_relative {
        ($($val:expr => $offset:expr),*) => {
            $({
                let [a,b,c] = *$val;
                map.insert([a,b,c,Relative as u8], $offset);
            })*
        }
    }
    list_implied! {
        b"nop" => 0xEA,
        b"dec" => 0x3A,
        b"dex" => 0xCA,
        b"dey" => 0x88,
        b"inc" => 0x1A,
        b"inx" => 0xE8,
        b"iny" => 0xC8,
        b"asl" => 0x0A,
        b"rol" => 0x2A,
        b"lsr" => 0x4A,
        b"ror" => 0x6A,
        b"rtl" => 0x6B,
        b"rts" => 0x60,
        b"rti" => 0x40,
        b"stp" => 0xDB,
        b"wai" => 0xCB,
        b"clc" => 0x18,
        b"cld" => 0xD8,
        b"cli" => 0x58,
        b"clv" => 0xB8,
        b"sec" => 0x38,
        b"sed" => 0xF8,
        b"sei" => 0x78,
        b"pha" => 0x48,
        b"phx" => 0xDA,
        b"phy" => 0x5A,
        b"pla" => 0x68,
        b"plx" => 0xFA,
        b"ply" => 0x7A,
        b"phb" => 0x8B,
        b"phd" => 0x0B,
        b"phk" => 0x4B,
        b"php" => 0x08,
        b"plb" => 0xAB,
        b"pld" => 0x2B,
        b"plp" => 0x28,
        b"tax" => 0xAA,
        b"tay" => 0xA8,
        b"tsx" => 0xBA,
        b"txa" => 0x8A,
        b"txs" => 0x9A,
        b"txy" => 0x9B,
        b"tya" => 0x98,
        b"tyx" => 0xBB,
        b"tcd" => 0x5B,
        b"tcs" => 0x1B,
        b"tdc" => 0x7B,
        b"tsc" => 0x3B,
        b"xba" => 0xEB,
        b"xce" => 0xFB
    }
    list_relative! {
        b"bpl" => 0x10,
        b"bmi" => 0x30,
        b"bvc" => 0x50,
        b"bvs" => 0x70,
        b"bra" => 0x80,
        b"bcc" => 0x90,
        b"bne" => 0xD0,
        b"bcs" => 0xB0,
        b"beq" => 0xF0
    }
    kinds! { b"brl",
        RelativeWord => 0x82
    }
    kinds! { b"ldy",
        Immediate =>  0xA0,
        DirectPage => 0xA4,
        Absolute =>   0xAC,
        DpX =>        0xB4,
        AbsoluteX =>  0xBC
    }
    kinds! { b"ldx",
        Immediate =>  0xA2,
        DirectPage => 0xA6,
        Absolute =>   0xAE,
        DpY =>        0xB6,
        AbsoluteY =>  0xBE
    }
    kinds! { b"stx",
        DirectPage => 0x86,
        Absolute =>   0x8E,
        DpY =>        0x96
    }
    kinds! { b"sty",
        DirectPage => 0x84,
        Absolute =>   0x8C,
        DpX =>        0x94
    }
    list! {
        s, i in {
            b"cpy" => 0x00,
            b"cpx" => 0x20
        }
        kinds! { s,
            Immediate =>  0xC0+i,
            DirectPage => 0xC4+i,
            Absolute =>   0xCC+i
        }
    }
    list! {
        s, i in {
            b"ora" => 0x00,
            b"and" => 0x20,
            b"eor" => 0x40,
            b"adc" => 0x60,
            b"sta" => 0x80,
            b"lda" => 0xA0,
            b"cmp" => 0xC0,
            b"sbc" => 0xE0
        }
        kinds! { s,
            DpIndX =>     0x01+i,
            Stack =>      0x03+i,
            DirectPage => 0x05+i,
            DpIndLong =>  0x07+i,
            Immediate =>  0x09+i,
            Absolute =>   0x0D+i,
            AbsLong =>    0x0F+i,
            DpIndY =>     0x11+i,
            DpInd =>      0x12+i,
            StackY =>     0x13+i,
            DpX =>        0x15+i,
            DpIndLongY => 0x17+i,
            AbsoluteY =>  0x19+i,
            AbsoluteX =>  0x1D+i,
            AbsLongX =>   0x1F+i
        }
    }
    map.remove(b"sta\x01");
    kinds! { b"stz",
        DirectPage => 0x64,
        DpX => 0x74,
        Absolute => 0x9C,
        AbsoluteX => 0x9E
    }
    kinds! { b"bit",
        DirectPage => 0x24,
        Absolute => 0x2C,
        DpX => 0x34,
        AbsoluteX => 0x3C,
        Immediate => 0x89
    }
    list! {
        s, i in {
            b"asl" => 0x00,
            b"rol" => 0x20,
            b"lsr" => 0x40,
            b"ror" => 0x60,
            b"dec" => 0xC0,
            b"inc" => 0xE0,
        }
        kinds! { s,
            DirectPage => 0x06+i,
            Absolute =>   0x0E+i,
            DpX =>        0x16+i,
            AbsoluteX =>  0x1E+i
        }
    }
    list! {
        s, i in {
            b"trb" => 0x00,
            b"tsb" => 0x10
        }
        kinds! { s,
            DirectPage => 0x04+i,
            Absolute =>   0x0C+i
        }
    }
    list! {
        s, i in {
            b"tsb" => 0x00,
            b"trb" => 0x10
        }
        kinds! { s,
            DirectPage => 0x04+i,
            Absolute =>   0x0C+i
        }
    }
    kinds! { b"jmp",
        Absolute => 0x4C,
        //AbsLong => 0x5C,
        AbsInd => 0x6C,
        AbsIndX => 0x7C
        //AbsIndLong => 0xDC
    }
    kinds! { b"jml",
        AbsLong =>      0x5C,
        AbsIndLong =>   0xDC
    }
    kinds! { b"jsl",
        AbsLong =>      0x22
    }
    kinds! { b"jsr",
        Absolute =>     0x20,
        AbsIndX =>      0xFC
    }

    kinds! { b"mvn", BlockMove => 0x54 }
    kinds! { b"mvp", BlockMove => 0x44 }
    kinds! { b"rep", Immediate => 0xC2 }
    kinds! { b"sep", Immediate => 0xE2 }
    kinds! { b"brk", Immediate => 0x00 }
    kinds! { b"cop", Immediate => 0x02 }
    kinds! { b"wdm", Immediate => 0x42 }

    kinds! { b"pea",
        Absolute => 0xF4
    }
    kinds! { b"pei",
        DpInd => 0xD4
    }
    kinds! { b"per",
        Absolute => 0x62
    }

    let mut seen = HashMap::new();
    for (k,v) in map.iter() {
        if let Some(k2) = seen.get(&v) {
            panic!("duplicate instr: {:?}, {:?}", k, k2);
        }
        seen.insert(v,k);
    }
    for i in 0..=255 {
        if !seen.contains_key(&i) {
            panic!("no opcode ${:02X}", i);
        }
    }
}
