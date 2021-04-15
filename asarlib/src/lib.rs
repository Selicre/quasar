use libc::{c_int, c_char, c_void};
use std::collections::HashMap;
use std::cell::{Cell, RefCell};

use std::ffi::CStr;

use quasar::{
    executor::{self, Target},
    assembler::Assembler,
    context::ContextStr,
    message::MsgQueue,
    rom::Rom,
    freespace
};

#[repr(C)]
pub struct ErrorData {
    full_err_data: *mut c_char,
    raw_err_data: *mut c_char,
    block: *mut c_char,
    filename: *mut c_char,
    line: c_int,
    caller_filename: *mut c_char,
    caller_line: c_int,
    err_id: c_int,
}

#[repr(C)]
pub struct LabelData {
    name: *mut c_char,
    location: c_int,
}

#[repr(C)]
pub struct DefineData {
    name: *mut c_char,
    contents: *mut c_char,
}

#[repr(C)]
pub struct WrittenBlockData {
    file_offset: c_int,
    snes_addr: c_int,
    num_bytes: c_int,
}

#[repr(C)]
pub enum MapperType {
    Invalid,
    LoRom,
    HiRom,
    Sa1Rom,
    BigSa1Rom,
    SfxRom,
    ExLoRom,
    ExHiRom,
    NoRom
}

#[repr(C)]
pub struct WarnSetting {
    warn_id: *mut c_char,
    enabled: bool,
}

#[repr(C)]
pub struct MemoryFile {
    path: *mut c_char,
    buffer: *mut c_void,
    length: usize
}

#[repr(C)]
pub struct PatchParams {
    struct_size: c_int,

    patch_loc: *const c_char,
    rom_data: *mut c_char,
    buf_len: c_int,
    rom_len: *mut c_int,

    include_paths: *const *const c_char,
    num_include_paths: c_int,

    should_reset: c_char,

    custom_defines: *const DefineData,
    custom_defines_len: c_int,

    std_includes: *const c_char,
    std_defines: *const c_char,

    warn_settings: *const WarnSetting,
    warn_settings_len: c_int,

    memory_files: *const MemoryFile,
    memory_files_len: c_int,

    override_checksum_gen: c_char,
    override_checksum: c_char,
}

#[derive(Default)]
struct PatchState {
    labels: HashMap<String, u32>,
    print_ptrs: Vec<*const c_char>,
    print_data: Vec<u8>,
}

thread_local! {
    static STATE: RefCell<PatchState> = RefCell::new(PatchState::default());
}

#[no_mangle]
pub unsafe extern "C" fn asar_version() -> c_int { 10900 }

#[no_mangle]
pub unsafe extern "C" fn asar_apiversion() -> c_int { 303 }

#[no_mangle]
pub unsafe extern "C" fn asar_init() -> c_char { 1 }

#[no_mangle]
pub unsafe extern "C" fn asar_reset() { }

#[no_mangle]
pub unsafe extern "C" fn asar_close() { }

#[no_mangle]
pub unsafe extern "C" fn asar_patch(
    patch_loc: *const c_char,
    rom_data: *mut c_char,
    buf_len: c_int,
    rom_len: *mut c_int
) -> c_char {
    println!("patch called with: ");
    let asm_file = CStr::from_ptr(patch_loc).to_string_lossy();
    println!("buf_len: {}", buf_len);
    println!("rom_len: {}", *rom_len);
    let mut rom = std::slice::from_raw_parts(rom_data as *const u8, *rom_len as usize).to_vec();
    let mut rom = Rom::new(rom);

    let mut target = Target::new(rom.clone());
    let mut asm = Assembler::new();

    executor::exec_file(&asm_file, ContextStr::cli(), &mut target, &mut asm);
    MsgQueue::drain(|i| {
        println!("{}", i);
    });
    if MsgQueue::has_error() {
        println!("Parsing failed");
        return 0;
    }
    target.profiler("executed");

    freespace::resolve_freespace(&mut rom, &mut asm);
    asm.resolve_labels();
    asm.write_to_rom(&mut target, &mut rom);
    MsgQueue::drain(|i| {
        println!("{}", i);
    });
    /*for (k,v) in target.labels().clone().iter().enumerate() {
        if let Some(expr) = asm.get_label_value(k).cloned() {
            let expr = expr.try_eval_float(&mut target, &mut asm).unwrap();
            println!("%{}: {:?} => {}", k, v, expr);
        } else {
            println!("%{}: {:?}", k, v);
        }
    }*/
    if MsgQueue::has_error() {
        println!("Assembly failed");
        return 0;
    }
    rom.fix_checksum();
    let rslice = rom.as_slice();
    if rslice.len() > buf_len as usize {
        println!("buffer too small");
        return 0;
    }
    rom_data.copy_from(rslice.as_ptr() as _, rslice.len());
    *rom_len = rslice.len() as _;

    STATE.with(|c| {
        let mut state = c.borrow_mut();
        *state = PatchState::default();

        for (k,v) in target.labels().clone().iter().enumerate() {
            if let quasar::expression::Label::Named { stack, invoke } = v {
                if stack.len() == 1 && invoke.is_none() {
                    if let Some(expr) = asm.get_label_value(k).cloned() {
                        let expr = expr.try_eval_int(&mut target, &mut asm).unwrap();
                        state.labels.insert(stack[0].clone(), expr);
                        println!("label {} = {}", &stack[0], expr);
                    }
                }
            }
        }
        let prints = target.prints();
        let mut lengths = vec![];
        for i in prints {
            lengths.push(state.print_data.len());
            state.print_data.extend_from_slice(i.as_bytes());
            state.print_data.push(0);
        }
        for i in lengths {
            let c = state.print_data.as_ptr().add(i) as _;
            state.print_ptrs.push(c);
        }
        println!("{:?}", state.print_ptrs);
        println!("{:?}", String::from_utf8_lossy(&state.print_data));
    });
    1
}

#[no_mangle]
pub unsafe extern "C" fn asar_patch_ex(params_base: *const c_int) -> c_char {
    println!("asar_patch_ex unimplemented!");
    0
}

#[no_mangle]
pub unsafe extern "C" fn asar_maxromsize() -> c_int { 16 * 1024 * 1024 }


#[no_mangle]
pub unsafe extern "C" fn asar_geterrors(count: *mut c_int) -> *const ErrorData {
    *count = 0; std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn asar_getwarnings(count: *mut c_int) -> *const ErrorData {
    *count = 0; std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn asar_getprints(count: *mut c_int) -> *const *const c_char {
    use std::ffi::CString;
    STATE.with(|c| {
        let c = c.borrow();
        *count = c.print_ptrs.len() as _;
        println!("got {} prints", *count);
        c.print_ptrs.as_ptr()
    })
}

#[no_mangle]
pub unsafe extern "C" fn asar_getalllabels(count: *mut c_int) -> *const LabelData {
    *count = 0; std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn asar_getlabelval(name: *const c_char) -> c_int {
    let name = CStr::from_ptr(name).to_string_lossy();
    STATE.with(|c| {
        let val = c.borrow().labels.get(name.as_ref()).map(|c| *c as c_int).unwrap_or(-1);
        println!("{}: {}", name, val);
        val
    })
}


#[no_mangle]
pub unsafe extern "C" fn asar_getalldefines(count: *mut c_int) -> *const DefineData {
    *count = 0; std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn asar_getdefine(name: *const c_char) -> *const DefineData {
    std::ptr::null()
}


#[no_mangle]
pub unsafe extern "C" fn asar_resolvedefines(data: *const c_char, learn_new: bool) -> *const c_char {
    std::ptr::null()
}


#[no_mangle]
pub unsafe extern "C" fn asar_math(math: *const c_char, errors: *const *const c_char) -> *const c_char {
    std::ptr::null()
}


#[no_mangle]
pub unsafe extern "C" fn asar_getwrittenblocks(count: *mut c_int) -> *const WrittenBlockData {
    *count = 0;
    std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn asar_getmapper() -> MapperType {
    MapperType::LoRom
}

#[no_mangle]
pub unsafe extern "C" fn asar_getsymbolsfile(format: *const c_char) -> *const c_char {
    std::ptr::null()
}

