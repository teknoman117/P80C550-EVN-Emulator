use std::fs;
use std::path::Path;
use std::rc::Rc;

enum AddressSpace {
    Code,
    Data,
}

trait Memory {
    fn read_memory(&mut self, space: AddressSpace, address: usize) -> u8;
    fn write_memory(&mut self, space: AddressSpace, address: usize, data: u8);
}

struct ROM {
    data: Vec<u8>,
}

impl ROM {
    fn load_from_binary(path: &Path) -> Result<ROM, Box<dyn std::error::Error + 'static>> {
        Ok(ROM {
            data: fs::read(path)?,
        })
    }
}

impl Memory for ROM {
    fn read_memory(&mut self, _space: AddressSpace, address: usize) -> u8 {
        if address < self.data.len() {
            self.data[address]
        } else {
            0xFFu8
        }
    }

    fn write_memory(&mut self, _space: AddressSpace, _address: usize, _data: u8) {
        // it's ROM, do nothing
    }
}

struct RAM {
    data: Vec<u8>,
}

impl RAM {
    fn create_with_size(size: usize) -> RAM {
        let mut data = Vec::with_capacity(size);
        data.resize(size, 0);
        RAM { data: data }
    }
}

impl Memory for RAM {
    fn read_memory(&mut self, _space: AddressSpace, address: usize) -> u8 {
        self.data[address]
    }
    fn write_memory(&mut self, _space: AddressSpace, address: usize, data: u8) {
        self.data[address] = data;
    }
}

struct MemoryMapperModule<A: Memory, B: Memory> {
    rom: Rc<A>,
    ram: Rc<B>,
}

impl<A: Memory, B: Memory> MemoryMapperModule<A, B> {
    fn new(rom: Rc<A>, ram: Rc<B>) -> MemoryMapperModule<A, B> {
        MemoryMapperModule { rom: rom, ram: ram }
    }
}

impl<A: Memory, B: Memory> Memory for MemoryMapperModule<A, B> {
    fn read_memory(&mut self, space: AddressSpace, address: usize) -> u8 {
        match space {
            AddressSpace::Code => Rc::get_mut(&mut self.rom)
                .unwrap()
                .read_memory(AddressSpace::Data, address),
            AddressSpace::Data => Rc::get_mut(&mut self.ram)
                .unwrap()
                .read_memory(AddressSpace::Data, address),
        }
    }
    fn write_memory(&mut self, _space: AddressSpace, _address: usize, _data: u8) {}
}

struct SelfProgramModule<A: Memory, B: Memory> {
    rom: Rc<A>,
    ram: Rc<B>,
}

impl<A: Memory, B: Memory> SelfProgramModule<A, B> {
    fn new(rom: Rc<A>, ram: Rc<B>) -> SelfProgramModule<A, B> {
        SelfProgramModule { rom: rom, ram: ram }
    }
}

impl<A: Memory, B: Memory> Memory for SelfProgramModule<A, B> {
    fn read_memory(&mut self, space: AddressSpace, address: usize) -> u8 {
        match space {
            AddressSpace::Code => Rc::get_mut(&mut self.rom)
                .unwrap()
                .read_memory(AddressSpace::Data, address),
            AddressSpace::Data => Rc::get_mut(&mut self.ram)
                .unwrap()
                .read_memory(AddressSpace::Data, address),
        }
    }
    fn write_memory(&mut self, _space: AddressSpace, _address: usize, _data: u8) {}
}

enum ISA8051 {
    ACALL,
    ADD,
    ADDC,
    AJMP,
    ANL,
    CJNE,
    CLR,
    CPL,
    DA,
    DEC,
    DIV,
    DJNZ,
    INC,
    JB,
    JBC,
    JC,
    JMP,
    JNB,
    JNC,
    JNZ,
    JZ,
    LCALL,
    LJMP,
    MOV,
    MOVC,
    MOVX,
    MUL,
    NOP,
    ORL,
    POP,
    PUSH,
    RET,
    RETI,
    RL,
    RLC,
    RR,
    RRC,
    SETB,
    SJMP,
    SUBB,
    SWAP,
    XCH,
    XCHD,
    XRL,
    Undefined
}

struct CPU8051 {}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    // load the application rom
    let rom_path = Path::new("rom.bin");
    let rom = Rc::new(ROM::load_from_binary(rom_path)?);

    // create a ram for the application
    let ram = Rc::new(RAM::create_with_size(8192));

    // create the memory mapper
    let mut mmu = MemoryMapperModule::new(rom, ram);

    println!("rom:0 = {:x}", mmu.read_memory(AddressSpace::Code, 0));
    println!("ram:0 = {:x}", mmu.read_memory(AddressSpace::Data, 0));

    Ok(())
}
