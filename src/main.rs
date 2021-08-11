use std::fs;
use std::path::Path;
use std::rc::Rc;

/* 8051 address spaces */
enum Address {
    Code(u32),
    ExternalData(u32),
    InternalData(u8),
    SpecialFunctionRegister(u8),
    Bit(u8),
}

/* 8051 registers */
enum Register8051 {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    DPTR,
    A,
    CY,
    PC,
}

/* 8051 flags */
enum Flags8051 {
    CY,
    AC,
    F0,
    RS1,
    RS0,
    OV,
    User,
    P,
}

/* 8051 addressing modes */
enum AddressingMode {
    // 8-bit immediate (most immediates)
    Immediate8(u8),
    // 16-bit immediate (dptr load)
    Immediate16(u16),
    // register
    Register(Register8051),
    // internal ram direct address
    Direct(u8),
    // internal ram indirect address
    Indirect(Register8051),
    // indirect (DPTR or PC) + offset (A) indirect access (movx, movc)
    IndirectOffset(Register8051),
}

trait Memory {
    fn read_memory(&mut self, address : Address) -> Result<u8, &'static str>;
    fn write_memory(&mut self, address : Address, data: u8) -> Result<(), &'static str>;
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
    fn read_memory(&mut self, address : Address) -> Result<u8, &'static str> {
        let address = match address {
            Address::Code(a) => Some(a as usize),
            Address::ExternalData(a) => Some(a as usize),
            _ => None
        };

        if let Some(a) = address {
            if a < self.data.len() {
                Ok(self.data[a])
            } else {
                Err("address out of range")
            }
        } else {
            Err("unsupported addressing mode")
        }
    }

    // all writes to ROM result in an error
    fn write_memory(&mut self, _address: Address, _data: u8) -> Result<(), &'static str> {
        Err("write attempted to read-only memory")
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
    fn read_memory(&mut self, address : Address) -> Result<u8, &'static str> {
        let address = match address {
            Address::Code(a) => Some(a as usize),
            Address::ExternalData(a) => Some(a as usize),
            Address::InternalData(a) => Some(a as usize),
            _ => None
        };

        if let Some(a) = address {
            if a < self.data.len() {
                Ok(self.data[a as usize])
            } else {
                Err("address out of range")
            }
        } else {
            Err("unsupported addressing mode")
        }
    }

    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        let address = match address {
            Address::ExternalData(a) => Some(a as usize),
            Address::InternalData(a) => Some(a as usize),
            _ => None
        };

        if let Some(a) = address {
            if a < self.data.len() {
                self.data[a] = data;
                Ok(())
            } else {
                Err("address out of range")
            }
        } else {
            Err("unsupported addressing mode")
        }
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
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::Code(a) => Rc::get_mut(&mut self.rom)
                .unwrap()
                .read_memory(Address::ExternalData(a)),
            Address::ExternalData(a) => Rc::get_mut(&mut self.ram)
                .unwrap()
                .read_memory(Address::ExternalData(a)),
            _ => Err("unsupported addressing mode")
        }
    }
    fn write_memory(&mut self, _address: Address, _data: u8) -> Result<(), &'static str> {
        Err("unimplemented")
    }
}

/*struct SelfProgramModule<A: Memory, B: Memory> {
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
}*/

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
    let mut rom = Rc::new(ROM::load_from_binary(rom_path)?);

    // create a ram for the application
    let ram = Rc::new(RAM::create_with_size(8192));

    // create the memory mapper
    let mut mmu = MemoryMapperModule::new(rom, ram);

    println!("rom:0 = {:x}", mmu.read_memory(Address::Code(0u32))?);
    println!("ram:0 = {:x}", mmu.read_memory(Address::ExternalData(0u32))?);
    //println!("rom:0 = {:x}", Rc::get_mut(&mut rom).unwrap().read_memory(Address::Code(0))?);

    Ok(())
}
