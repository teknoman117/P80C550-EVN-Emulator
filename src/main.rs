use std::fs;
use std::path::Path;
use std::rc::Rc;

/* 8051 address spaces */
#[derive(Clone, Copy, Debug)]
enum Address {
    Code(u16),
    ExternalData(u16),
    InternalData(u8),
    SpecialFunctionRegister(u8),
    Bit(u8),
}

/* 8051 registers */
#[derive(Clone, Copy, Debug)]
enum Register8051 {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    A,
    C,
    PC,
    DPTR,
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
#[derive(Clone, Copy, Debug)]
enum AddressingMode {
    // Immediate (most immediates)
    Immediate(u8),
    // register
    Register(Register8051),
    // bit direct address
    Bit(u8),
    // internal ram direct address
    Direct(u8),
    // internal ram indirect address
    Indirect(Register8051),
    // indirect (DPTR or PC) + offset (A) indirect access (movx, movc)
    IndirectOffset(Register8051),
}

#[derive(Clone, Copy, Debug)]
enum ISA8051 {
    ACALL(u16),
    ADD(AddressingMode),
    ADDC(AddressingMode),
    AJMP(u16),
    ANL(AddressingMode, AddressingMode),
    CJNE(AddressingMode, AddressingMode, u8),
    CLR(AddressingMode),
    CPL(AddressingMode),
    DA,
    DEC(AddressingMode),
    DIV,
    DJNZ(AddressingMode, u8),
    INC(AddressingMode),
    JB(u8, u8),
    JBC(u8, u8),
    JC(u8),
    JMP,
    JNB(u8, u8),
    JNC(u8),
    JNZ(u8),
    JZ(u8),
    LCALL(u16),
    LJMP(u16),
    LoadDptr(u16),
    MOV(AddressingMode, AddressingMode),
    MOVC(AddressingMode),
    MOVX(AddressingMode, AddressingMode),
    MUL,
    NOP,
    ORL(AddressingMode, AddressingMode),
    POP(u8),
    PUSH(u8),
    RET,
    RETI,
    RL,
    RLC,
    RR,
    RRC,
    SETB(AddressingMode),
    SJMP(u8),
    SUBB(AddressingMode),
    SWAP,
    XCH(AddressingMode),
    XCHD(AddressingMode),
    XRL(AddressingMode, AddressingMode),
    Undefined,
}

trait Memory {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str>;
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str>;
}

struct CPU8051<A: Memory> {
    bank: u8,
    carry_flag: u8,
    accumulator: u8,
    data_pointer: u16,
    program_counter: u16,
    memory: Rc<A>,
}

impl<A: Memory> CPU8051<A> {
    fn new(memory: Rc<A>) -> CPU8051<A> {
        CPU8051 {
            bank: 0,
            carry_flag: 0,
            accumulator: 0,
            data_pointer: 0,
            program_counter: 0,
            memory: memory,
        }
    }

    // Immediate (most immediates)
    /*Immediate(u8),
    // register
    Register(Register8051),
    // bit direct address
    Bit(u8),
    // internal ram direct address
    Direct(u8),
    // internal ram indirect address
    Indirect(Register8051),
    // indirect (DPTR or PC) + offset (A) indirect access (movx, movc)
    IndirectOffset(Register8051),*/

    /*
        R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    A,
    C,
    PC,
    DPTR,
    */

    // perform a load using a particular addressing mode
    fn load(&mut self, mode: AddressingMode) -> Result<u8, &'static str> {
        let mut mem = Rc::get_mut(&mut self.memory).unwrap();
        match mode {
            AddressingMode::Immediate(imm8) => Ok(imm8),
            AddressingMode::Register(register) => {
                // 8051 registers occupy the first 32-bytes of memory
                let bank = self.bank << 3;
                match register {
                    Register8051::A => Ok(self.accumulator),
                    Register8051::C => Ok(self.carry_flag),
                    Register8051::R0 => mem.read_memory(Address::InternalData(bank + 0)),
                    Register8051::R1 => mem.read_memory(Address::InternalData(bank + 1)),
                    Register8051::R2 => mem.read_memory(Address::InternalData(bank + 2)),
                    Register8051::R3 => mem.read_memory(Address::InternalData(bank + 3)),
                    Register8051::R4 => mem.read_memory(Address::InternalData(bank + 4)),
                    Register8051::R5 => mem.read_memory(Address::InternalData(bank + 5)),
                    Register8051::R6 => mem.read_memory(Address::InternalData(bank + 6)),
                    Register8051::R7 => mem.read_memory(Address::InternalData(bank + 7)),
                    _ => Err("unsupported register"),
                }
            }
            AddressingMode::Bit(bit) => {
                // 8051 bit values occupy 0x20 to 0x2F
                if bit < 128 {
                    let octet = mem.read_memory(Address::InternalData(0x20 + (bit >> 3)))?;
                    if octet & (1 << (bit & 0x07)) != 0 {
                        Ok(1)
                    } else {
                        Ok(0)
                    }
                } else {
                    Err("invalid bit address")
                }
            }
            AddressingMode::Direct(address) => {
                // 128-byte iram of 8051 vs SFR (upper 128 on 8052 can only be used via indirect)
                if address < 128 {
                    mem.read_memory(Address::InternalData(address))
                } else {
                    mem.read_memory(Address::SpecialFunctionRegister(address))
                }
            }
            _ => Err("unsupported addressing mode (load)"),
        }
    }

    // perform a store using an addressing mode
    fn store(&mut self, mode: AddressingMode, data: u8) -> Result<(), &'static str> {
        let mut mem = Rc::get_mut(&mut self.memory).unwrap();
        match mode {
            AddressingMode::Register(register) => {
                // 8051 registers occupy the first 32-bytes of memory
                let bank = self.bank << 3;
                match register {
                    Register8051::A => {
                        self.accumulator = data;
                        Ok(())
                    }
                    Register8051::C => {
                        self.carry_flag = data;
                        Ok(())
                    }
                    Register8051::R0 => mem.write_memory(Address::InternalData(bank + 0), data),
                    Register8051::R1 => mem.write_memory(Address::InternalData(bank + 1), data),
                    Register8051::R2 => mem.write_memory(Address::InternalData(bank + 2), data),
                    Register8051::R3 => mem.write_memory(Address::InternalData(bank + 3), data),
                    Register8051::R4 => mem.write_memory(Address::InternalData(bank + 4), data),
                    Register8051::R5 => mem.write_memory(Address::InternalData(bank + 5), data),
                    Register8051::R6 => mem.write_memory(Address::InternalData(bank + 6), data),
                    Register8051::R7 => mem.write_memory(Address::InternalData(bank + 7), data),
                    _ => Err("unsupported register"),
                }
            }
            AddressingMode::Bit(bit) => {
                // 8051 bit values occupy 0x20 to 0x2F
                if bit < 128 {
                    let mut octet = mem.read_memory(Address::InternalData(0x20 + (bit >> 3)))?;
                    if data != 0 {
                        octet |= 1 << (bit & 0x07);
                    } else {
                        octet &= !(1 << (bit & 0x07));
                    }
                    mem.write_memory(Address::InternalData(0x20 + (bit >> 3)), octet)
                } else {
                    Err("invalid bit address")
                }
            }
            AddressingMode::Direct(address) => {
                // 128-byte iram of 8051 vs SFR (upper 128 on 8052 can only be used via indirect)
                if address < 128 {
                    mem.write_memory(Address::InternalData(address), data)
                } else {
                    mem.write_memory(Address::SpecialFunctionRegister(address), data)
                }
            }
            _ => Err("unsupported addressing mode (store)"),
        }
    }

    // decode the next instruction and return the next program counter
    fn decode_next_instruction(&mut self) -> Result<(ISA8051, u16), &'static str> {
        let opcode = Rc::get_mut(&mut self.memory)
            .unwrap()
            .read_memory(Address::Code(self.program_counter))?;

        // decode instruction
        match opcode {
            0x00 => Ok((ISA8051::NOP, 1)),
            0x01 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                let address = (((opcode & 0xE0) as u16) << 3) | (arg1 as u16);
                Ok((ISA8051::AJMP(address), 2))
            }
            0x02 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 2))?;
                let address = ((arg2 as u16) << 8) | (arg1 as u16);
                Ok((ISA8051::LJMP(address), 3))
            }
            0x52 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ANL(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::A),
                    ),
                    2,
                ))
            }
            0x90 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 2))?;
                let pointer = ((arg2 as u16) << 8) | (arg1 as u16);
                Ok((ISA8051::LoadDptr(pointer), 3))
            }
            _ => Err("unimplemented instruction (decode)"),
        }
    }

    // step
    fn step(&mut self) -> Result<(), &'static str> {
        let (instruction, length) = self.decode_next_instruction()?;
        let mut next_program_counter = self.program_counter + length;
        println!("{:04x}: {:?}", self.program_counter, instruction);

        let result = match instruction {
            ISA8051::NOP => Ok(()),
            ISA8051::AJMP(address) => {
                next_program_counter = (self.program_counter & 0xF800) | address;
                Ok(())
            }
            ISA8051::LJMP(address) => {
                next_program_counter = address;
                Ok(())
            }
            ISA8051::ANL(operand1, operand2) => {
                let data = self.load(operand1)? & self.load(operand2)?;
                self.store(operand1, data)
            }
            _ => Err("unimplemented instruction (execute)"),
        };
        self.program_counter = next_program_counter;
        result
    }
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
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        let address = match address {
            Address::Code(a) => Some(a as usize),
            Address::ExternalData(a) => Some(a as usize),
            _ => None,
        };

        if let Some(a) = address {
            if a < self.data.len() {
                Ok(self.data[a])
            } else {
                Err("address out of range")
            }
        } else {
            Err("unsupported addressing mode for ROM")
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
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        let address = match address {
            Address::Code(a) => Some(a as usize),
            Address::ExternalData(a) => Some(a as usize),
            Address::InternalData(a) => Some(a as usize),
            _ => None,
        };

        if let Some(a) = address {
            if a < self.data.len() {
                Ok(self.data[a as usize])
            } else {
                Err("address out of range")
            }
        } else {
            Err("unsupported addressing mode for RAM (read)")
        }
    }

    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        let address = match address {
            Address::ExternalData(a) => Some(a as usize),
            Address::InternalData(a) => Some(a as usize),
            _ => None,
        };

        if let Some(a) = address {
            if a < self.data.len() {
                self.data[a] = data;
                Ok(())
            } else {
                Err("address out of range")
            }
        } else {
            Err("unsupported addressing mode for RAM (write)")
        }
    }
}

struct MemoryMapperModule<A: Memory, B: Memory> {
    rom: Rc<A>,
    iram: Rc<B>,
    xram: Rc<B>,
}

impl<A: Memory, B: Memory> MemoryMapperModule<A, B> {
    fn new(rom: Rc<A>, iram: Rc<B>, xram: Rc<B>) -> MemoryMapperModule<A, B> {
        MemoryMapperModule { rom: rom, iram: iram, xram: xram }
    }
}

impl<A: Memory, B: Memory> Memory for MemoryMapperModule<A, B> {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::Code(a) => Rc::get_mut(&mut self.rom)
                .unwrap()
                .read_memory(Address::ExternalData(a)),
            Address::InternalData(a) => Rc::get_mut(&mut self.iram)
                .unwrap()
                .read_memory(Address::InternalData(a)),
            Address::ExternalData(a) => Rc::get_mut(&mut self.xram)
                .unwrap()
                .read_memory(Address::ExternalData(a)),
            _ => Err("unsupported addressing mode for memory mapper (read)"),
        }
    }
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::InternalData(a) => Rc::get_mut(&mut self.iram)
                .unwrap()
                .write_memory(Address::InternalData(a), data),
            Address::ExternalData(a) => Rc::get_mut(&mut self.xram)
                .unwrap()
                .write_memory(Address::ExternalData(a), data),
            _ => Err("unsupported addressing mode for memory mapper (write)"),
        }
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

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    // load the application rom
    let rom_path = Path::new("rom.bin");
    let rom = Rc::new(ROM::load_from_binary(rom_path)?);

    // create ram for the application
    let iram = Rc::new(RAM::create_with_size(128));
    let xram = Rc::new(RAM::create_with_size(8192));

    // create mapper
    let mut mapper = Rc::new(MemoryMapperModule::new(rom, iram, xram));

    // create the cpu
    let mut cpu = CPU8051::new(mapper);
    loop {
        cpu.step()?;
    }

    Ok(())
}
