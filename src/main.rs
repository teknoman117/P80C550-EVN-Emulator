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
    CJNE(AddressingMode, AddressingMode, i8),
    CLR(AddressingMode),
    CPL(AddressingMode),
    DA,
    DEC(AddressingMode),
    DIV,
    DJNZ(AddressingMode, i8),
    INC(AddressingMode),
    JB(u8, i8),
    JBC(u8, i8),
    JC(i8),
    JMP,
    JNB(u8, i8),
    JNC(i8),
    JNZ(i8),
    JZ(i8),
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
    auxillary_carry_flag: u8,
    overflow_flag: u8,
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
            auxillary_carry_flag: 0,
            overflow_flag: 0,
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
            AddressingMode::Indirect(register) => {
                // R0 or R1 indirect load
                let bank = self.bank << 3;
                match register {
                    Register8051::R0 => {
                        let address = mem.read_memory(Address::InternalData(bank + 0))?;
                        mem.read_memory(Address::InternalData(address))
                    }
                    Register8051::R1 => {
                        let address = mem.read_memory(Address::InternalData(bank + 1))?;
                        mem.read_memory(Address::InternalData(address))
                    }
                    _ => Err("unsupported register for indirect load"),
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
            AddressingMode::Indirect(register) => {
                // R0 or R1 indirect store
                let bank = self.bank << 3;
                match register {
                    Register8051::R0 => {
                        let address = mem.read_memory(Address::InternalData(bank + 0))?;
                        mem.write_memory(Address::InternalData(address), data)
                    }
                    Register8051::R1 => {
                        let address = mem.read_memory(Address::InternalData(bank + 1))?;
                        mem.write_memory(Address::InternalData(address), data)
                    }
                    _ => Err("unsupported register for indirect store"),
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
            // ANL iram addr, A
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
            // ADD A, #data
            0x24 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ADD(AddressingMode::Immediate(arg1)),
                    2,
                ))
            }
            // ADD A, iram addr
            0x25 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ADD(AddressingMode::Direct(arg1)),
                    2,
                ))
            }
            // ADD A, @R0
            0x26 => Ok((ISA8051::ADD(AddressingMode::Indirect(Register8051::R0)), 1)),
            // ADD A, @R1
            0x27 => Ok((ISA8051::ADD(AddressingMode::Indirect(Register8051::R1)), 1)),
            // ADD A, R0
            0x28 => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R0)), 1)),
            // ADD A, R1
            0x29 => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R1)), 1)),
            // ADD A, R2
            0x2A => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R2)), 1)),
            // ADD A, R3
            0x2B => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R3)), 1)),
            // ADD A, R4
            0x2C => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R4)), 1)),
            // ADD A, R5
            0x2D => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R5)), 1)),
            // ADD A, R6
            0x2E => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R6)), 1)),
            // ADD A, R7
            0x2F => Ok((ISA8051::ADD(AddressingMode::Register(Register8051::R7)), 1)),
            // ADDC A, #data
            0x34 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ADDC(AddressingMode::Immediate(arg1)),
                    2,
                ))
            }
            // ADDC A, iram addr
            0x35 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ADDC(AddressingMode::Direct(arg1)),
                    2,
                ))
            }
            // ADDC A, @R0
            0x36 => Ok((ISA8051::ADDC(AddressingMode::Indirect(Register8051::R0)), 1)),
            // ADDC A, @R1
            0x37 => Ok((ISA8051::ADDC(AddressingMode::Indirect(Register8051::R1)), 1)),
            // ADDC A, R0
            0x38 => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R0)), 1)),
            // ADDC A, R1
            0x39 => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R1)), 1)),
            // ADDC A, R2
            0x3A => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R2)), 1)),
            // ADDC A, R3
            0x3B => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R3)), 1)),
            // ADDC A, R4
            0x3C => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R4)), 1)),
            // ADDC A, R5
            0x3D => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R5)), 1)),
            // ADDC A, R6
            0x3E => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R6)), 1)),
            // ADDC A, R7
            0x3F => Ok((ISA8051::ADDC(AddressingMode::Register(Register8051::R7)), 1)),
            // JZ
            0x60 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))? as i8;
                Ok((ISA8051::JZ(arg1), 2))
            }
            // MOV @R0, #data
            0x76 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Indirect(Register8051::R0),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV @R1, #data
            0x77 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Indirect(Register8051::R1),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV @R0, A
            0xF6 => Ok((
                ISA8051::MOV(
                    AddressingMode::Indirect(Register8051::R0),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV @R1, A
            0xF7 => Ok((
                ISA8051::MOV(
                    AddressingMode::Indirect(Register8051::R1),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV @R0, iram addr
            0xA6 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Indirect(Register8051::R0),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV @R1, iram addr
            0xA7 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Indirect(Register8051::R1),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV A, #data
            0x74 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::A),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV A, @R0
            0xE6 => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Indirect(Register8051::R0),
                ),
                1,
            )),
            // MOV A, @R1
            0xE7 => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Indirect(Register8051::R1),
                ),
                1,
            )),
            // MOV A, R0
            0xE8 => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R0),
                ),
                1,
            )),
            // MOV A, R1
            0xE9 => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R1),
                ),
                1,
            )),
            // MOV A, R2
            0xEA => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R2),
                ),
                1,
            )),
            // MOV A, R3
            0xEB => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R3),
                ),
                1,
            )),
            // MOV A, R4
            0xEC => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R4),
                ),
                1,
            )),
            // MOV A, R5
            0xED => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R5),
                ),
                1,
            )),
            // MOV A, R6
            0xEE => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R6),
                ),
                1,
            )),
            // MOV A, R7
            0xEF => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R7),
                ),
                1,
            )),
            // MOV A, iram addr
            0xE5 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::A),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV C, bit addr
            0xA2 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::C),
                        AddressingMode::Bit(arg1),
                    ),
                    2,
                ))
            }
            // MOV DPTR, #data16
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
            // MOV R0, #data
            0x78 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R0),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R1, #data
            0x79 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R1),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R2, #data
            0x7A => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R2),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R3, #data
            0x7B => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R3),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R4, #data
            0x7C => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R4),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R5, #data
            0x7D => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R5),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R6, #data
            0x7E => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R6),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R7, #data
            0x7F => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R7),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // MOV R0, A
            0xF8 => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R0),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R1, A
            0xF9 => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R1),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R2, A
            0xFA => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R2),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R3, A
            0xFB => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R3),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R4, A
            0xFC => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R4),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R5, A
            0xFD => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R5),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R6, A
            0xFE => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R6),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R7, A
            0xFF => Ok((
                ISA8051::MOV(
                    AddressingMode::Register(Register8051::R7),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOV R0, iram addr
            0xA8 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R0),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R1, iram addr
            0xA9 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R1),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R2, iram addr
            0xAA => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R2),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R3, iram addr
            0xAB => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R3),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R4, iram addr
            0xAC => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R4),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R5, iram addr
            0xAD => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R5),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R6, iram addr
            0xAE => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R6),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV R7, iram addr
            0xAF => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Register(Register8051::R7),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // MOV bit addr, C
            0x92 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Bit(arg1),
                        AddressingMode::Register(Register8051::C),
                    ),
                    2,
                ))
            }
            // MOV bit addr, C
            0x75 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 2))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Immediate(arg2),
                    ),
                    3,
                ))
            }
            // MOV iram addr, @R0
            0x86 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Indirect(Register8051::R0),
                    ),
                    2,
                ))
            }
            // MOV iram addr, @R1
            0x87 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Indirect(Register8051::R1),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R0
            0x88 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R0),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R1
            0x89 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R1),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R2
            0x8A => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R2),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R3
            0x8B => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R3),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R4
            0x8C => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R4),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R5
            0x8D => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R5),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R6
            0x8E => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R6),
                    ),
                    2,
                ))
            }
            // MOV iram addr, R7
            0x8F => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::R7),
                    ),
                    2,
                ))
            }
            // MOV iram addr, A
            0xF5 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::A),
                    ),
                    2,
                ))
            }
            // MOV iram addr, iram addr
            0xF5 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 2))?;
                Ok((
                    ISA8051::MOV(AddressingMode::Direct(arg1), AddressingMode::Direct(arg2)),
                    3,
                ))
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
            ISA8051::ADD(operand2) => {
                let data = self.load(operand2)?;
                let result: u16 = (self.accumulator as u16) + (data as u16);
                let half_result: u8 = (self.accumulator & 0xf) + (data & 0xf);
                self.accumulator = (result & 0xff) as u8;

                // flags
                if result > 255 {
                    self.carry_flag = 1;
                } else {
                    self.carry_flag = 0;
                }
                if half_result > 16 {
                    self.auxillary_carry_flag = 1;
                } else {
                    self.auxillary_carry_flag = 0;
                }
                Ok(())
            }
            ISA8051::ADDC(operand2) => {
                let data = self.load(operand2)?;
                let result: u16 = (self.accumulator as u16) + (data as u16) + (self.carry_flag as u16);
                let half_result: u8 = (self.accumulator & 0xf) + (data & 0xf) + (self.carry_flag & 0x1);
                self.accumulator = (result & 0xff) as u8;

                // flags
                if result > 255 {
                    self.carry_flag = 1;
                } else {
                    self.carry_flag = 0;
                }
                if half_result > 16 {
                    self.auxillary_carry_flag = 1;
                } else {
                    self.auxillary_carry_flag = 0;
                }
                Ok(())
            }
            ISA8051::ANL(operand1, operand2) => {
                let data = self.load(operand1)? & self.load(operand2)?;
                self.store(operand1, data)
            }
            ISA8051::JZ(address) => {
                println!("accumulator = {}", self.accumulator);
                if self.accumulator == 0 {
                    next_program_counter = ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            ISA8051::MOV(operand1, operand2) => {
                let data = self.load(operand2)?;
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
        MemoryMapperModule {
            rom: rom,
            iram: iram,
            xram: xram,
        }
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
    let xram = Rc::new(RAM::create_with_size(32768));

    // create mapper
    let mut mapper = Rc::new(MemoryMapperModule::new(rom, iram, xram));

    // create the cpu
    let mut cpu = CPU8051::new(mapper);
    loop {
        cpu.step()?;
    }

    Ok(())
}
