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
    // external ram indirect address (movx)
    IndirectExternal(Register8051),
    // code rom indirect (DPTR or PC) + offset (A) indirect access (movc)
    IndirectCode(Register8051),
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
    POP(AddressingMode),
    PUSH(AddressingMode),
    RET,
    RETI,
    RL,
    RLC,
    RR,
    RRC,
    SETB(AddressingMode),
    SJMP(i8),
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
    stack_pointer: u8,
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
            stack_pointer: 0,
            data_pointer: 0,
            program_counter: 0,
            memory: memory,
        }
    }

    // register from number
    fn register_from_id(id: u8) -> Register8051 {
        match id & 0x7 {
            0 => Register8051::R0,
            1 => Register8051::R1,
            2 => Register8051::R2,
            3 => Register8051::R3,
            4 => Register8051::R4,
            5 => Register8051::R5,
            6 => Register8051::R6,
            7 => Register8051::R7,
            _ => Register8051::A,
        }
    }

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
                    match bit {
                        _ => mem.read_memory(Address::Bit(bit)),
                    }
                }
            }
            AddressingMode::Direct(address) => {
                // 128-byte iram of 8051 vs SFR (upper 128 on 8052 can only be used via indirect)
                if address < 128 {
                    mem.read_memory(Address::InternalData(address))
                } else {
                    match address {
                        0x81 => Ok(self.stack_pointer),
                        0x82 => Ok((self.data_pointer & 0xff) as u8),
                        0x83 => Ok(((self.data_pointer >> 8) & 0xff) as u8),
                        0xE0 => Ok(self.accumulator),
                        _ => mem.read_memory(Address::SpecialFunctionRegister(address)),
                    }
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
            AddressingMode::IndirectExternal(register) => {
                // R0 or R1 indirect load
                let bank = self.bank << 3;
                match register {
                    Register8051::R0 => {
                        let address = mem.read_memory(Address::InternalData(bank + 0))?;
                        mem.read_memory(Address::ExternalData(address as u16))
                    }
                    Register8051::R1 => {
                        let address = mem.read_memory(Address::InternalData(bank + 1))?;
                        mem.read_memory(Address::ExternalData(address as u16))
                    }
                    Register8051::DPTR => mem.read_memory(Address::ExternalData(self.data_pointer)),
                    _ => Err("unsupported register for indirect load (external)"),
                }
            }
            AddressingMode::IndirectCode(register) => match register {
                Register8051::DPTR => {
                    mem.read_memory(Address::Code(self.data_pointer + (self.accumulator as u16)))
                }
                Register8051::PC => mem.read_memory(Address::Code(
                    self.program_counter + (self.accumulator as u16) + 1,
                )),
                _ => Err("unsupported register for indirect load (code)"),
            },
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
                    match bit {
                        _ => mem.write_memory(Address::Bit(bit), 1),
                    }
                }
            }
            AddressingMode::Direct(address) => {
                // 128-byte iram of 8051 vs SFR (upper 128 on 8052 can only be used via indirect)
                if address < 128 {
                    mem.write_memory(Address::InternalData(address), data)
                } else {
                    match address {
                        0x81 => {
                            self.stack_pointer = data;
                            Ok(())
                        }
                        0x82 => {
                            self.data_pointer = (self.data_pointer & 0xff00) | (data as u16);
                            Ok(())
                        }
                        0x83 => {
                            self.data_pointer = (self.data_pointer & 0x00ff) | ((data as u16) << 8);
                            Ok(())
                        }
                        0xE0 => {
                            self.accumulator = data;
                            Ok(())
                        }
                        _ => mem.write_memory(Address::SpecialFunctionRegister(address), data),
                    }
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
            AddressingMode::IndirectExternal(register) => {
                // R0 or R1 indirect store
                let bank = self.bank << 3;
                match register {
                    Register8051::R0 => {
                        let address = mem.read_memory(Address::InternalData(bank + 0))?;
                        mem.write_memory(Address::ExternalData(address as u16), data)
                    }
                    Register8051::R1 => {
                        let address = mem.read_memory(Address::InternalData(bank + 1))?;
                        mem.write_memory(Address::ExternalData(address as u16), data)
                    }
                    Register8051::DPTR => {
                        mem.write_memory(Address::ExternalData(self.data_pointer), data)
                    }
                    _ => Err("unsupported register for indirect store"),
                }
            }
            _ => Err("unsupported addressing mode (store)"),
        }
    }

    // decode the next instruction and return the next program counter
    fn decode_next_instruction(&mut self) -> Result<(ISA8051, u16), &'static str> {
        let mut mem = Rc::get_mut(&mut self.memory).unwrap();
        let opcode = mem.read_memory(Address::Code(self.program_counter))?;

        // decode instruction
        match opcode {
            // ANL iram addr, A
            0x52 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ANL(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::A),
                    ),
                    2,
                ))
            }
            // AJMP #address
            0x01 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let address = (((opcode & 0xE0) as u16) << 3) | (arg1 as u16);
                Ok((ISA8051::AJMP(address), 2))
            }
            // ADD A, #data
            0x24 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::ADD(AddressingMode::Immediate(arg1)), 2))
            }
            // ADD A, iram addr
            0x25 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::ADD(AddressingMode::Direct(arg1)), 2))
            }
            // ADD A, @R0
            0x26 => Ok((ISA8051::ADD(AddressingMode::Indirect(Register8051::R0)), 1)),
            // ADD A, @R1
            0x27 => Ok((ISA8051::ADD(AddressingMode::Indirect(Register8051::R1)), 1)),
            // ADD A, R0..R7
            0x28..=0x2F => Ok((
                ISA8051::ADD(AddressingMode::Register(CPU8051::<A>::register_from_id(
                    opcode,
                ))),
                1,
            )),
            // ADDC A, #data
            0x34 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::ADDC(AddressingMode::Immediate(arg1)), 2))
            }
            // ADDC A, iram addr
            0x35 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::ADDC(AddressingMode::Direct(arg1)), 2))
            }
            // ADDC A, @R0
            0x36 => Ok((ISA8051::ADDC(AddressingMode::Indirect(Register8051::R0)), 1)),
            // ADDC A, @R1
            0x37 => Ok((ISA8051::ADDC(AddressingMode::Indirect(Register8051::R1)), 1)),
            // ADDC A, R0..R7
            0x38..=0x3F => Ok((ISA8051::ADDC(AddressingMode::Register(CPU8051::<A>::register_from_id(opcode))), 1)),
            // CJNE A, #data, reladdr
            0xB4 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::A),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE A, iram addr, reladdr
            0xB5 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::A),
                        AddressingMode::Direct(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE @R0, #data, reladdr
            0xB6 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Indirect(Register8051::R0),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE @R1, #data, reladdr
            0xB7 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Indirect(Register8051::R1),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R0, #data, reladdr
            0xB8 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R0),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R1, #data, reladdr
            0xB9 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R1),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R2, #data, reladdr
            0xBA => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R2),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R3, #data, reladdr
            0xBB => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R3),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R4, #data, reladdr
            0xBC => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R4),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R5, #data, reladdr
            0xBD => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R5),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R6, #data, reladdr
            0xBE => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R6),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CJNE R7, #data, reladdr
            0xBF => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((
                    ISA8051::CJNE(
                        AddressingMode::Register(Register8051::R7),
                        AddressingMode::Immediate(arg1),
                        arg2,
                    ),
                    3,
                ))
            }
            // CLR bit addr
            0xC2 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::CLR(AddressingMode::Bit(arg1)), 2))
            }
            // CLR C
            0xC3 => Ok((ISA8051::CLR(AddressingMode::Register(Register8051::C)), 1)),
            // CLR A
            0xE4 => Ok((ISA8051::CLR(AddressingMode::Register(Register8051::A)), 1)),
            // DJNZ iram addr, reladdr
            0xD5 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))? as i8;
                Ok((ISA8051::DJNZ(AddressingMode::Direct(arg1), arg2), 3))
            }
            // DJNZ R0, reladdr
            0xD8 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R0), arg1),
                    2,
                ))
            }
            // DJNZ R1, reladdr
            0xD9 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R1), arg1),
                    2,
                ))
            }
            // DJNZ R2, reladdr
            0xDA => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R2), arg1),
                    2,
                ))
            }
            // DJNZ R3, reladdr
            0xDB => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R3), arg1),
                    2,
                ))
            }
            // DJNZ R4, reladdr
            0xDC => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R4), arg1),
                    2,
                ))
            }
            // DJNZ R5, reladdr
            0xDD => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R5), arg1),
                    2,
                ))
            }
            // DJNZ R6, reladdr
            0xDE => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R6), arg1),
                    2,
                ))
            }
            // DJNZ R7, reladdr
            0xDF => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((
                    ISA8051::DJNZ(AddressingMode::Register(Register8051::R7), arg1),
                    2,
                ))
            }
            // INC A
            0x04 => Ok((ISA8051::INC(AddressingMode::Register(Register8051::A)), 1)),
            // INC iram addr
            0x05 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::INC(AddressingMode::Direct(arg1)), 2))
            }
            // INC @R0
            0x06 => Ok((ISA8051::INC(AddressingMode::Indirect(Register8051::R0)), 1)),
            // INC @R1
            0x07 => Ok((ISA8051::INC(AddressingMode::Indirect(Register8051::R1)), 1)),
            // INC R0
            0x08 => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R0)), 1)),
            // INC R1
            0x09 => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R1)), 1)),
            // INC R2
            0x0A => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R2)), 1)),
            // INC R3
            0x0B => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R3)), 1)),
            // INC R4
            0x0C => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R4)), 1)),
            // INC R5
            0x0D => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R5)), 1)),
            // INC R6
            0x0E => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R6)), 1)),
            // INC R7
            0x0F => Ok((ISA8051::INC(AddressingMode::Register(Register8051::R7)), 1)),
            // INC DPTR
            0xA3 => Ok((
                ISA8051::INC(AddressingMode::Register(Register8051::DPTR)),
                1,
            )),
            // JNZ
            0x70 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((ISA8051::JNZ(arg1), 2))
            }
            // JZ
            0x60 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((ISA8051::JZ(arg1), 2))
            }
            // LCALL #address
            0x12 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))?;
                let address = ((arg1 as u16) << 8) | (arg2 as u16);
                Ok((ISA8051::LCALL(address), 3))
            }
            // LJMP #address
            0x02 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))?;
                let address = ((arg1 as u16) << 8) | (arg2 as u16);
                Ok((ISA8051::LJMP(address), 3))
            }
            // MOV @R0, #data
            0x76 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))?;
                let pointer = ((arg1 as u16) << 8) | (arg2 as u16);
                Ok((ISA8051::LoadDptr(pointer), 3))
            }
            // MOV R0, #data
            0x78 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
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
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::MOV(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::A),
                    ),
                    2,
                ))
            }
            // MOV iram addr, iram addr
            0x85 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))?;
                Ok((
                    ISA8051::MOV(AddressingMode::Direct(arg1), AddressingMode::Direct(arg2)),
                    3,
                ))
            }
            // MOVC A, @A+DPTR
            0x93 => Ok((
                ISA8051::MOVC(AddressingMode::IndirectCode(Register8051::DPTR)),
                1,
            )),
            // MOVC A, @A+DPTR
            0x83 => Ok((
                ISA8051::MOVC(AddressingMode::IndirectCode(Register8051::PC)),
                1,
            )),
            // MOVX @DPTR, A
            0xF0 => Ok((
                ISA8051::MOVX(
                    AddressingMode::IndirectExternal(Register8051::DPTR),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOVX @R0, A
            0xF2 => Ok((
                ISA8051::MOVX(
                    AddressingMode::IndirectExternal(Register8051::R0),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOVX @R1, A
            0xF3 => Ok((
                ISA8051::MOVX(
                    AddressingMode::IndirectExternal(Register8051::R1),
                    AddressingMode::Register(Register8051::A),
                ),
                1,
            )),
            // MOVX A, @DPTR
            0xE0 => Ok((
                ISA8051::MOVX(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::IndirectExternal(Register8051::DPTR),
                ),
                1,
            )),
            // MOVX A, @R0
            0xE2 => Ok((
                ISA8051::MOVX(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::IndirectExternal(Register8051::R0),
                ),
                1,
            )),
            // MOVX A, @R1
            0xE3 => Ok((
                ISA8051::MOVX(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::IndirectExternal(Register8051::R1),
                ),
                1,
            )),
            // NOP
            0x00 => Ok((ISA8051::NOP, 1)),
            // ORL iram addr, A
            0x42 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ORL(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Register(Register8051::A),
                    ),
                    2,
                ))
            }
            // ORL iram addr, #data
            0x43 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                let arg2 = mem.read_memory(Address::Code(self.program_counter + 2))?;
                Ok((
                    ISA8051::ORL(
                        AddressingMode::Direct(arg1),
                        AddressingMode::Immediate(arg2),
                    ),
                    3,
                ))
            }
            // ORL A, #data
            0x44 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ORL(
                        AddressingMode::Register(Register8051::A),
                        AddressingMode::Immediate(arg1),
                    ),
                    2,
                ))
            }
            // ORL A, #data
            0x45 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ORL(
                        AddressingMode::Register(Register8051::A),
                        AddressingMode::Direct(arg1),
                    ),
                    2,
                ))
            }
            // ORL A, @R0
            0x46 => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Indirect(Register8051::R0),
                ),
                1,
            )),
            // ORL A, @R1
            0x47 => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Indirect(Register8051::R1),
                ),
                1,
            )),
            // ORL A, R0
            0x48 => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R0),
                ),
                1,
            )),
            // ORL A, R1
            0x49 => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R1),
                ),
                1,
            )),
            // ORL A, R2
            0x4A => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R2),
                ),
                1,
            )),
            // ORL A, R3
            0x4B => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R3),
                ),
                1,
            )),
            // ORL A, R4
            0x4C => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R4),
                ),
                1,
            )),
            // ORL A, R5
            0x4D => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R5),
                ),
                1,
            )),
            // ORL A, R6
            0x4E => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R6),
                ),
                1,
            )),
            // ORL A, R7
            0x4F => Ok((
                ISA8051::ORL(
                    AddressingMode::Register(Register8051::A),
                    AddressingMode::Register(Register8051::R7),
                ),
                1,
            )),
            // ORL C, bit addr
            0x72 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ORL(
                        AddressingMode::Register(Register8051::C),
                        AddressingMode::Bit(arg1),
                    ),
                    2,
                ))
            }
            // ORL C, /bit addr (C <- C or NOT bit)
            /*0xA0 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((
                    ISA8051::ORL(
                        AddressingMode::Register(Register8051::C),
                        AddressingMode::Bit(arg1),
                    ),
                    2,
                ))
            }*/
            // POP
            0xD0 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::POP(AddressingMode::Direct(arg1)), 2))
            }
            // PUSH
            0xC0 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::PUSH(AddressingMode::Direct(arg1)), 2))
            }
            // RET
            0x22 => Ok((ISA8051::RET, 1)),
            // RETI
            0x32 => Ok((ISA8051::RETI, 1)),
            // SETB C
            0xD3 => Ok((ISA8051::SETB(AddressingMode::Register(Register8051::C)), 1)),
            // SETB bit addr
            0xD2 => {
                let arg1 = mem.read_memory(Address::Code(self.program_counter + 1))?;
                Ok((ISA8051::SETB(AddressingMode::Bit(arg1)), 2))
            }
            // SJMP reladdr
            0x80 => {
                let arg1 = Rc::get_mut(&mut self.memory)
                    .unwrap()
                    .read_memory(Address::Code(self.program_counter + 1))?
                    as i8;
                Ok((ISA8051::SJMP(arg1), 2))
            }
            // catch unimplemented
            _ => {
                println!("unknown opcode - 0x{:02x}", opcode);
                Err("unimplemented instruction (decode)")
            }
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
            ISA8051::CJNE(operand1, operand2, offset) => {
                if self.load(operand1)? != self.load(operand2)? {
                    next_program_counter = ((next_program_counter as i16) + (offset as i16)) as u16;
                }
                Ok(())
            }
            ISA8051::CLR(address) => self.store(address, 0),
            ISA8051::DJNZ(address, offset) => {
                let mut data = self.load(address)?;
                println!("{:?} = {} -> {}", address, data, data - 1);
                data = data - 1;
                self.store(address, data)?;
                if data != 0 {
                    next_program_counter = ((next_program_counter as i16) + (offset as i16)) as u16;
                }
                Ok(())
            }
            ISA8051::INC(address) => {
                if let AddressingMode::Register(Register8051::DPTR) = address {
                    self.data_pointer = self.data_pointer + 1;
                    Ok(())
                } else {
                    let data = self.load(address)?;
                    self.store(address, data + 1)
                }
            }
            ISA8051::LJMP(address) => {
                next_program_counter = address;
                Ok(())
            }
            ISA8051::LCALL(address) => {
                if self.stack_pointer >= 127 {
                    panic!("stack overflow in LCALL");
                }
                let mut mem = Rc::get_mut(&mut self.memory).unwrap();
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 1),
                    (next_program_counter & 0xff) as u8,
                )?;
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 2),
                    ((next_program_counter >> 8) & 0xff) as u8,
                )?;
                self.stack_pointer = self.stack_pointer + 2;
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
                let result: u16 =
                    (self.accumulator as u16) + (data as u16) + (self.carry_flag as u16);
                let half_result: u8 =
                    (self.accumulator & 0xf) + (data & 0xf) + (self.carry_flag & 0x1);
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
            ISA8051::JNZ(address) => {
                println!("accumulator = {}", self.accumulator);
                if self.accumulator != 0 {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            ISA8051::JZ(address) => {
                println!("accumulator = {}", self.accumulator);
                if self.accumulator == 0 {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            ISA8051::MOV(operand1, operand2) => {
                let data = self.load(operand2)?;
                self.store(operand1, data)
            }
            ISA8051::MOVC(operand) => {
                self.accumulator = self.load(operand)?;
                Ok(())
            }
            ISA8051::MOVX(operand1, operand2) => {
                let data = self.load(operand2)?;
                self.store(operand1, data)
            }
            ISA8051::ORL(operand1, operand2) => {
                let data = self.load(operand1)? | self.load(operand2)?;
                self.store(operand1, data)
            }
            ISA8051::POP(address) => {
                let mut mem = Rc::get_mut(&mut self.memory).unwrap();
                let data = mem.read_memory(Address::InternalData(self.stack_pointer))?;
                self.stack_pointer = self.stack_pointer - 1;
                self.store(address, data)
            }
            ISA8051::PUSH(address) => {
                if self.stack_pointer >= 127 {
                    panic!("stack overflow in PUSH");
                }
                let data = self.load(address)?;
                let mut mem = Rc::get_mut(&mut self.memory).unwrap();
                mem.write_memory(Address::InternalData(self.stack_pointer + 1), data)?;
                self.stack_pointer = self.stack_pointer + 1;
                Ok(())
            }
            ISA8051::RET => {
                let mut mem = Rc::get_mut(&mut self.memory).unwrap();
                next_program_counter =
                    mem.read_memory(Address::InternalData(self.stack_pointer))? as u16;
                next_program_counter <<= 8;
                next_program_counter |=
                    mem.read_memory(Address::InternalData(self.stack_pointer - 1))? as u16;
                self.stack_pointer = self.stack_pointer - 2;
                Ok(())
            }
            ISA8051::RETI => {
                let mut mem = Rc::get_mut(&mut self.memory).unwrap();
                next_program_counter =
                    mem.read_memory(Address::InternalData(self.stack_pointer))? as u16;
                next_program_counter <<= 8;
                next_program_counter |=
                    mem.read_memory(Address::InternalData(self.stack_pointer - 1))? as u16;
                self.stack_pointer = self.stack_pointer - 2;
                Ok(())
            }
            ISA8051::SETB(address) => self.store(address, 1),
            ISA8051::SJMP(offset) => {
                next_program_counter = ((next_program_counter as i16) + (offset as i16)) as u16;
                Ok(())
            }
            ISA8051::LoadDptr(a) => {
                self.data_pointer = a;
                Ok(())
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

struct P80C550<A: Memory, B: Memory, C: Memory> {
    rom: Rc<A>,
    iram: Rc<B>,
    xram: Rc<C>,

    // 8051 peripherals
    tcon: u8,
    tmod: u8,
    tl0: u8,
    tl1: u8,
    th0: u8,
    th1: u8,
    ie: u8,
}

impl<A: Memory, B: Memory, C: Memory> P80C550<A, B, C> {
    fn new(rom: Rc<A>, iram: Rc<B>, xram: Rc<C>) -> P80C550<A, B, C> {
        P80C550 {
            rom: rom,
            iram: iram,
            xram: xram,
            tcon: 0,
            tmod: 0,
            tl0: 0,
            tl1: 0,
            th0: 0,
            th1: 0,
            ie: 0,
        }
    }
}

impl<A: Memory, B: Memory, C: Memory> Memory for P80C550<A, B, C> {
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
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x88..=0x8F => {
                        if (self.tcon >> (bit & 0x7)) != 0 {
                            Ok(1)
                        } else {
                            Ok(0)
                        }
                    }
                    0xA8..=0xAF => {
                        if (self.ie >> (bit & 0x7)) != 0 {
                            Ok(1)
                        } else {
                            Ok(0)
                        }
                    }
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x88 => Ok(self.tcon),
                0x89 => Ok(self.tmod),
                0x8A => Ok(self.tl0),
                0x8B => Ok(self.tl1),
                0x8C => Ok(self.th0),
                0x8D => Ok(self.th1),
                0xA8 => Ok(self.ie),
                _ => Err("non-existant SFR"),
            },
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
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x88..=0x8F => {
                        if data != 0 {
                            self.tcon |= (1 << (bit & 0x7));
                        } else {
                            self.tcon &= !(1 << (bit & 0x07));
                        }
                        Ok(())
                    }
                    0xA8..=0xAF => {
                        if data != 0 {
                            self.ie |= (1 << (bit & 0x7));
                        } else {
                            self.ie &= !(1 << (bit & 0x07));
                        }
                        Ok(())
                    }
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x88 => {
                    self.tcon = data;
                    Ok(())
                }
                0x89 => {
                    self.tmod = data;
                    Ok(())
                }
                0x8a => {
                    self.tl0 = data;
                    Ok(())
                }
                0x8b => {
                    self.tl1 = data;
                    Ok(())
                }
                0x8c => {
                    self.th0 = data;
                    Ok(())
                }
                0x8d => {
                    self.th1 = data;
                    Ok(())
                }
                0xa8 => {
                    self.ie = data;
                    Ok(())
                }
                _ => Err("non-existant SFR"),
            },
            _ => Err("unsupported addressing mode for memory mapper (write)"),
        }
    }
}

struct Peripherals<A: Memory> {
    ram: Rc<A>,
}

impl<A: Memory> Peripherals<A> {
    fn new(ram: Rc<A>) -> Peripherals<A> {
        Peripherals { ram: ram }
    }
}

impl<A: Memory> Memory for Peripherals<A> {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::ExternalData(a) => {
                if a < 0x8000 {
                    Rc::get_mut(&mut self.ram).unwrap().read_memory(address)
                } else {
                    match a {
                        0x8400 => {
                            println!("spi.data read");
                            Ok((0xFF))
                        }
                        0x8401 => {
                            println!("spi.control read");
                            Ok((0x80))
                        }
                        _ => Err("unused address (read)"),
                    }
                }
            }
            _ => Err("unsupported address space"),
        }
    }
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::ExternalData(a) => {
                if a < 0x8000 {
                    Rc::get_mut(&mut self.ram)
                        .unwrap()
                        .write_memory(address, data)
                } else {
                    match a {
                        0x8400 => {
                            // spi control
                            println!("spi.data = {:x}", data);
                            Ok(())
                        }
                        0x8401 => {
                            // spi control
                            println!("spi.control = {:x}", data);
                            Ok(())
                        }
                        0x9400 => {
                            // uart channel b control
                            println!("am85c30.channel.b.control = {:x}", data);
                            Ok(())
                        }
                        _ => Err("unused address (write)"),
                    }
                }
            }
            _ => Err("unsupported address space"),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    // load the application rom
    let rom_path = Path::new("rom.bin");
    let rom = Rc::new(ROM::load_from_binary(rom_path)?);

    // create ram for the application
    let iram = Rc::new(RAM::create_with_size(128));
    let xram = Rc::new(RAM::create_with_size(32768));
    let peripherals = Rc::new(Peripherals::new(xram));

    // create mapper
    let mut mapper = Rc::new(P80C550::new(rom, iram, peripherals));

    // create the cpu
    let mut cpu = CPU8051::new(mapper);
    loop {
        cpu.step()?;
    }

    Ok(())
}
