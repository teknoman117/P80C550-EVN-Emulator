use crate::mcs51::memory::Memory;
use crate::mcs51::{get_bit, set_bit};

use bitflags::bitflags;

use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
pub enum Address {
    Code(u16),
    ExternalData(u16),
    InternalData(u8),
    SpecialFunctionRegister(u8),
    Bit(u8),
}

#[derive(Clone, Copy, Debug)]
pub enum Register {
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

#[derive(Clone, Copy, Debug)]
pub enum AddressingMode {
    // Immediate (most immediates)
    Immediate(u8),
    // register
    Register(Register),
    // bit direct address
    Bit(u8),
    // bit direct address, NOT of bit
    NotBit(u8),
    // internal ram direct address
    Direct(u8),
    // internal ram indirect address
    Indirect(Register),
    // external ram indirect address (movx)
    IndirectExternal(Register),
    // code rom indirect (DPTR or PC) + offset (A) indirect access (movc)
    IndirectCode(Register),
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
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
    Interrupt(u16, u8),
    JB(AddressingMode, i8),
    JBC(AddressingMode, i8),
    JC(i8),
    JMP,
    JNB(AddressingMode, i8),
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
}

fn register_from_op(id: u8) -> Register {
    match id & 0x7 {
        0 => Register::R0,
        1 => Register::R1,
        2 => Register::R2,
        3 => Register::R3,
        4 => Register::R4,
        5 => Register::R5,
        6 => Register::R6,
        7 => Register::R7,
        _ => Register::A,
    }
}

bitflags! {
    struct Flags: u8 {
        const PARITY =         0b00000001;
        const USER0 =          0b00000010;
        const OVERFLOW =       0b00000100;
        const BANKSELECT0 =    0b00001000;
        const BANKSELECT1 =    0b00010000;
        const USER1 =          0b00100000;
        const AUXILIARYCARRY = 0b01000000;
        const CARRY =          0b10000000;
    }
}

impl Flags {
    pub fn bank(&self) -> u8 {
        self.bits & (Flags::BANKSELECT1 | Flags::BANKSELECT0).bits
    }
    pub fn carry(&self) -> u8 {
        if self.contains(Flags::CARRY) {
            1
        } else {
            0
        }
    }
}

pub trait InterruptSource {
    // get a vector of with equal or greater priority (return vector and priority)
    fn peek_vector(&mut self) -> Option<(u16, u8)>;
    fn pop_vector(&mut self);
}

pub struct CPU<A>
where
    A: Memory + InterruptSource,
{
    flags: Flags,
    accumulator: u8,
    b: u8,
    stack_pointer: u8,
    data_pointer: u16,
    program_counter: u16,
    memory: Rc<A>,
    ip0: bool,
    ip1: bool,
}

impl<A> CPU<A>
where
    A: Memory + InterruptSource,
{
    pub fn new(memory: Rc<A>) -> CPU<A> {
        CPU {
            flags: Flags::empty(),
            accumulator: 0,
            b: 0,
            stack_pointer: 0,
            data_pointer: 0,
            program_counter: 0,
            memory: memory,
            ip0: false,
            ip1: false
        }
    }

    // perform a load using a particular addressing mode
    fn load(&mut self, mode: AddressingMode) -> Result<u8, &'static str> {
        let mem = Rc::get_mut(&mut self.memory).unwrap();
        match mode {
            AddressingMode::Immediate(imm8) => Ok(imm8),
            AddressingMode::Register(register) => match register {
                Register::A => Ok(self.accumulator),
                Register::C => Ok(self.flags.carry()),
                Register::R0 => mem.read_memory(Address::InternalData(self.flags.bank() + 0)),
                Register::R1 => mem.read_memory(Address::InternalData(self.flags.bank() + 1)),
                Register::R2 => mem.read_memory(Address::InternalData(self.flags.bank() + 2)),
                Register::R3 => mem.read_memory(Address::InternalData(self.flags.bank() + 3)),
                Register::R4 => mem.read_memory(Address::InternalData(self.flags.bank() + 4)),
                Register::R5 => mem.read_memory(Address::InternalData(self.flags.bank() + 5)),
                Register::R6 => mem.read_memory(Address::InternalData(self.flags.bank() + 6)),
                Register::R7 => mem.read_memory(Address::InternalData(self.flags.bank() + 7)),
                _ => Err("unsupported register"),
            },
            AddressingMode::Bit(bit) => {
                // 8051 bit values occupy 0x20 to 0x2F
                if bit < 128 {
                    let octet = mem.read_memory(Address::InternalData(0x20 + (bit >> 3)))?;
                    Ok(get_bit(octet, bit & 7))
                } else {
                    match bit {
                        0xD0..=0xD7 => {
                            let flag = Flags::from_bits(1 << (bit & 7)).unwrap();
                            if self.flags.contains(flag) {
                                Ok(1)
                            } else {
                                Ok(0)
                            }
                        }
                        0xE0..=0xE7 => Ok(get_bit(self.accumulator, bit & 7)),
                        0xF0..=0xF7 => Ok(get_bit(self.b, bit & 7)),
                        _ => mem.read_memory(Address::Bit(bit)),
                    }
                }
            }
            AddressingMode::NotBit(bit) => Ok(!self.load(AddressingMode::Bit(bit))? & 0x1),
            AddressingMode::Direct(address) => {
                // 128-byte iram of 8051 vs SFR (upper 128 on 8052 can only be used via indirect)
                if address < 128 {
                    mem.read_memory(Address::InternalData(address))
                } else {
                    match address {
                        0x81 => Ok(self.stack_pointer),
                        0x82 => Ok(self.data_pointer.to_le_bytes()[0]),
                        0x83 => Ok(self.data_pointer.to_le_bytes()[1]),
                        0xD0 => Ok(self.flags.bits),
                        0xE0 => Ok(self.accumulator),
                        0xF0 => Ok(self.b),
                        _ => mem.read_memory(Address::SpecialFunctionRegister(address)),
                    }
                }
            }
            AddressingMode::Indirect(register) => match register {
                Register::R0 => {
                    let address = mem.read_memory(Address::InternalData(self.flags.bank() + 0))?;
                    mem.read_memory(Address::InternalData(address))
                }
                Register::R1 => {
                    let address = mem.read_memory(Address::InternalData(self.flags.bank() + 1))?;
                    mem.read_memory(Address::InternalData(address))
                }
                _ => Err("unsupported register for indirect load"),
            },
            AddressingMode::IndirectExternal(register) => match register {
                // port 2 forms the upper 8 bits of an indirect external access with R0/1
                Register::R0 => {
                    let address = [
                        mem.read_memory(Address::InternalData(self.flags.bank() + 0))?,
                        mem.read_memory(Address::SpecialFunctionRegister(0xA0))?,
                    ];
                    mem.read_memory(Address::ExternalData(u16::from_le_bytes(address)))
                }
                Register::R1 => {
                    let address = [
                        mem.read_memory(Address::InternalData(self.flags.bank() + 1))?,
                        mem.read_memory(Address::SpecialFunctionRegister(0xA0))?,
                    ];
                    mem.read_memory(Address::ExternalData(u16::from_le_bytes(address)))
                }
                Register::DPTR => mem.read_memory(Address::ExternalData(self.data_pointer)),
                _ => Err("unsupported register for indirect load (external)"),
            },
            AddressingMode::IndirectCode(register) => match register {
                Register::DPTR => {
                    mem.read_memory(Address::Code(self.data_pointer + (self.accumulator as u16)))
                }
                Register::PC => mem.read_memory(Address::Code(
                    self.program_counter + (self.accumulator as u16) + 1,
                )),
                _ => Err("unsupported register for indirect load (code)"),
            },
        }
    }

    // perform a store using an addressing mode
    fn store(&mut self, mode: AddressingMode, data: u8) -> Result<(), &'static str> {
        let mem = Rc::get_mut(&mut self.memory).unwrap();
        match mode {
            AddressingMode::Register(register) => match register {
                Register::A => {
                    self.accumulator = data;
                    Ok(())
                }
                Register::C => {
                    self.flags.set(Flags::CARRY, data != 0);
                    Ok(())
                }
                Register::R0 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 0), data)
                }
                Register::R1 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 1), data)
                }
                Register::R2 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 2), data)
                }
                Register::R3 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 3), data)
                }
                Register::R4 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 4), data)
                }
                Register::R5 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 5), data)
                }
                Register::R6 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 6), data)
                }
                Register::R7 => {
                    mem.write_memory(Address::InternalData(self.flags.bank() + 7), data)
                }
                _ => Err("unsupported register"),
            },
            AddressingMode::Bit(bit) => {
                // 8051 bit values occupy 0x20 to 0x2F
                if bit < 128 {
                    let octet = mem.read_memory(Address::InternalData(0x20 + (bit >> 3)))?;
                    mem.write_memory(
                        Address::InternalData(0x20 + (bit >> 3)),
                        set_bit(octet, bit & 7, data != 0),
                    )
                } else {
                    match bit {
                        0xD0..=0xD7 => {
                            let flag = Flags::from_bits(1 << (bit & 7)).unwrap();
                            self.flags.set(flag, data != 0);
                            Ok(())
                        }
                        0xE0..=0xE7 => {
                            self.accumulator = set_bit(self.accumulator, bit & 7, data != 0);
                            Ok(())
                        }
                        0xF0..=0xF7 => {
                            self.b = set_bit(self.b, bit & 7, data != 0);
                            Ok(())
                        }
                        _ => mem.write_memory(Address::Bit(bit), data),
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
                            self.data_pointer =
                                u16::from_le_bytes([data, self.data_pointer.to_le_bytes()[1]]);
                            Ok(())
                        }
                        0x83 => {
                            self.data_pointer =
                                u16::from_le_bytes([self.data_pointer.to_le_bytes()[0], data]);
                            Ok(())
                        }
                        0xD0 => {
                            self.flags.bits = data;
                            Ok(())
                        }
                        0xE0 => {
                            self.accumulator = data;
                            Ok(())
                        }
                        0xF0 => {
                            self.b = data;
                            Ok(())
                        }
                        _ => mem.write_memory(Address::SpecialFunctionRegister(address), data),
                    }
                }
            }
            AddressingMode::Indirect(register) => match register {
                Register::R0 => {
                    let address = mem.read_memory(Address::InternalData(self.flags.bank() + 0))?;
                    mem.write_memory(Address::InternalData(address), data)
                }
                Register::R1 => {
                    let address = mem.read_memory(Address::InternalData(self.flags.bank() + 1))?;
                    mem.write_memory(Address::InternalData(address), data)
                }
                _ => Err("unsupported register for indirect store"),
            },
            AddressingMode::IndirectExternal(register) => match register {
                // port 2 forms the upper 8 bits of an indirect external access with R0/1
                Register::R0 => {
                    let address = [
                        mem.read_memory(Address::InternalData(self.flags.bank() + 0))?,
                        mem.read_memory(Address::SpecialFunctionRegister(0xA0))?,
                    ];
                    mem.write_memory(Address::ExternalData(u16::from_le_bytes(address)), data)
                }
                Register::R1 => {
                    let address = [
                        mem.read_memory(Address::InternalData(self.flags.bank() + 1))?,
                        mem.read_memory(Address::SpecialFunctionRegister(0xA0))?,
                    ];
                    mem.write_memory(Address::ExternalData(u16::from_le_bytes(address)), data)
                }
                Register::DPTR => mem.write_memory(Address::ExternalData(self.data_pointer), data),
                _ => Err("unsupported register for indirect store"),
            },
            _ => Err("unsupported addressing mode (store)"),
        }
    }

    // decode the next instruction
    fn decode_next_opcode(&mut self) -> Result<Instruction, &'static str> {
        let mem = Rc::get_mut(&mut self.memory).unwrap();
        let opcode = mem.read_memory(Address::Code(self.program_counter))?;
        let arg1 = mem.read_memory(Address::Code(self.program_counter + 1));
        let arg2 = mem.read_memory(Address::Code(self.program_counter + 2));

        // decode instruction
        match opcode {
            // NOP
            0x00 => Ok(Instruction::NOP),
            // AJMP #address
            0x01 | 0x21 | 0x41 | 0x61 | 0x81 | 0xA1 | 0xC1 | 0xE1 => {
                let address = (((opcode & 0xE0) as u16) << 3) | (arg1? as u16);
                Ok(Instruction::AJMP(address))
            }
            // LJMP #address
            0x02 => {
                let address = ((arg1? as u16) << 8) | (arg2? as u16);
                Ok(Instruction::LJMP(address))
            }
            // RR A
            0x03 => Ok(Instruction::RR),
            // INC A
            0x04 => Ok(Instruction::INC(AddressingMode::Register(Register::A))),
            // INC iram addr
            0x05 => Ok(Instruction::INC(AddressingMode::Direct(arg1?))),
            // INC @R0
            0x06 => Ok(Instruction::INC(AddressingMode::Indirect(Register::R0))),
            // INC @R1
            0x07 => Ok(Instruction::INC(AddressingMode::Indirect(Register::R1))),
            // INC Rx
            0x08..=0x0F => Ok(Instruction::INC(AddressingMode::Register(
                register_from_op(opcode),
            ))),
            // JBC bit addr, reladdr
            0x10 => Ok(Instruction::JBC(AddressingMode::Bit(arg1?), arg2? as i8)),
            // ACALL #address
            0x11 | 0x31 | 0x51 | 0x71 | 0x91 | 0xB1 | 0xD1 | 0xF1 => {
                let address = (((opcode & 0xE0) as u16) << 3) | (arg1? as u16);
                Ok(Instruction::ACALL(address))
            }
            // LCALL #address
            0x12 => {
                let address = ((arg1? as u16) << 8) | (arg2? as u16);
                Ok(Instruction::LCALL(address))
            }
            // RRC A
            0x13 => Ok(Instruction::RRC),
            // DEC A
            0x14 => Ok(Instruction::DEC(AddressingMode::Register(Register::A))),
            // DEC iram addr
            0x15 => Ok(Instruction::DEC(AddressingMode::Direct(arg1?))),
            // DEC @R0
            0x16 => Ok(Instruction::DEC(AddressingMode::Indirect(Register::R0))),
            // DEC @R1
            0x17 => Ok(Instruction::DEC(AddressingMode::Indirect(Register::R1))),
            // DEC Rx
            0x18..=0x1F => Ok(Instruction::DEC(AddressingMode::Register(
                register_from_op(opcode),
            ))),
            // JB bit addr, reladdr
            0x20 => Ok(Instruction::JB(AddressingMode::Bit(arg1?), arg2? as i8)),
            // RET
            0x22 => Ok(Instruction::RET),
            // RL A
            0x23 => Ok(Instruction::RL),
            // ADD A, #data
            0x24 => Ok(Instruction::ADD(AddressingMode::Immediate(arg1?))),
            // ADD A, iram addr
            0x25 => Ok(Instruction::ADD(AddressingMode::Direct(arg1?))),
            // ADD A, @R0
            0x26 => Ok(Instruction::ADD(AddressingMode::Indirect(Register::R0))),
            // ADD A, @R1
            0x27 => Ok(Instruction::ADD(AddressingMode::Indirect(Register::R1))),
            // ADD A, Rx
            0x28..=0x2F => Ok(Instruction::ADD(AddressingMode::Register(
                register_from_op(opcode),
            ))),
            // JNB bit addr, reladdr
            0x30 => Ok(Instruction::JNB(AddressingMode::Bit(arg1?), arg2? as i8)),
            // RETI
            0x32 => Ok(Instruction::RETI),
            // RLC A
            0x33 => Ok(Instruction::RLC),
            // ADDC A, #data
            0x34 => Ok(Instruction::ADDC(AddressingMode::Immediate(arg1?))),
            // ADDC A, iram addr
            0x35 => Ok(Instruction::ADDC(AddressingMode::Direct(arg1?))),
            // ADDC A, @R0
            0x36 => Ok(Instruction::ADDC(AddressingMode::Indirect(Register::R0))),
            // ADDC A, @R1
            0x37 => Ok(Instruction::ADDC(AddressingMode::Indirect(Register::R1))),
            // ADDC A, Rx
            0x38..=0x3F => Ok(Instruction::ADDC(AddressingMode::Register(
                register_from_op(opcode),
            ))),
            // JC reladdr
            0x40 => Ok(Instruction::JC(arg1? as i8)),
            // ORL iram addr, A
            0x42 => Ok(Instruction::ORL(
                AddressingMode::Direct(arg1?),
                AddressingMode::Register(Register::A),
            )),
            // ORL iram addr, #data
            0x43 => Ok(Instruction::ORL(
                AddressingMode::Direct(arg1?),
                AddressingMode::Immediate(arg2?),
            )),
            // ORL A, #data
            0x44 => Ok(Instruction::ORL(
                AddressingMode::Register(Register::A),
                AddressingMode::Immediate(arg1?),
            )),
            // ORL A, iram addr
            0x45 => Ok(Instruction::ORL(
                AddressingMode::Register(Register::A),
                AddressingMode::Direct(arg1?),
            )),
            // ORL A, @R0
            0x46 => Ok(Instruction::ORL(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R0),
            )),
            // ORL A, @R1
            0x47 => Ok(Instruction::ORL(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R1),
            )),
            // ORL A, Rx
            0x48..=0x4F => Ok(Instruction::ORL(
                AddressingMode::Register(Register::A),
                AddressingMode::Register(register_from_op(opcode)),
            )),
            // JNC reladdr
            0x50 => Ok(Instruction::JNC(arg1? as i8)),
            // ANL iram addr, A
            0x52 => Ok(Instruction::ANL(
                AddressingMode::Direct(arg1?),
                AddressingMode::Register(Register::A),
            )),
            // ANL iram addr, #data
            0x53 => Ok(Instruction::ANL(
                AddressingMode::Direct(arg1?),
                AddressingMode::Immediate(arg2?),
            )),
            // ANL A, #data
            0x54 => Ok(Instruction::ANL(
                AddressingMode::Register(Register::A),
                AddressingMode::Immediate(arg1?),
            )),
            // ANL A, iram addr
            0x55 => Ok(Instruction::ANL(
                AddressingMode::Register(Register::A),
                AddressingMode::Direct(arg1?),
            )),
            // ANL A, @R0
            0x56 => Ok(Instruction::ANL(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R0),
            )),
            // ANL A, @R1
            0x57 => Ok(Instruction::ANL(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R1),
            )),
            // ANL A, @R0
            0x58..=0x5F => Ok(Instruction::ANL(
                AddressingMode::Register(Register::A),
                AddressingMode::Register(register_from_op(opcode)),
            )),
            // JZ
            0x60 => Ok(Instruction::JZ(arg1? as i8)),
            // XRL iram addr, A
            0x62 => Ok(Instruction::XRL(
                AddressingMode::Direct(arg1?),
                AddressingMode::Register(Register::A),
            )),
            // XRL iram addr, #data
            0x63 => Ok(Instruction::XRL(
                AddressingMode::Direct(arg1?),
                AddressingMode::Immediate(arg2?),
            )),
            // XRL A, #data
            0x64 => Ok(Instruction::XRL(
                AddressingMode::Register(Register::A),
                AddressingMode::Immediate(arg1?),
            )),
            // XRL A, iram addr
            0x65 => Ok(Instruction::XRL(
                AddressingMode::Register(Register::A),
                AddressingMode::Direct(arg1?),
            )),
            // XRL A, @R0
            0x66 => Ok(Instruction::XRL(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R0),
            )),
            // XRL A, @R1
            0x67 => Ok(Instruction::XRL(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R1),
            )),
            // XRL A, Rx
            0x68..=0x6F => Ok(Instruction::XRL(
                AddressingMode::Register(Register::A),
                AddressingMode::Register(register_from_op(opcode)),
            )),
            // JNZ
            0x70 => Ok(Instruction::JNZ(arg1? as i8)),
            // ORL C, bit addr
            0x72 => Ok(Instruction::ORL(
                AddressingMode::Register(Register::C),
                AddressingMode::Bit(arg1?),
            )),
            // JMP @A+DPTR
            0x73 => Ok(Instruction::JMP),
            // MOV A, #data
            0x74 => Ok(Instruction::MOV(
                AddressingMode::Register(Register::A),
                AddressingMode::Immediate(arg1?),
            )),
            // MOV bit addr, C
            0x75 => Ok(Instruction::MOV(
                AddressingMode::Direct(arg1?),
                AddressingMode::Immediate(arg2?),
            )),
            // MOV @R0, #data
            0x76 => Ok(Instruction::MOV(
                AddressingMode::Indirect(Register::R0),
                AddressingMode::Immediate(arg1?),
            )),
            // MOV @R1, #data
            0x77 => Ok(Instruction::MOV(
                AddressingMode::Indirect(Register::R1),
                AddressingMode::Immediate(arg1?),
            )),
            // MOV Rx, #data
            0x78..=0x7F => Ok(Instruction::MOV(
                AddressingMode::Register(register_from_op(opcode)),
                AddressingMode::Immediate(arg1?),
            )),
            // SJMP reladdr
            0x80 => Ok(Instruction::SJMP(arg1? as i8)),
            // ANL C, bit addr
            0x82 => Ok(Instruction::ANL(
                AddressingMode::Register(Register::C),
                AddressingMode::Bit(arg1?),
            )),
            // MOVC A, @A+DPTR
            0x83 => Ok(Instruction::MOVC(AddressingMode::IndirectCode(
                Register::PC,
            ))),
            // DIV AB
            0x84 => Ok(Instruction::DIV),
            // MOV iram addr, iram addr
            0x85 => Ok(Instruction::MOV(
                AddressingMode::Direct(arg2?),
                AddressingMode::Direct(arg1?),
            )),
            // MOV iram addr, @R0
            0x86 => Ok(Instruction::MOV(
                AddressingMode::Direct(arg1?),
                AddressingMode::Indirect(Register::R0),
            )),
            // MOV iram addr, @R1
            0x87 => Ok(Instruction::MOV(
                AddressingMode::Direct(arg1?),
                AddressingMode::Indirect(Register::R1),
            )),
            // MOV iram addr, Rx
            0x88..=0x8F => Ok(Instruction::MOV(
                AddressingMode::Direct(arg1?),
                AddressingMode::Register(register_from_op(opcode)),
            )),
            // MOV DPTR, #data16
            0x90 => {
                let pointer = ((arg1? as u16) << 8) | (arg2? as u16);
                Ok(Instruction::LoadDptr(pointer))
            }
            // MOV bit addr, C
            0x92 => Ok(Instruction::MOV(
                AddressingMode::Bit(arg1?),
                AddressingMode::Register(Register::C),
            )),
            // MOVC A, @A+DPTR
            0x93 => Ok(Instruction::MOVC(AddressingMode::IndirectCode(
                Register::DPTR,
            ))),
            // SUBB A, #data
            0x94 => Ok(Instruction::SUBB(AddressingMode::Immediate(arg1?))),
            // SUBB A, iram addr
            0x95 => Ok(Instruction::SUBB(AddressingMode::Direct(arg1?))),
            // SUBB A, @R0
            0x96 => Ok(Instruction::SUBB(AddressingMode::Indirect(Register::R0))),
            // SUBB A, @R0
            0x97 => Ok(Instruction::SUBB(AddressingMode::Indirect(Register::R1))),
            // SUBB A, @R0
            0x98..=0x9F => Ok(Instruction::SUBB(AddressingMode::Register(
                register_from_op(opcode),
            ))),
            // ORL C, /bit addr (C <- C or NOT bit)
            0xA0 => Ok(Instruction::ORL(
                AddressingMode::Register(Register::C),
                AddressingMode::NotBit(arg1?),
            )),
            // MOV C, bit addr
            0xA2 => Ok(Instruction::MOV(
                AddressingMode::Register(Register::C),
                AddressingMode::Bit(arg1?),
            )),
            // INC DPTR
            0xA3 => Ok(Instruction::INC(AddressingMode::Register(Register::DPTR))),
            // MUL AB
            0xA4 => Ok(Instruction::MUL),
            // Undefined instruction
            0xA5 => Err("undefined instruction opcode"),
            // MOV @R0, iram addr
            0xA6 => Ok(Instruction::MOV(
                AddressingMode::Indirect(Register::R0),
                AddressingMode::Direct(arg1?),
            )),
            // MOV @R1, iram addr
            0xA7 => Ok(Instruction::MOV(
                AddressingMode::Indirect(Register::R1),
                AddressingMode::Direct(arg1?),
            )),
            // MOV Rx, iram addr
            0xA8..=0xAF => Ok(Instruction::MOV(
                AddressingMode::Register(register_from_op(opcode)),
                AddressingMode::Direct(arg1?),
            )),
            // ANL C, /bit addr
            0xB0 => Ok(Instruction::ANL(
                AddressingMode::Register(Register::C),
                AddressingMode::NotBit(arg1?),
            )),
            // CPL bit addr,
            0xB2 => Ok(Instruction::CPL(AddressingMode::Bit(arg1?))),
            // CPL C
            0xB3 => Ok(Instruction::CPL(AddressingMode::Register(Register::C))),
            // CJNE A, #data, reladdr
            0xB4 => Ok(Instruction::CJNE(
                AddressingMode::Register(Register::A),
                AddressingMode::Immediate(arg1?),
                arg2? as i8,
            )),
            // CJNE A, iram addr, reladdr
            0xB5 => Ok(Instruction::CJNE(
                AddressingMode::Register(Register::A),
                AddressingMode::Direct(arg1?),
                arg2? as i8,
            )),
            // CJNE @R0, #data, reladdr
            0xB6 => Ok(Instruction::CJNE(
                AddressingMode::Indirect(Register::R0),
                AddressingMode::Immediate(arg1?),
                arg2? as i8,
            )),
            // CJNE @R1, #data, reladdr
            0xB7 => Ok(Instruction::CJNE(
                AddressingMode::Indirect(Register::R1),
                AddressingMode::Immediate(arg1?),
                arg2? as i8,
            )),
            // CJNE Rx, #data, reladdr
            0xB8..=0xBF => Ok(Instruction::CJNE(
                AddressingMode::Register(register_from_op(opcode)),
                AddressingMode::Immediate(arg1?),
                arg2? as i8,
            )),
            // PUSH
            0xC0 => Ok(Instruction::PUSH(AddressingMode::Direct(arg1?))),
            // CLR bit addr
            0xC2 => Ok(Instruction::CLR(AddressingMode::Bit(arg1?))),
            // CLR C
            0xC3 => Ok(Instruction::CLR(AddressingMode::Register(Register::C))),
            // SWAP A
            0xC4 => Ok(Instruction::SWAP),
            // XCH A, iram addr
            0xC5 => Ok(Instruction::XCH(AddressingMode::Direct(arg1?))),
            // XCH A, @R0
            0xC6 => Ok(Instruction::XCH(AddressingMode::Indirect(Register::R0))),
            // XCH A, @R1
            0xC7 => Ok(Instruction::XCH(AddressingMode::Indirect(Register::R1))),
            // XCH A, Rx
            0xC8..=0xCF => Ok(Instruction::XCH(AddressingMode::Register(
                register_from_op(opcode),
            ))),
            // POP
            0xD0 => Ok(Instruction::POP(AddressingMode::Direct(arg1?))),
            // SETB bit addr
            0xD2 => Ok(Instruction::SETB(AddressingMode::Bit(arg1?))),
            // SETB C
            0xD3 => Ok(Instruction::SETB(AddressingMode::Register(Register::C))),
            // DA A
            0xD4 => Ok(Instruction::DA),
            // DJNZ iram addr, reladdr
            0xD5 => Ok(Instruction::DJNZ(
                AddressingMode::Direct(arg1?),
                arg2? as i8,
            )),
            // XCHD A, @R0
            0xD6 => Ok(Instruction::XCHD(AddressingMode::Indirect(Register::R0))),
            // XCHD A, @R1
            0xD7 => Ok(Instruction::XCHD(AddressingMode::Indirect(Register::R1))),
            // DJNZ Rx, reladdr
            0xD8..=0xDF => Ok(Instruction::DJNZ(
                AddressingMode::Register(register_from_op(opcode)),
                arg1? as i8,
            )),
            // MOVX A, @DPTR
            0xE0 => Ok(Instruction::MOVX(
                AddressingMode::Register(Register::A),
                AddressingMode::IndirectExternal(Register::DPTR),
            )),
            // MOVX A, @R0
            0xE2 => Ok(Instruction::MOVX(
                AddressingMode::Register(Register::A),
                AddressingMode::IndirectExternal(Register::R0),
            )),
            // MOVX A, @R1
            0xE3 => Ok(Instruction::MOVX(
                AddressingMode::Register(Register::A),
                AddressingMode::IndirectExternal(Register::R1),
            )),
            // CLR A
            0xE4 => Ok(Instruction::CLR(AddressingMode::Register(Register::A))),
            // MOV A, iram addr
            0xE5 => Ok(Instruction::MOV(
                AddressingMode::Register(Register::A),
                AddressingMode::Direct(arg1?),
            )),
            // MOV A, @R0
            0xE6 => Ok(Instruction::MOV(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R0),
            )),
            // MOV A, @R1
            0xE7 => Ok(Instruction::MOV(
                AddressingMode::Register(Register::A),
                AddressingMode::Indirect(Register::R1),
            )),
            // MOV A, Rx
            0xE8..=0xEF => Ok(Instruction::MOV(
                AddressingMode::Register(Register::A),
                AddressingMode::Register(register_from_op(opcode)),
            )),
            // MOVX @DPTR, A
            0xF0 => Ok(Instruction::MOVX(
                AddressingMode::IndirectExternal(Register::DPTR),
                AddressingMode::Register(Register::A),
            )),
            // MOVX @R0, A
            0xF2 => Ok(Instruction::MOVX(
                AddressingMode::IndirectExternal(Register::R0),
                AddressingMode::Register(Register::A),
            )),
            // MOVX @R1, A
            0xF3 => Ok(Instruction::MOVX(
                AddressingMode::IndirectExternal(Register::R1),
                AddressingMode::Register(Register::A),
            )),
            // CPL A
            0xF4 => Ok(Instruction::CPL(AddressingMode::Register(Register::A))),
            // MOV iram addr, A
            0xF5 => Ok(Instruction::MOV(
                AddressingMode::Direct(arg1?),
                AddressingMode::Register(Register::A),
            )),
            // MOV @R0, A
            0xF6 => Ok(Instruction::MOV(
                AddressingMode::Indirect(Register::R0),
                AddressingMode::Register(Register::A),
            )),
            // MOV @R1, A
            0xF7 => Ok(Instruction::MOV(
                AddressingMode::Indirect(Register::R1),
                AddressingMode::Register(Register::A),
            )),
            // MOV Rx, A
            0xF8..=0xFF => Ok(Instruction::MOV(
                AddressingMode::Register(register_from_op(opcode)),
                AddressingMode::Register(Register::A),
            )),
        }
    }

    // decode the next instruction or interrupt
    fn decode_next_instruction(&mut self) -> Result<Instruction, &'static str> {
        // check if there is an interrupt available
        match Rc::get_mut(&mut self.memory).unwrap().peek_vector() {
            Some((vector, priority)) => {
                // construct priority that we'd accept
                let min_priority = if self.ip1 {
                    2u8
                } else if self.ip0 {
                    1u8
                } else {
                    0u8
                };

                // if the priority is sufficient, execute interrupt
                if priority >= min_priority {
                    Ok(Instruction::Interrupt(vector, priority))
                } else {
                    self.decode_next_opcode()
                }
            }
            None => self.decode_next_opcode()
        }
    }

    // decode length of instruction
    fn decode_instruction_length(&self, instruction: Instruction) -> Result<u16, &'static str> {
        match instruction {
            Instruction::ACALL(_) => Ok(2),
            Instruction::ADD(operand2) => match operand2 {
                AddressingMode::Indirect(_) => Ok(1),
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::ADDC(operand2) => match operand2 {
                AddressingMode::Indirect(_) => Ok(1),
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::AJMP(_) => Ok(2),
            Instruction::ANL(operand1, operand2) => {
                let operand1 = match operand1 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                let operand2 = match operand2 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                Ok(operand1 + operand2 + 1)
            }
            Instruction::CJNE(_, _, _) => Ok(3),
            Instruction::CLR(address) => match address {
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::CPL(address) => match address {
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::DA => Ok(1),
            Instruction::DEC(address) => match address {
                AddressingMode::Indirect(_) => Ok(1),
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::DIV => Ok(1),
            Instruction::DJNZ(address, _) => {
                let address = match address {
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                Ok(address + 2)
            }
            Instruction::INC(address) => match address {
                AddressingMode::Indirect(_) => Ok(1),
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::Interrupt(_, _) => Ok(0),
            Instruction::JB(_, _) => Ok(3),
            Instruction::JBC(_, _) => Ok(3),
            Instruction::JC(_) => Ok(2),
            Instruction::JMP => Ok(1),
            Instruction::JNB(_, _) => Ok(3),
            Instruction::JNC(_) => Ok(2),
            Instruction::JNZ(_) => Ok(2),
            Instruction::JZ(_) => Ok(2),
            Instruction::LCALL(_) => Ok(3),
            Instruction::LJMP(_) => Ok(3),
            Instruction::MOV(operand1, operand2) => {
                let operand1 = match operand1 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                let operand2 = match operand2 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                Ok(operand1 + operand2 + 1)
            }
            Instruction::MOVC(_) => Ok(1),
            Instruction::MOVX(_, _) => Ok(1),
            Instruction::MUL => Ok(1),
            Instruction::NOP => Ok(1),
            Instruction::ORL(operand1, operand2) => {
                let operand1 = match operand1 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                let operand2 = match operand2 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                Ok(operand1 + operand2 + 1)
            }
            Instruction::POP(_) => Ok(2),
            Instruction::PUSH(_) => Ok(2),
            Instruction::RET => Ok(1),
            Instruction::RETI => Ok(1),
            Instruction::RL => Ok(1),
            Instruction::RLC => Ok(1),
            Instruction::RR => Ok(1),
            Instruction::RRC => Ok(1),
            Instruction::SETB(_) => Ok(2),
            Instruction::SJMP(_) => Ok(2),
            Instruction::SUBB(operand2) => match operand2 {
                AddressingMode::Indirect(_) => Ok(1),
                AddressingMode::Register(_) => Ok(1),
                _ => Ok(2),
            },
            Instruction::SWAP => Ok(1),
            Instruction::XCH(operand2) => {
                let operand2 = match operand2 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                Ok(operand2 + 1)
            }
            Instruction::XCHD(_) => Ok(1),
            Instruction::XRL(operand1, operand2) => {
                let operand1 = match operand1 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                let operand2 = match operand2 {
                    AddressingMode::Indirect(_) => 0,
                    AddressingMode::Register(_) => 0,
                    _ => 1,
                };
                Ok(operand1 + operand2 + 1)
            }
            Instruction::LoadDptr(_) => Ok(3),
        }
    }

    // execute an instruction
    pub fn execute_instruction(&mut self, instruction: Instruction) -> Result<(), &'static str> {
        let length = self.decode_instruction_length(instruction)?;
        let mut next_program_counter = self.program_counter + length;
        println!("{:04x}: {:?}", self.program_counter, instruction);

        let result = match instruction {
            Instruction::ACALL(address) => {
                if self.stack_pointer >= 127 {
                    panic!("stack overflow in ACALL");
                }
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 1),
                    next_program_counter.to_le_bytes()[0],
                )?;
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 2),
                    next_program_counter.to_le_bytes()[1],
                )?;
                self.stack_pointer = self.stack_pointer + 2;
                next_program_counter = (self.program_counter & 0xF800) | address;
                Ok(())
            }
            Instruction::ADD(operand2) => {
                let data = self.load(operand2)?;
                let result = (self.accumulator as u16) + (data as u16);
                let half_result = (self.accumulator & 0xf) + (data & 0xf);
                let signed_result = (self.accumulator & 0x7f) + (data & 0x7f);
                self.accumulator = result as u8;

                // flags
                self.flags.set(Flags::CARRY, result > 255);
                self.flags.set(Flags::AUXILIARYCARRY, half_result > 16);
                self.flags.set(
                    Flags::OVERFLOW,
                    self.flags.contains(Flags::CARRY) ^ (signed_result > 127),
                );
                Ok(())
            }
            Instruction::ADDC(operand2) => {
                let data = self.load(operand2)?;
                let result =
                    (self.accumulator as u16) + (data as u16) + (self.flags.carry() as u16);
                let half_result = (self.accumulator & 0xf) + (data & 0xf) + self.flags.carry();
                let signed_result = (self.accumulator & 0x7f) + (data & 0x7f) + self.flags.carry();
                self.accumulator = result as u8;

                // flags
                self.flags.set(Flags::CARRY, result > 255);
                self.flags.set(Flags::AUXILIARYCARRY, half_result > 16);
                self.flags.set(
                    Flags::OVERFLOW,
                    self.flags.contains(Flags::CARRY) ^ (signed_result > 127),
                );
                Ok(())
            }
            Instruction::AJMP(address) => {
                next_program_counter = (self.program_counter & 0xF800) | address;
                Ok(())
            }
            Instruction::ANL(operand1, operand2) => {
                let data = self.load(operand1)? & self.load(operand2)?;
                self.store(operand1, data)
            }
            Instruction::CJNE(operand1, operand2, offset) => {
                let operand1 = self.load(operand1)?;
                let operand2 = self.load(operand2)?;
                self.flags.set(Flags::CARRY, operand1 < operand2);
                if operand1 != operand2 {
                    next_program_counter = ((next_program_counter as i16) + (offset as i16)) as u16;
                }
                Ok(())
            }
            Instruction::CLR(address) => self.store(address, 0),
            Instruction::CPL(address) => {
                let data = self.load(address)?;
                self.store(address, !data)
            }
            Instruction::DA => {
                let mut result = self.accumulator as u16;
                if ((result & 0xf) > 9) || self.flags.contains(Flags::AUXILIARYCARRY) {
                    result = result + 0x06;
                }
                if result > 255 {
                    self.flags.insert(Flags::CARRY);
                }
                if (((result >> 4) & 0xf) > 9) || self.flags.contains(Flags::CARRY) {
                    result = result + 0x60;
                }
                if result > 255 {
                    self.flags.insert(Flags::CARRY);
                }
                self.accumulator = result as u8;
                Ok(())
            }
            Instruction::DEC(address) => {
                let data = self.load(address)?;
                self.store(address, data - 1)
            }
            Instruction::DIV => {
                self.flags.set(Flags::OVERFLOW, self.b == 0);
                self.flags.remove(Flags::CARRY);
                if self.b != 0 {
                    let quotient = self.accumulator / self.b;
                    let remainder = self.accumulator % self.b;
                    self.accumulator = quotient;
                    self.b = remainder;
                }
                Ok(())
            }
            Instruction::DJNZ(address, offset) => {
                let mut data = self.load(address)?;
                data = data - 1;
                self.store(address, data)?;
                if data != 0 {
                    next_program_counter = ((next_program_counter as i16) + (offset as i16)) as u16;
                }
                Ok(())
            }
            Instruction::INC(address) => {
                if let AddressingMode::Register(Register::DPTR) = address {
                    self.data_pointer = self.data_pointer + 1;
                    Ok(())
                } else {
                    let data = self.load(address)?;
                    self.store(address, data + 1)
                }
            }
            Instruction::Interrupt(address, priority) => {
                if self.stack_pointer >= 127 {
                    panic!("stack overflow in Interrupt");
                }
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 1),
                    next_program_counter.to_le_bytes()[0],
                )?;
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 2),
                    next_program_counter.to_le_bytes()[1],
                )?;
                self.stack_pointer = self.stack_pointer + 2;
                next_program_counter = address;
                match priority {
                    0 => self.ip0 = true,
                    1 => self.ip1 = true,
                    _ => panic!("unsupported priority"),
                }
                mem.pop_vector();
                Ok(())
            }
            Instruction::JB(bit, address) => {
                let data = self.load(bit)?;
                if data != 0 {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::JBC(bit, address) => {
                let data = self.load(bit)?;
                if data != 0 {
                    self.store(bit, 0)?;
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::JC(address) => {
                if self.flags.contains(Flags::CARRY) {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::JMP => {
                next_program_counter = self.data_pointer + self.accumulator as u16;
                Ok(())
            }
            Instruction::JNB(bit, address) => {
                let data = self.load(bit)?;
                if data == 0 {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::JNC(address) => {
                if !self.flags.contains(Flags::CARRY) {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::JNZ(address) => {
                if self.accumulator != 0 {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::JZ(address) => {
                if self.accumulator == 0 {
                    next_program_counter =
                        ((next_program_counter as i16) + (address as i16)) as u16;
                }
                Ok(())
            }
            Instruction::LCALL(address) => {
                if self.stack_pointer >= 127 {
                    panic!("stack overflow in LCALL");
                }
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 1),
                    next_program_counter.to_le_bytes()[0],
                )?;
                mem.write_memory(
                    Address::InternalData(self.stack_pointer + 2),
                    next_program_counter.to_le_bytes()[1],
                )?;
                self.stack_pointer = self.stack_pointer + 2;
                next_program_counter = address;
                Ok(())
            }
            Instruction::LJMP(address) => {
                next_program_counter = address;
                Ok(())
            }
            Instruction::MOV(operand1, operand2) => {
                let data = self.load(operand2)?;
                self.store(operand1, data)
            }
            Instruction::MOVC(operand) => {
                self.accumulator = self.load(operand)?;
                Ok(())
            }
            Instruction::MOVX(operand1, operand2) => {
                let data = self.load(operand2)?;
                self.store(operand1, data)
            }
            Instruction::MUL => {
                let result = (self.accumulator as u16) * (self.b as u16);
                self.accumulator = result.to_le_bytes()[0];
                self.b = result.to_le_bytes()[1];
                self.flags.set(Flags::OVERFLOW, self.b != 0);
                self.flags.remove(Flags::CARRY);
                Ok(())
            }
            Instruction::NOP => Ok(()),
            Instruction::ORL(operand1, operand2) => {
                let data = self.load(operand1)? | self.load(operand2)?;
                self.store(operand1, data)
            }
            Instruction::POP(address) => {
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                let data = mem.read_memory(Address::InternalData(self.stack_pointer))?;
                self.stack_pointer = self.stack_pointer - 1;
                self.store(address, data)
            }
            Instruction::PUSH(address) => {
                if self.stack_pointer >= 127 {
                    panic!("stack overflow in PUSH");
                }
                let data = self.load(address)?;
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                mem.write_memory(Address::InternalData(self.stack_pointer + 1), data)?;
                self.stack_pointer = self.stack_pointer + 1;
                Ok(())
            }
            Instruction::RET => {
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                next_program_counter = u16::from_le_bytes([
                    mem.read_memory(Address::InternalData(self.stack_pointer - 1))?,
                    mem.read_memory(Address::InternalData(self.stack_pointer))?,
                ]);
                self.stack_pointer = self.stack_pointer - 2;
                Ok(())
            }
            Instruction::RETI => {
                let mem = Rc::get_mut(&mut self.memory).unwrap();
                next_program_counter = u16::from_le_bytes([
                    mem.read_memory(Address::InternalData(self.stack_pointer - 1))?,
                    mem.read_memory(Address::InternalData(self.stack_pointer))?,
                ]);
                self.stack_pointer = self.stack_pointer - 2;
                if self.ip1 == true {
                    self.ip1 = false;
                } else if self.ip0 == true {
                    self.ip0 = false;
                }
                Ok(())
            }
            Instruction::RL => {
                self.accumulator = self.accumulator.rotate_left(1);
                Ok(())
            }
            Instruction::RLC => {
                let a = self.accumulator;
                self.accumulator = ((self.accumulator << 1) & 0xfe) | self.flags.carry();
                self.flags.set(Flags::CARRY, ((a >> 7) & 0x01) != 0);
                Ok(())
            }
            Instruction::RR => {
                self.accumulator = self.accumulator.rotate_right(1);
                Ok(())
            }
            Instruction::RRC => {
                let a = self.accumulator;
                self.accumulator =
                    ((self.accumulator >> 1) & 0x7f) | ((self.flags.carry() << 7) & 0x80);
                self.flags.set(Flags::CARRY, (a & 0x01) != 0);
                Ok(())
            }
            Instruction::SETB(address) => self.store(address, 1),
            Instruction::SJMP(offset) => {
                next_program_counter = ((next_program_counter as i16) + (offset as i16)) as u16;
                Ok(())
            }
            Instruction::SUBB(operand2) => {
                let data = self.load(operand2)?;
                let result =
                    (self.accumulator as u16) - (data as u16) - (self.flags.carry() as u16);
                // flags
                self.flags.set(
                    Flags::AUXILIARYCARRY,
                    ((data & 0xf) + self.flags.carry()) > (self.accumulator & 0xf),
                );
                self.flags
                    .set(Flags::CARRY, (data + self.flags.carry()) > self.accumulator);
                self.flags.set(
                    Flags::OVERFLOW,
                    ((result as i16) > 127) || ((result as i16) < -128),
                );
                self.accumulator = result as u8;
                Ok(())
            }
            Instruction::SWAP => {
                self.accumulator =
                    ((self.accumulator >> 4) & 0x0f) | ((self.accumulator << 4) & 0xf0);
                Ok(())
            }
            Instruction::XCH(operand2) => {
                let data = self.accumulator;
                self.accumulator = self.load(operand2)?;
                self.store(operand2, data)
            }
            Instruction::XCHD(operand2) => {
                let a = self.accumulator;
                let i = self.load(operand2)?;
                self.accumulator = (a & 0xf0) | (i & 0x03);
                self.store(operand2, (i & 0xf0) | (a & 0x03))
            }
            Instruction::XRL(operand1, operand2) => {
                let data = self.load(operand1)? ^ self.load(operand2)?;
                self.store(operand1, data)
            }
            Instruction::LoadDptr(a) => {
                self.data_pointer = a;
                Ok(())
            }
        };
        self.program_counter = next_program_counter;
        self.flags
            .set(Flags::PARITY, self.accumulator.count_ones() & 1 == 1);
        result
    }

    pub fn step(&mut self) -> Result<(), &'static str> {
        let instruction = self.decode_next_instruction()?;
        self.execute_instruction(instruction)?;
        Rc::get_mut(&mut self.memory).unwrap().tick();
        Ok(())
    }
}
