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

pub trait Memory {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str>;
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str>;
}

pub mod cpu;
