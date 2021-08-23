use crate::mcs51::cpu::{Address, InterruptSource, CPU};
use crate::mcs51::memory::{Memory, RAM};
use crate::mcs51::peripherals::timer::Timer;
use crate::mcs51::{get_bit, set_bit};

use bitflags::bitflags;

use std::rc::Rc;

bitflags! {
    struct IE: u8 {
        const EX0 = 0b00000001;
        const ET0 = 0b00000010;
        const EX1 = 0b00000100;
        const ET1 = 0b00001000;
        const ES  = 0b00010000;
        const EAD = 0b00100000;
        const EWD = 0b01000000;
        const EA  = 0b10000000;
    }

    struct IP: u8 {
        const PX0 = 0b00000001;
        const PT0 = 0b00000010;
        const PX1 = 0b00000100;
        const PT1 = 0b00001000;
        const PS  = 0b00010000;
        const PAD = 0b00100000;
        const PWD = 0b01000000;
    }

    struct PCON: u8 {
        const IDL  = 0b00000001;
        const PD   = 0b00000010;
        const GF0  = 0b00000100;
        const GF1  = 0b00001000;
        const SIDL = 0b01000000;
        const SMOD = 0b10000000;
    }
}

pub struct Peripherals<A, B>
where
    A: Memory,
    B: Memory,
{
    rom: Rc<A>,
    xram: Rc<B>,
    iram: RAM,

    // 8051 peripherals
    timer: Timer,

    // 8051 io ports
    port0: u8,
    port1: u8,
    port2: u8,
    port3: u8,

    // 8051 interrupts
    ie: IE,
    ip: IP,
    pcon: PCON,
}

impl<A, B> Peripherals<A, B>
where
    A: Memory,
    B: Memory,
{
    pub fn new(rom: Rc<A>, xram: Rc<B>) -> Peripherals<A, B> {
        Peripherals {
            rom: rom,
            iram: RAM::create_with_size(128),
            xram: xram,
            timer: Timer::new(),
            port0: 0xff,
            port1: 0xff,
            port2: 0xff,
            port3: 0xff,
            ie: IE::empty(),
            ip: IP::empty(),
            pcon: PCON::empty(),
        }
    }
}

impl<A, B> Memory for Peripherals<A, B>
where
    A: Memory,
    B: Memory,
{
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::Code(a) => Rc::get_mut(&mut self.rom)
                .unwrap()
                .read_memory(Address::ExternalData(a)),
            Address::InternalData(a) => self.iram.read_memory(Address::InternalData(a)),
            Address::ExternalData(a) => Rc::get_mut(&mut self.xram)
                .unwrap()
                .read_memory(Address::ExternalData(a)),
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x80..=0x87 => Ok(get_bit(self.port0, bit & 7)),
                    0x88..=0x8F => self.timer.read_memory(address),
                    0x90..=0x97 => Ok(get_bit(self.port1, bit & 7)),
                    0xA0..=0xA7 => Ok(get_bit(self.port2, bit & 7)),
                    0xA8..=0xAF => {
                        let flag = IE::from_bits(1 << (bit & 7)).unwrap();
                        if self.ie.contains(flag) {
                            Ok(1)
                        } else {
                            Ok(0)
                        }
                    }
                    0xB0..=0xB7 => Ok(get_bit(self.port3, bit & 7)),
                    0xB8..=0xBF => {
                        let flag = IP::from_bits(1 << (bit & 7)).unwrap();
                        if self.ip.contains(flag) {
                            Ok(1)
                        } else {
                            Ok(0)
                        }
                    }
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x80 => Ok(self.port0),
                0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D => self.timer.read_memory(address),
                0x90 => Ok(self.port1),
                0xA0 => Ok(self.port2),
                0xA8 => Ok(self.ie.bits),
                0xB0 => Ok(self.port3),
                0xB7 => Ok(self.pcon.bits),
                0xB8 => Ok(self.ip.bits),
                _ => Err("non-existant SFR"),
            },
        }
    }
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::InternalData(a) => self.iram.write_memory(Address::InternalData(a), data),
            Address::ExternalData(a) => Rc::get_mut(&mut self.xram)
                .unwrap()
                .write_memory(Address::ExternalData(a), data),
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x80..=0x87 => {
                        self.port0 = set_bit(self.port0, bit & 7, data != 0);
                        Ok(())
                    }
                    0x88..=0x8F => self.timer.write_memory(address, data),
                    0x90..=0x97 => {
                        self.port1 = set_bit(self.port1, bit & 7, data != 0);
                        Ok(())
                    }
                    0xA0..=0xA7 => {
                        self.port2 = set_bit(self.port2, bit & 7, data != 0);
                        Ok(())
                    }
                    0xA8..=0xAF => {
                        let flag = IE::from_bits(1 << (bit & 7)).unwrap();
                        self.ie.set(flag, data != 0);
                        Ok(())
                    }
                    0xB0..=0xB7 => {
                        self.port3 = set_bit(self.port3, bit & 7, data != 0);
                        Ok(())
                    }
                    0xB8..=0xBF => {
                        let flag = IP::from_bits(1 << (bit & 7)).unwrap();
                        self.ip.set(flag, data != 0);
                        Ok(())
                    }
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x80 => {
                    self.port0 = data;
                    Ok(())
                }
                0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D => self.timer.write_memory(address, data),
                0x90 => {
                    self.port1 = data;
                    Ok(())
                }
                0xA0 => {
                    self.port2 = data;
                    Ok(())
                }
                0xA8 => {
                    self.ie.bits = data;
                    Ok(())
                }
                0xB0 => {
                    self.port3 = data;
                    Ok(())
                }
                0xB7 => {
                    self.pcon.bits = data;
                    Ok(())
                }
                0xB8 => {
                    self.ip.bits = data;
                    Ok(())
                }
                _ => Err("non-existant SFR"),
            },
            _ => Err("unsupported addressing mode for memory mapper (write)"),
        }
    }

    // tick updates peripherals
    fn tick(&mut self) {
        Rc::get_mut(&mut self.rom).unwrap().tick();
        Rc::get_mut(&mut self.xram).unwrap().tick();
        self.iram.tick();
        self.timer.tick();
    }
}

impl<A, B> InterruptSource for Peripherals<A, B>
where
    A: Memory,
    B: Memory,
{
    fn peek_vector(&mut self) -> Option<(u8, u8)> {
        if self.ie.contains(IE::EA) {
            None
        } else {
            None
        }
    }

    fn pop_vector(&mut self) {

    }
}

pub fn create<A, B>(rom: Rc<A>, xram: Rc<B>) -> CPU<Peripherals<A, B>>
where
    A: Memory,
    B: Memory,
{
    let soc = Rc::new(Peripherals::new(rom, xram));
    CPU::new(soc)
}
