use crate::mcs51::cpu::{Address, CPU};
use crate::mcs51::memory::{Memory, RAM};
use crate::mcs51::{assign_bit, get_bit};

use std::rc::Rc;

pub struct Peripherals<A: Memory, B: Memory> {
    rom: Rc<A>,
    xram: Rc<B>,
    iram: RAM,

    // 8051 peripherals
    tcon: u8,
    tmod: u8,
    tl0: u8,
    tl1: u8,
    th0: u8,
    th1: u8,
    ie: u8,

    // 8051 io ports
    port0: u8,
    port1: u8,
    port2: u8,
    port3: u8,
}

impl<A: Memory, B: Memory> Peripherals<A, B> {
    pub fn new(rom: Rc<A>, xram: Rc<B>) -> Peripherals<A, B> {
        Peripherals {
            rom: rom,
            iram: RAM::create_with_size(128),
            xram: xram,
            tcon: 0,
            tmod: 0,
            tl0: 0,
            tl1: 0,
            th0: 0,
            th1: 0,
            ie: 0,
            port0: 0xff,
            port1: 0xff,
            port2: 0xff,
            port3: 0xff,
        }
    }
}

impl<A: Memory, B: Memory> Memory for Peripherals<A, B> {
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
                    0x88..=0x8F => Ok(get_bit(self.tcon, bit & 7)),
                    0x90..=0x97 => Ok(get_bit(self.port1, bit & 7)),
                    0xA0..=0xA7 => Ok(get_bit(self.port2, bit & 7)),
                    0xA8..=0xAF => Ok(get_bit(self.ie, bit & 7)),
                    0xB0..=0xB7 => Ok(get_bit(self.port3, bit & 7)),
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x80 => Ok(self.port0),
                0x88 => Ok(self.tcon),
                0x89 => Ok(self.tmod),
                0x8A => Ok(self.tl0),
                0x8B => Ok(self.tl1),
                0x8C => Ok(self.th0),
                0x8D => Ok(self.th1),
                0x90 => Ok(self.port1),
                0xA0 => Ok(self.port2),
                0xA8 => Ok(self.ie),
                0xB0 => Ok(self.port3),
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
                        self.port0 = assign_bit(self.port0, bit & 7, data);
                        Ok(())
                    }
                    0x88..=0x8F => {
                        self.tcon = assign_bit(self.tcon, bit & 7, data);
                        Ok(())
                    }
                    0x90..=0x97 => {
                        self.port1 = assign_bit(self.port1, bit & 7, data);
                        Ok(())
                    }
                    0xA0..=0xA7 => {
                        self.port2 = assign_bit(self.port2, bit & 7, data);
                        Ok(())
                    }
                    0xA8..=0xAF => {
                        self.ie = assign_bit(self.ie, bit & 7, data);
                        Ok(())
                    }
                    0xB0..=0xB7 => {
                        self.port3 = assign_bit(self.port3, bit & 7, data);
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
                0x90 => {
                    self.port1 = data;
                    Ok(())
                }
                0xa0 => {
                    self.port2 = data;
                    Ok(())
                }
                0xa8 => {
                    self.ie = data;
                    Ok(())
                }
                0xb0 => {
                    self.port3 = data;
                    Ok(())
                }
                _ => Err("non-existant SFR"),
            },
            _ => Err("unsupported addressing mode for memory mapper (write)"),
        }
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
