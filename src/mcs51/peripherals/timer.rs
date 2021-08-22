use crate::mcs51::cpu::Address;
use crate::mcs51::memory::Memory;
use crate::mcs51::{get_bit, set_bit};

pub struct Timer {
    tcon: u8,
    tmod: u8,
    tl0: u8,
    tl1: u8,
    th0: u8,
    th1: u8,
    ie: u8,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
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

impl Memory for Timer {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x88..=0x8F => Ok(get_bit(self.tcon, bit & 7)),
                    0xA8..=0xAF => Ok(get_bit(self.ie, bit & 7)),
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
            _ => Err("unsupported addressing mode for timer"),
        }
    }

    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x88..=0x8F => {
                        self.tcon = set_bit(self.tcon, bit & 7, data != 0);
                        Ok(())
                    }
                    0xA8..=0xAF => {
                        self.ie = set_bit(self.ie, bit & 7, data != 0);
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
            _ => Err("unsupported addressing mode for timer"),
        }
    }

    fn tick(&mut self) {}
}
