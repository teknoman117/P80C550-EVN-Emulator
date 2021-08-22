use crate::mcs51::cpu::{Address, CPU};
use crate::mcs51::memory::{Memory, RAM};
use crate::mcs51::peripherals::timer::Timer;
use crate::mcs51::{get_bit, set_bit};

use std::rc::Rc;

pub struct Peripherals<A: Memory, B: Memory> {
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
}

impl<A: Memory, B: Memory> Peripherals<A, B> {
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
                    0x90..=0x97 => Ok(get_bit(self.port1, bit & 7)),
                    0xA0..=0xA7 => Ok(get_bit(self.port2, bit & 7)),
                    0xB0..=0xB7 => Ok(get_bit(self.port3, bit & 7)),
                    0x88..=0x8F | 0xA8..=0xAF => self.timer.read_memory(address),
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x80 => Ok(self.port0),
                0x90 => Ok(self.port1),
                0xA0 => Ok(self.port2),
                0xB0 => Ok(self.port3),
                0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0xA8 => self.timer.read_memory(address),
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
                    0x90..=0x97 => {
                        self.port1 = set_bit(self.port1, bit & 7, data != 0);
                        Ok(())
                    }
                    0xA0..=0xA7 => {
                        self.port2 = set_bit(self.port2, bit & 7, data != 0);
                        Ok(())
                    }
                    0xB0..=0xB7 => {
                        self.port3 = set_bit(self.port3, bit & 7, data != 0);
                        Ok(())
                    }
                    0x88..=0x8F | 0xA8..=0xAF => self.timer.write_memory(address, data),
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x80 => {
                    self.port0 = data;
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
                0xb0 => {
                    self.port3 = data;
                    Ok(())
                }
                0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0xA8 => {
                    self.timer.write_memory(address, data)
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

pub fn create<A, B>(rom: Rc<A>, xram: Rc<B>) -> CPU<Peripherals<A, B>>
where
    A: Memory,
    B: Memory,
{
    let soc = Rc::new(Peripherals::new(rom, xram));
    CPU::new(soc)
}
