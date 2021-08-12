use std::fs;
use std::path::Path;
use std::rc::Rc;

mod mcs51;
use mcs51::{Address, Memory, CPU};

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
                            self.tcon |= 1 << (bit & 0x7);
                        } else {
                            self.tcon &= !(1 << (bit & 0x07));
                        }
                        Ok(())
                    }
                    0xA8..=0xAF => {
                        if data != 0 {
                            self.ie |= 1 << (bit & 0x7);
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
                            Ok(0xFF)
                        }
                        0x8401 => {
                            println!("spi.control read");
                            Ok(0x80)
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
    let mapper = Rc::new(P80C550::new(rom, iram, peripherals));

    // create the cpu
    let mut cpu = CPU::new(mapper);
    loop {
        cpu.step()?;
    }

    Ok(())
}
