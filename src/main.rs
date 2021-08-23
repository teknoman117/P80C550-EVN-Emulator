use std::path::Path;
use std::rc::Rc;

mod mcs51;
use mcs51::cpu::Address;
use mcs51::memory::{Memory, RAM, ROM};
use mcs51::soc::p80c550;

struct Peripherals {
    ram: RAM,
}

impl Peripherals {
    fn new() -> Peripherals {
        Peripherals {
            ram: RAM::create_with_size(32768),
        }
    }
}

impl Memory for Peripherals {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::ExternalData(a) => {
                if a < 0x8000 {
                    self.ram.read_memory(address)
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
                        0x9400 => {
                            println!("am85c30.channel.b.control");
                            Ok(0x00)
                        }
                        0x9401 => {
                            println!("am85c30.channel.b.data");
                            Ok(0x00)
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
                    self.ram.write_memory(address, data)
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
                        0x9401 => {
                            // uart channel b data
                            println!("am85c30.channel.b.data = {:x}", data);
                            Ok(())
                        }
                        _ => Err("unused address (write)"),
                    }
                }
            }
            _ => Err("unsupported address space"),
        }
    }

    fn tick(&mut self) {}
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    // load the application rom
    let rom_path = Path::new("rom.bin");
    let rom = Rc::new(ROM::load_from_binary(rom_path)?);

    // create board specific peripherals
    let peripherals = Rc::new(Peripherals::new());

    // create the cpu
    let mut cpu = p80c550::create(rom, peripherals);

    // run 1 second at 11.0592 MHz
    for _ in 1..921600 {
        cpu.step()?;
    }

    Ok(())
}
