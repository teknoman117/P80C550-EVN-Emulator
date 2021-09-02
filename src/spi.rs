use crate::mcs51::cpu::Address;
use crate::mcs51::memory::Memory;

pub struct SPI {

}

impl SPI {
    pub fn new() -> SPI {
        SPI {

        }
    }
}

impl Memory for SPI {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::ExternalData(a) => {
                match a & 1 {
                    0 => {
                        println!("spi.data read");
                        Ok(0xFF)
                    }
                    1 => {
                        println!("spi.control read");
                        Ok(0x80)
                    }
                    _ => panic!("impossible register"),
                }
            }
            _ => Err("unsupported address space"),
        }
    }
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::ExternalData(a) => {
                match a & 1 {
                    0 => {
                        println!("spi.data write = {:x}", data);
                        Ok(())
                    }
                    1 => {
                        println!("spi.control write = {:x}", data);
                        Ok(())
                    }
                    _ => panic!("impossible register"),
                }
            }
            _ => Err("unsupported address space"),
        }
    }

    fn tick(&mut self) {}
}