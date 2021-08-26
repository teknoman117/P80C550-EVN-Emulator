use crate::mcs51::cpu::Address;
use crate::mcs51::memory::Memory;

pub struct ESCC {
    register_a: u8,
    register_b: u8,
}

impl ESCC {
    pub fn new() -> ESCC {
        ESCC {
            register_a: 0,
            register_b: 0,
        }
    }
}

impl Memory for ESCC {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::ExternalData(a) => {
                let address = a & 3;
                match address {
                    0 => {
                        println!("am85c30.channel.b.control");
                        Ok(0x00)
                    }
                    1 => {
                        println!("am85c30.channel.b.data");
                        Ok(0x00)
                    }
                    2 => {
                        println!("am85c30.channel.a.control");
                        Ok(0x00)
                    }
                    3 => {
                        println!("am85c30.channel.a.data");
                        Ok(0x00)
                    }
                    _ => Err("unused address (read)"),
                }
            }
            _ => Err("unsupported address space"),
        }
    }
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::ExternalData(a) => {
                let address = a & 3;
                match address {
                    0 => {
                        // uart channel b control
                        println!("am85c30.channel.b.control = {:x}", data);
                        Ok(())
                    }
                    1 => {
                        // uart channel b data
                        println!("am85c30.channel.b.data = {:x}", data);
                        Ok(())
                    }
                    2 => {
                        // uart channel a control
                        println!("am85c30.channel.a.control = {:x}", data);
                        Ok(())
                    }
                    3 => {
                        // uart channel a data
                        println!("am85c30.channel.a.data = {:x}", data);
                        Ok(())
                    }
                    _ => Err("unused address (write)"),
                }
            }
            _ => Err("unsupported address space"),
        }
    }

    fn tick(&mut self) {}
}