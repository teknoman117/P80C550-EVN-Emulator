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
                        let result = match self.register_b {
                            0 => {
                                /* status register (set tx empty) */
                                Ok(0x04)
                            }
                            _ => {
                                println!("implemented read register {:?}", self.register_b);
                                Ok(0x00)
                            }
                        };
                        self.register_b = 0;
                        result
                    }
                    1 => {
                        println!("am85c30.channel.b.data");
                        Ok(0x00)
                    }
                    2 => {
                        println!("am85c30.channel.a.control");
                        self.register_a = 0;
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
                        let mut next_register = 0;
                        match self.register_b {
                            0 => {
                                let mut register_select = data & 0x07;
                                let action = (data >> 3) & 0x07;
                                let reset = (data >> 6) & 0x03;
                                match action {
                                    0 => {
                                        /* null code */
                                    }
                                    1 => {
                                        register_select = register_select + 8;
                                    }
                                    2 => {
                                        /* reset ext/status interrupts */
                                    }
                                    3 => {
                                        /* send abort */
                                    }
                                    4 => {
                                        /* enable interrupt on next rx character */
                                    }
                                    5 => {
                                        /* reset tx interrupt pending */
                                    }
                                    6 => {
                                        /* reset error */
                                    }
                                    7 => {
                                        /* reset highest IUS (??) */
                                    }
                                    _ => panic!("impossible")
                                }
                                match reset {
                                    0 => {
                                        /* null code */
                                    }
                                    1 => {
                                        /* reset rx CRC checker */
                                    }
                                    2 => {
                                        /* reset tx CRC generator */
                                    }
                                    3 => {
                                        /* reset tx underrun / end of message latch */
                                    }
                                    _ => panic!("impossible")
                                }
                                next_register = register_select;
                            }
                            _ => {
                                println!("unimplemented write register {:?} = {:x}", self.register_b, data);
                            }
                        }
                        self.register_b = next_register;
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