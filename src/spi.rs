use crate::mcs51::cpu::Address;
use crate::mcs51::memory::Memory;

use bitflags::bitflags;

use std::rc::Rc;

pub trait Device {
    fn transfer(&mut self, data: u8) -> u8;
}

pub struct NullDevice {}

impl NullDevice {
    pub fn new() -> NullDevice {
        NullDevice {}
    }
}

impl Device for NullDevice {
    fn transfer(&mut self, _data: u8) -> u8 {
        0xff
    }
}

bitflags! {
    struct Control: u8 {
        const PRESCALER0       = 0b00000001;
        const PRESCALER1       = 0b00000010;
        const SLAVE_SELECT0    = 0b00000100;
        const SLAVE_SELECT1    = 0b00001000;
        const BUSY             = 0b00100000;
        const INTERRUPT_ENABLE = 0b01000000;
        const INTERRUPT_FLAG   = 0b10000000;
        const ASSIGNABLE       = 0b01101111;
    }
}

pub enum Prescaler {
    Clk1_2,
    Clk1_8,
    Clk1_32,
    Clk1_128,
}

impl Control {
    fn prescaler(&self) -> Prescaler {
        match self.bits & 3 {
            0 => Prescaler::Clk1_2,
            1 => Prescaler::Clk1_8,
            2 => Prescaler::Clk1_32,
            3 => Prescaler::Clk1_128,
            _ => panic!("impossible value"),
        }
    }

    fn slave(&self) -> Option<u8> {
        let value = (self.bits >> 2) & 3;
        if value > 0 {
            Some(value - 1)
        } else {
            None
        }
    }
}

pub struct SPI {
    buffer: u8,
    buffer_ttl: u8,
    control: Control,
    devices: [Rc<dyn Device>; 3],
}

impl SPI {
    pub fn new(devices: [Rc<dyn Device>; 3]) -> SPI {
        SPI {
            buffer: 0,
            buffer_ttl: 0,
            control: Control::empty(),
            devices: devices,
        }
    }
}

impl Memory for SPI {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::ExternalData(a) => match a & 1 {
                // SPI Data Register
                0 => {
                    // clear interrupt flag
                    self.control.remove(Control::INTERRUPT_FLAG);

                    // if the ttl hasn't reached, throw an error. undefined behavior on real hardware
                    if self.buffer_ttl > 1 {
                        Err("buffer is not ready")
                    } else {
                        Ok(self.buffer)
                    }
                }
                // SPI Control Register
                1 => Ok(self.control.bits),
                _ => panic!("impossible register"),
            },
            _ => Err("unsupported address space"),
        }
    }
    fn write_memory(&mut self, address: Address, data: u8) -> Result<(), &'static str> {
        match address {
            Address::ExternalData(a) => match a & 1 {
                // SPI Data Register
                0 => {
                    self.buffer = match self.control.slave() {
                        Some(id) => Rc::get_mut(&mut self.devices[id as usize])
                            .unwrap()
                            .transfer(data),
                        None => 0xff,
                    };
                    // machine cycles until buffer is ready
                    // = ceil((8 bits * clocks per bit) / 12 clock per machine cycle)
                    self.buffer_ttl = match self.control.prescaler() {
                        Prescaler::Clk1_2 => 2,
                        Prescaler::Clk1_8 => 6,
                        Prescaler::Clk1_32 => 22,
                        Prescaler::Clk1_128 => 86,
                    };
                    // clear interrupt flag
                    self.control.remove(Control::INTERRUPT_FLAG);
                    Ok(())
                }
                // SPI Control Register
                1 => {
                    // copy assignable bits to control register, clear interrupt flag if requested
                    let flags = Control::from_bits_truncate(data);
                    let assignable = flags.intersection(Control::ASSIGNABLE);
                    self.control = if flags.contains(Control::INTERRUPT_FLAG) {
                        assignable
                    } else {
                        assignable | self.control.intersection(Control::INTERRUPT_FLAG)
                    };
                    Ok(())
                }
                _ => panic!("impossible register"),
            },
            _ => Err("unsupported address space"),
        }
    }

    fn tick(&mut self) {
        self.buffer_ttl = match self.buffer_ttl.checked_sub(1) {
            Some(ttl) => ttl,
            None => 0,
        }
    }

    // TODO: interrupt assertion system
}
