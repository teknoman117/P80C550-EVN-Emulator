use crate::mcs51::cpu::Address;
use crate::mcs51::memory::Memory;
use crate::mcs51::{get_bit, set_bit};

use bitflags::bitflags;

bitflags! {
    struct TCON: u8 {
        const IT0 = 0b00000001;
        const IE0 = 0b00000010;
        const IT1 = 0b00000100;
        const IE1 = 0b00001000;
        const TR0 = 0b00010000;
        const TF0 = 0b00100000;
        const TR1 = 0b01000000;
        const TF1 = 0b10000000;
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimerMode {
    Mode13Bit,
    Mode16Bit,
    Mode8BitAutoReload,
    ModeSplit,
}

bitflags! {
    struct TMOD: u8 {
        const T0_M0 =   0b00000001;
        const T0_M1 =   0b00000010;
        const T0_CT =   0b00000100;
        const T0_GATE = 0b00001000;
        const T1_M0 =   0b00010000;
        const T1_M1 =   0b00100000;
        const T1_CT =   0b01000000;
        const T1_GATE = 0b10000000;
    }
}

impl TMOD {
    pub fn timer0_mode(&self) -> TimerMode {
        match self.bits & (TMOD::T0_M1 | TMOD::T0_M0).bits {
            0 => TimerMode::Mode13Bit,
            1 => TimerMode::Mode16Bit,
            2 => TimerMode::Mode8BitAutoReload,
            3 => TimerMode::ModeSplit,
            _ => TimerMode::ModeSplit,
        }
    }
    pub fn timer1_mode(&self) -> TimerMode {
        match self.bits & (TMOD::T1_M1 | TMOD::T1_M0).bits {
            0x00 => TimerMode::Mode13Bit,
            0x10 => TimerMode::Mode16Bit,
            0x20 => TimerMode::Mode8BitAutoReload,
            0x30 => TimerMode::ModeSplit,
            _ => TimerMode::ModeSplit,
        }
    }
}

pub struct Timer {
    tcon: TCON,
    tmod: TMOD,
    ie: u8,
    t0_value: u16,
    t1_value: u16,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            tcon: TCON::empty(),
            tmod: TMOD::empty(),
            ie: 0,
            t0_value: 0,
            t1_value: 0,
        }
    }
}

impl Memory for Timer {
    fn read_memory(&mut self, address: Address) -> Result<u8, &'static str> {
        match address {
            Address::Bit(bit) => {
                // generally used for SFR bit access
                match bit {
                    0x88..=0x8F => {
                        let flag = TCON::from_bits(1 << (bit & 7)).unwrap();
                        if self.tcon.contains(flag) {
                            Ok(1)
                        } else {
                            Ok(0)
                        }
                    }
                    0xA8..=0xAF => Ok(get_bit(self.ie, bit & 7)),
                    _ => Err("non-existant bit address"),
                }
            }
            Address::SpecialFunctionRegister(a) => match a {
                0x88 => Ok(self.tcon.bits),
                0x89 => Ok(self.tmod.bits),
                0x8A => Ok(self.t0_value.to_le_bytes()[0]),
                0x8B => Ok(self.t1_value.to_le_bytes()[0]),
                0x8C => Ok(self.t0_value.to_le_bytes()[1]),
                0x8D => Ok(self.t1_value.to_le_bytes()[1]),
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
                        let flag = TCON::from_bits(1 << (bit & 7)).unwrap();
                        self.tcon.set(flag, data != 0);
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
                    self.tcon.bits = data;
                    Ok(())
                }
                0x89 => {
                    self.tmod.bits = data;
                    Ok(())
                }
                0x8a => {
                    self.t0_value = u16::from_le_bytes([data, self.t0_value.to_le_bytes()[1]]);
                    Ok(())
                }
                0x8b => {
                    self.t1_value = u16::from_le_bytes([data, self.t1_value.to_le_bytes()[1]]);
                    Ok(())
                }
                0x8c => {
                    self.t0_value = u16::from_le_bytes([self.t0_value.to_le_bytes()[0], data]);
                    Ok(())
                }
                0x8d => {
                    self.t1_value = u16::from_le_bytes([self.t1_value.to_le_bytes()[0], data]);
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

    fn tick(&mut self) {
        // TODO: counter or interrupt mode
        //   counter mode (C/T = 1), timer only counts if T0/1 is high
        //   gate mode (GATE = 1), timer only counts if INT0/1 is high
        match self.tmod.timer0_mode() {
            TimerMode::Mode13Bit => {
                if self.tcon.contains(TCON::TR0) {
                    let values = self.t0_value.to_le_bytes();
                    let lower = (values[0] & 0x1f) + 1;
                    let upper = if lower == 32 {
                        match values[1].checked_add(1) {
                            Some(v) => v,
                            None => {
                                self.tcon.insert(TCON::TF0);
                                0
                            }
                        }
                    } else {
                        values[1]
                    };

                    self.t0_value = u16::from_le_bytes([lower & 0x1f, upper]);
                }
            }
            TimerMode::Mode16Bit => {
                if self.tcon.contains(TCON::TR0) {
                    self.t0_value = match self.t0_value.checked_add(1) {
                        Some(v) => v,
                        None => {
                            self.tcon.insert(TCON::TF0);
                            0
                        }
                    }
                }
            }
            TimerMode::Mode8BitAutoReload => {
                if self.tcon.contains(TCON::TR0) {
                    let value = self.t0_value.to_le_bytes()[0];
                    let reload = self.t0_value.to_le_bytes()[1];
                    let next_value = match value.checked_add(1) {
                        Some(v) => v,
                        None => {
                            self.tcon.insert(TCON::TF0);
                            reload
                        }
                    };
                    self.t0_value = u16::from_le_bytes([next_value, reload]);
                }
            }
            TimerMode::ModeSplit => {
                self.t0_value = u16::from_le_bytes({
                    let values = self.t0_value.to_le_bytes();
                    [
                        // low timer
                        if self.tcon.contains(TCON::TR0) {
                            match values[0].checked_add(1) {
                                Some(v) => v,
                                None => {
                                    self.tcon.insert(TCON::TF0);
                                    0
                                }
                            }
                        } else {
                            values[0]
                        },
                        // high timer
                        if self.tcon.contains(TCON::TR1) {
                            match values[1].checked_add(1) {
                                Some(v) => v,
                                None => {
                                    self.tcon.insert(TCON::TF1);
                                    0
                                }
                            }
                        } else {
                            values[1]
                        },
                    ]
                });
            }
        }
        match self.tmod.timer1_mode() {
            TimerMode::Mode13Bit => {
                if self.tcon.contains(TCON::TR1) || self.tmod.timer0_mode() == TimerMode::ModeSplit
                {
                    let values = self.t1_value.to_le_bytes();
                    let lower = (values[0] & 0x1f) + 1;
                    let upper = if lower == 32 {
                        match values[1].checked_add(1) {
                            Some(v) => v,
                            None => {
                                if self.tmod.timer0_mode() != TimerMode::ModeSplit {
                                    self.tcon.insert(TCON::TF1);
                                }
                                0
                            }
                        }
                    } else {
                        values[1]
                    };

                    self.t1_value = u16::from_le_bytes([lower & 0x1f, upper]);
                }
            }
            TimerMode::Mode16Bit => {
                if self.tcon.contains(TCON::TR1) || self.tmod.timer0_mode() == TimerMode::ModeSplit
                {
                    self.t1_value = match self.t1_value.checked_add(1) {
                        Some(v) => v,
                        None => {
                            if self.tmod.timer0_mode() != TimerMode::ModeSplit {
                                self.tcon.insert(TCON::TF1);
                            }
                            0
                        }
                    }
                }
            }
            TimerMode::Mode8BitAutoReload => {
                if self.tcon.contains(TCON::TR1) || self.tmod.timer0_mode() == TimerMode::ModeSplit
                {
                    let value = self.t1_value.to_le_bytes()[0];
                    let reload = self.t1_value.to_le_bytes()[1];
                    let next_value = match value.checked_add(1) {
                        Some(v) => v,
                        None => {
                            if self.tmod.timer0_mode() != TimerMode::ModeSplit {
                                self.tcon.insert(TCON::TF1);
                            }
                            reload
                        }
                    };
                    self.t1_value = u16::from_le_bytes([next_value, reload]);
                }
            }
            TimerMode::ModeSplit => panic!("timer 1 does not support split mode"),
        }
    }
}
