use crate::spi::Device;

use std::collections::VecDeque;

enum SdCardState {
    Uninitialized,
    Idle,
}

pub struct SdCard {
    state: SdCardState,
    write_fifo: VecDeque<u8>,
    read_fifo: VecDeque<u8>,
}

impl SdCard {
    pub fn new() -> SdCard {
        SdCard {
            state: SdCardState::Uninitialized,
            write_fifo: VecDeque::new(),
            read_fifo: VecDeque::new(),
        }
    }

    fn tick(&mut self) {
        match self.state {
            SdCardState::Uninitialized | SdCardState::Idle => {
                // search for command
                while self.write_fifo.len() > 5 {
                    let command = self.write_fifo.pop_front().unwrap();
                    if (command & 0x40) == 0x40 {
                        // process command
                        //let argument = u32::from_be_bytes()
                    }
                }
            }
        }
    }
}

impl Device for SdCard {
    fn transfer(&mut self, data: u8) -> u8 {
        self.write_fifo.push_back(data);
        let response = self.read_fifo.pop_front().unwrap_or(0xFF);
        self.tick();
        response
    }
}
