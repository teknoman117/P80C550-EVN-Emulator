use std::fs;
use std::path::Path;
use std::rc::Rc;

enum AddressSpace {
    Code,
    Data
}

trait Memory {
    fn read_memory(&mut self, space: AddressSpace, address: usize) -> u8;
    fn write_memory(&mut self, space: AddressSpace, address: usize, data: u8);
}

struct ROM {
    data: Vec<u8>
}

impl ROM {
    fn load_from_binary(path: &Path) -> Result<ROM, Box<dyn std::error::Error + 'static>> {
        Ok(ROM {
            data: fs::read(path)?
        })
    }
}

impl Memory for ROM {
    fn read_memory(&mut self, _space: AddressSpace, address: usize) -> u8 {
        if address < self.data.len() {
            self.data[address]
        } else {
            0xFFu8
        }
    }

    fn write_memory(&mut self, _space: AddressSpace, _address: usize, _data: u8) {
        // it's ROM, do nothing
    }
}

struct RAM {
    data: Vec<u8>
}

impl RAM {
    fn create_with_size(size: usize) -> RAM {
        let mut data = Vec::with_capacity(size);
        data.resize(size, 0);
        RAM {
            data: data
        }
    }
}

impl Memory for RAM {
    fn read_memory(&mut self, _space: AddressSpace, address: usize) -> u8 {
        self.data[address]
    }
    fn write_memory(&mut self, _space: AddressSpace, address: usize, data: u8) {
        self.data[address] = data;
    }
}

struct MemoryMapperModule<'a, A : Memory, B : Memory> {
    rom: &'a mut A,
    ram: &'a mut B
}

impl <'a, A : Memory, B : Memory> MemoryMapperModule<'a, A, B> {
    fn new(rom: &'a mut A, ram: &'a mut B) -> MemoryMapperModule<'a, A, B> {
        MemoryMapperModule {
            rom: rom,
            ram: ram
        }
    }
}

impl <'a, A : Memory, B : Memory> Memory for MemoryMapperModule<'a, A, B> {
    fn read_memory(&mut self, space: AddressSpace, address: usize) -> u8 {
        0
    }
    fn write_memory(&mut self, space: AddressSpace, address: usize, data: u8) {

    }
}

struct CPU8051<'a> {

}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    // load the application rom
    let rom_path = Path::new("rom.bin");
    let mut rom = ROM::load_from_binary(rom_path)?;

    // create a ram for the application
    let mut ram = RAM::create_with_size(8192);

    // create the memory mapper
    let mut mmu = MemoryMapperModule::new(&mut rom, &mut ram);

    Ok(())
}
