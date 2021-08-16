pub mod cpu;
pub mod memory;
pub mod soc;

use std::ops::{BitAnd, BitOr, Not, Shl};

pub fn insert_bit<T>(value: T, bit: u8) -> <T as BitOr>::Output
where
    T: BitOr + Shl + From<u8> + From<<T as Shl>::Output>,
{
    value | T::from(T::from(1) << T::from(bit))
}

pub fn remove_bit<T>(value: T, bit: u8) -> <T as BitAnd>::Output
where
    T: BitAnd + Not + Shl + From<u8> + From<<T as Shl>::Output> + From<<T as Not>::Output>,
{
    value & T::from(!T::from(T::from(1) << T::from(bit)))
}

pub fn set_bit<T>(value: T, bit: u8, set: bool) -> T
where
    T: BitAnd
        + BitOr
        + Not
        + Shl
        + From<u8>
        + From<<T as Shl>::Output>
        + From<<T as Not>::Output>
        + From<<T as BitOr>::Output>
        + From<<T as BitAnd>::Output>,
{
    if set {
        T::from(insert_bit(value, bit))
    } else {
        T::from(remove_bit(value, bit))
    }
}

pub fn get_bit<T>(value: T, bit: u8) -> T
where
    T: BitAnd + Shl + From<u8> + From<<T as Shl>::Output>,
    <T as BitAnd>::Output: PartialEq<T>,
{
    if (value & T::from(T::from(1) << T::from(bit))) != T::from(0) {
        T::from(1)
    } else {
        T::from(0)
    }
}
