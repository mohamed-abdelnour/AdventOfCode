mod modular_arithmetic;

use crate::MODULUS;
use modular_arithmetic::{discrete_log, modular_exp};

pub fn solve(keys: &(u128, u128)) -> Option<u128> {
    let (key_1, key_2) = *keys;
    let loop_size_1 = discrete_log(&7, &key_1, &MODULUS).unwrap();
    let loop_size_2 = discrete_log(&7, &key_2, &MODULUS).unwrap();
    let result_1 = modular_exp(&key_1, &loop_size_2, &MODULUS);
    let result_2 = modular_exp(&key_2, &loop_size_1, &MODULUS);
    if result_1 == result_2 {
        Some(result_1)
    } else {
        None
    }
}
