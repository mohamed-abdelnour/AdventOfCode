use crate::Instruction;

use std::collections::HashMap;

fn set_bit(num: u64, bit: u32) -> u64 {
    let bit = 2u64.pow(bit);
    num | bit
}

fn clear_bit(num: u64, bit: u32) -> u64 {
    let bit = 2u64.pow(bit);
    let complement = !bit;
    num & complement
}

fn apply_mask(mask: &str, mut value: u64) -> u64 {
    for (index, bit) in mask.chars().rev().enumerate() {
        if bit == '0' {
            value = clear_bit(value, index as u32);
        } else if bit == '1' {
            value = set_bit(value, index as u32);
        }
    }
    value
}

pub fn step_memory(instructions: &[Instruction]) -> u64 {
    let mut mask = String::new();
    let mut memory = HashMap::new();
    for instruction in instructions {
        let id = &instruction.id;
        let value = &instruction.value;

        if id == "mask" {
            mask = value.to_string();
        } else {
            let value = apply_mask(&mask, value.parse().unwrap());
            let id = id.parse::<u64>().unwrap();
            memory.insert(id, value);
        }
    }
    memory.values().sum()
}
