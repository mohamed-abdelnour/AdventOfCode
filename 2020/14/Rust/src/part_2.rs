use crate::Instruction;

use std::collections::HashMap;
use std::iter;

type Memory = HashMap<u64, u64>;

fn dec_to_bin(dec: &u64) -> String {
    format!("{:b}", dec)
}

fn bin_to_dec(bin: &str) -> u64 {
    u64::from_str_radix(bin, 2).unwrap()
}

fn x_count(string: &str) -> u32 {
    string.chars().filter(|x| x == &'X').count() as u32
}

fn mask_address(mask: &str, bin: &str) -> String {
    let bin = bin.chars().collect::<Vec<_>>();
    let mut result = Vec::new();
    for (index, bit) in mask.chars().enumerate() {
        if (index >= bin.len()) || (bit != '0') {
            result.push(bit);
        } else {
            result.push(bin[index]);
        }
    }
    result.iter().rev().collect()
}

fn gen_address(mask: &str, combination: &str) -> String {
    let mut result = Vec::new();
    let mut combination = combination.chars().collect::<Vec<_>>();
    for bit in mask.chars() {
        if bit == 'X' {
            let head = combination.remove(0);
            result.push(head);
        } else {
            result.push(bit);
        }
    }
    result.iter().collect()
}

fn gen_addresses(mask: &str, count: u32) -> Vec<String> {
    let mut result = Vec::new();
    for n in 0..(2u64.pow(count)) {
        let combination = dec_to_bin(&n);
        let mut zeros = iter::repeat('0')
            .take(count as usize)
            .skip(combination.len())
            .collect::<String>();
        zeros.push_str(&combination);
        let next_mask = gen_address(mask, &zeros);
        result.push(next_mask);
    }
    result.to_vec()
}

fn step_memory(mut memory: Memory, value: u64, addresses: &[u64]) -> Memory {
    for address in addresses {
        memory.insert(*address, value);
    }
    memory
}

pub fn step_input(instructions: &[Instruction]) -> u64 {
    let mut mask = String::new();
    let mut memory = HashMap::new();
    for instruction in instructions {
        let id = &instruction.id;
        let value = &instruction.value;
        if id == "mask" {
            mask = value.to_string();
        } else {
            let read = |x: &str| x.parse::<u64>().unwrap();
            let value = read(value);
            let id = read(id);

            let bin_id = dec_to_bin(&id).chars().rev().collect::<String>();
            let rev_mask = mask.chars().rev().collect::<String>();
            let masked_address = mask_address(&rev_mask, &bin_id);

            let count = x_count(&masked_address);

            let next_addresses = gen_addresses(&masked_address, count)
                .iter()
                .map(|x| bin_to_dec(x))
                .collect::<Vec<_>>();

            memory = step_memory(memory, value, &next_addresses);
        }
    }
    memory.values().sum()
}
