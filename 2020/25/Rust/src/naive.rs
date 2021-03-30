use crate::MODULUS;

fn transform_by<F>(predicate: F, subject_number: &u128) -> (u128, u128)
where
    F: Fn((u128, u128)) -> bool,
{
    let mut key = *subject_number;
    let mut loop_size = 1;
    while !predicate((key, loop_size)) {
        key = (key * subject_number) % MODULUS;
        loop_size += 1;
    }
    (key, loop_size)
}

pub fn solve(keys: &(u128, u128)) -> u128 {
    let (key_1, key_2) = *keys;
    let loop_size = transform_by(|x| x.0 == key_1, &7).1;
    transform_by(|x| x.1 == loop_size, &key_2).0
}
