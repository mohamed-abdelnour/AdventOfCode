use std::collections::HashMap;

pub fn modular_exp(base: &u128, exponent: &u128, modulus: &u128) -> u128 {
    let mut base = *base;
    let mut exponent = *exponent;
    let mut result = 1;
    while exponent != 0 {
        if exponent % 2 == 1 {
            result = (result * base) % modulus;
        }
        base = base.pow(2) % modulus;
        exponent /= 2;
    }
    result
}

fn gcd(a: &u128, b: &u128) -> u128 {
    let mut a = *a;
    let mut b = *b;
    while b != 0 {
        let next = a % b;
        a = b;
        b = next;
    }
    a
}

pub fn discrete_log(
    base: &u128,
    result: &u128,
    modulus: &u128,
) -> Option<u128> {
    let mut base = *base;
    let mut result = *result;
    let mut modulus = *modulus;

    base %= modulus;
    result %= modulus;

    let mut next = 1;
    let mut offset = 0;
    let mut divisor = gcd(&base, &modulus);
    while divisor > 1 {
        if result == next {
            return Some(offset);
        } else if (result % divisor) != 0 {
            return None;
        }
        result /= divisor;
        modulus /= divisor;
        offset += 1;
        next = (next * base / divisor) % modulus;
        divisor = gcd(&base, &modulus);
    }

    let maximum = (modulus as f64).sqrt().ceil() as u128;
    let base_n = modular_exp(&base, &maximum, &modulus);

    let mut values: HashMap<u128, u128> = HashMap::new();
    let mut current = result;
    for i in 0..=maximum {
        values.insert(current, i as u128);
        current = (current * base) % modulus;
    }

    current = next;
    for j in 1..=maximum {
        current = (current * base_n) % modulus;
        if let Some(value) = values.get(&current) {
            return Some(maximum * j - value + offset);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modular_exp() {
        assert_eq!(0, modular_exp(&2, &2, &2));
        assert_eq!(1, modular_exp(&2, &4, &3));
        assert_eq!(3, modular_exp(&2, &3, &5));
        assert_eq!(3, modular_exp(&5, &7, &11));
        assert_eq!(7, modular_exp(&11, &13, &17));
    }

    #[test]
    fn test_gcd() {
        assert_eq!(2, gcd(&2, &4));
        assert_eq!(1, gcd(&2, &3));
        assert_eq!(2, gcd(&30, &154));
        assert_eq!(6, gcd(&30, &42));
    }

    #[test]
    fn test_discrete_log() {
        let modulus = 20_201_227;
        assert_eq!(8, discrete_log(&7, &5_764_801, &modulus).unwrap());
        assert_eq!(11, discrete_log(&7, &17_807_724, &modulus).unwrap());

        let base = 2 * 3 * 5;
        let modulus = 2 * 3 * 7;
        assert_eq!(2, discrete_log(&base, &18, &modulus).unwrap());
        assert_eq!(3, discrete_log(&base, &36, &modulus).unwrap());

        let base = 2 * 3 * 5;
        let modulus = 2 * 7 * 11;
        assert_eq!(1, discrete_log(&base, &30, &modulus).unwrap());
        assert_eq!(2, discrete_log(&base, &130, &modulus).unwrap());
        assert_eq!(3, discrete_log(&base, &50, &modulus).unwrap());
        assert_eq!(4, discrete_log(&base, &114, &modulus).unwrap());
        assert_eq!(5, discrete_log(&base, &32, &modulus).unwrap());
        assert_eq!(6, discrete_log(&base, &36, &modulus).unwrap());
        assert_eq!(7, discrete_log(&base, &2, &modulus).unwrap());
        assert_eq!(8, discrete_log(&base, &60, &modulus).unwrap());
        assert_eq!(9, discrete_log(&base, &106, &modulus).unwrap());
    }
}
