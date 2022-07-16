//! A module providing an iterator over the two numbers adjacent to any integer.

use crate::integer::Integer;

/// An iterator over the two numbers adjacent to any integer.
#[derive(Debug, Default)]
pub struct Adjacent<N> {
    current: N,
    index: usize,
}

impl<N: Integer> From<N> for Adjacent<N> {
    fn from(current: N) -> Self {
        Self {
            current,
            ..Default::default()
        }
    }
}

impl<N: Integer> Iterator for Adjacent<N> {
    type Item = N;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.index += 1;

            match self.index {
                1 => {
                    if let n @ Some(_) = self.current.checked_sub(N::ONE) {
                        return n;
                    }
                }
                2 => {
                    if let n @ Some(_) = self.current.checked_add(N::ONE) {
                        return n;
                    }
                }
                _ => {
                    return None;
                }
            }
        }
    }
}
