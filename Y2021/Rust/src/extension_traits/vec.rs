/// An extension to `std::vec::Vec`.
pub trait VecExt {
    /// Ensures `self[max_index]` would not panic, resizing `self` if needed.
    fn resize_to_fit(&mut self, max_index: usize);
}

impl<T: Clone + Default> VecExt for Vec<T> {
    fn resize_to_fit(&mut self, max_index: usize) {
        if self.len() <= max_index {
            self.resize(max_index + 1, Default::default());
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;

    #[test]
    fn resize_to_fit() {
        let mut length: usize = 10;

        let mut vector = Vec::<u8>::with_capacity(length);

        for i in 1..3 {
            length *= i;
            let index = length - 1;
            vector.resize_to_fit(index);
            assert_eq!(vector[index], u8::default());
            assert_eq!(vector.len(), length);
        }

        vector.resize_to_fit(3);
        assert_eq!(vector.len(), length);

        let reference = iter::repeat(u8::default()).take(length).collect::<Vec<_>>();
        assert_eq!(vector, reference);
    }
}
