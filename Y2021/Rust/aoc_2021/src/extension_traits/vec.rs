/// An extension to `std::vec::Vec`.
pub trait VecExt<T> {
    /// Ensures `self[index]` would not panic, resizing `self` if needed.
    fn ensure_index(&mut self, index: usize)
    where
        T: Clone + Default;
}

impl<T> VecExt<T> for Vec<T> {
    fn ensure_index(&mut self, index: usize)
    where
        T: Clone + Default,
    {
        if self.len() <= index {
            self.resize(index + 1, Default::default());
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;

    #[test]
    fn ensure_index() {
        let mut length: usize = 10;

        let mut vector = Vec::<u8>::with_capacity(length);

        for i in 1..3 {
            length *= i;
            let index = length - 1;
            vector.ensure_index(index);
            assert_eq!(vector[index], u8::default());
            assert_eq!(vector.len(), length);
        }

        vector.ensure_index(3);
        assert_eq!(vector.len(), length);

        let reference = iter::repeat(u8::default()).take(length).collect::<Vec<_>>();
        assert_eq!(vector, reference);
    }
}
