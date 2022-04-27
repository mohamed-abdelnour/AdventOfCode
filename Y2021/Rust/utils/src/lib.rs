use std::error::Error;

/// Display the error variant of a result.
pub trait DisplayPanic<T, E> {
    /// Returns the contained `Some` value or panics with the error's `std::fmt::Display`
    /// implementation.
    fn unwrap_or_display_panic(self) -> T;
}

impl<T, E: Error> DisplayPanic<T, E> for Result<T, E> {
    fn unwrap_or_display_panic(self) -> T {
        self.unwrap_or_else(|err| panic!("{err}"))
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;
    use std::fmt;

    use super::*;

    #[derive(Debug, Eq, PartialEq)]
    struct E;

    impl fmt::Display for E {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            writeln!(f, "panicking with std::fmt::Display formatting")
        }
    }

    impl Error for E {}

    #[test]
    fn unwrap() {
        assert_eq!(Result::<u8, E>::Ok(0).unwrap_or_display_panic(), 0);
    }

    #[test]
    #[should_panic(expected = "panicking with std::fmt::Display formatting")]
    fn unwrap_panic() {
        Result::<u8, E>::Err(E).unwrap_or_display_panic();
    }
}
