use std::{
    env,
    error::Error,
    io,
    io::ErrorKind,
    path::{Path, PathBuf},
};

const CARGO_LOCK: &str = "Cargo.lock";
const INPUTS: &str = "Inputs";

pub fn project_root() -> io::Result<PathBuf> {
    for ancestor in env::current_dir()?.ancestors() {
        for dir in ancestor.read_dir()?.flatten() {
            if dir.file_name() == CARGO_LOCK {
                return Ok(ancestor.into());
            }
        }
    }

    Err(ErrorKind::NotFound.into())
}

pub fn input_dir(p: impl AsRef<Path>) -> io::Result<PathBuf> {
    project_root().and_then(|root| {
        root.parent()
            .ok_or_else(|| ErrorKind::NotFound.into())
            .map(|parent| parent.join(INPUTS).join(p))
    })
}

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
    use std::{error::Error, fmt};

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
