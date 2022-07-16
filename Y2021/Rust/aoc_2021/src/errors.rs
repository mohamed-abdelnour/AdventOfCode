//! A module providing an interface for dealing with errors.

use std::{error::Error, fmt};

/// Defines simple error types.
#[macro_export]
macro_rules! define_error {
    ($error:ident, $message:expr) => {
        #[allow(missing_docs)]
        #[derive(Debug)]
        pub struct $error;

        impl std::fmt::Display for $error {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $message)
            }
        }

        impl std::error::Error for $error {}
    };
}

define_error!(
    EmptyInputError,
    "input must not be empty any must contain at least one line ending"
);

define_error!(ParseDigitError, "a digit must be in b'0'..=b'9'");

/// An error that represents an unsatisfied expectation.
#[derive(Debug)]
pub struct Expectation {
    kind: String,
    expected: String,
    got: String,
}

impl Expectation {
    /// Returns a new `Expectation`.
    pub fn new(kind: impl ToString, expected: impl ToString, got: impl ToString) -> Self {
        let kind = kind.to_string();
        let expected = expected.to_string();
        let got = got.to_string();
        Expectation {
            kind,
            expected,
            got,
        }
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            r#"invalid {}: expected {}, got "{}""#,
            self.kind, self.expected, self.got
        )
    }
}

impl Error for Expectation {}
