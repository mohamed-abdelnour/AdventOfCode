use std::error::Error;
use std::fmt;

/// A simple error type that wraps a message.
#[derive(Debug)]
struct AdHocError {
    /// The message to wrap as an error.
    message: String,
}

impl AdHocError {
    /// Returns a new `AdHocError` from a given message.
    fn new(message: impl ToString) -> Self {
        let message = message.to_string();
        AdHocError { message }
    }
}

impl fmt::Display for AdHocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for AdHocError {}

/// Define simple error types using `AdHocError`.
#[macro_export]
macro_rules! define_error {
    ($error:ident, $message:expr) => {
        #[allow(missing_docs)]
        #[derive(Debug)]
        pub struct $error;

        impl fmt::Display for $error {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                AdHocError::new($message).fmt(f)
            }
        }

        impl Error for $error {}
    };
}

define_error!(EmptyInputError, "input must not be empty");
