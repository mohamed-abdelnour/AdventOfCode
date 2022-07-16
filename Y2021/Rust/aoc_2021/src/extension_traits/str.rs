//! Extension to the `str` type.

/// An extension trait to the `str` type.
pub trait StrExt {
    /// Returns two of `self`'s line endings.
    fn double_line_ending(&self) -> &'static str;
}

impl StrExt for &str {
    fn double_line_ending(&self) -> &'static str {
        if self.contains('\r') {
            "\r\n\r\n"
        } else {
            "\n\n"
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lf() {
        assert_eq!("0\n1".double_line_ending(), "\n\n");
    }

    #[test]
    fn crlf() {
        assert_eq!("0\r\n1".double_line_ending(), "\r\n\r\n");
    }
}
