use crate::errors::ParseDigitError;

/// An extension trait to the `u8` type.
pub trait U8Ext {
    /// Maps a byte in (b'0'..=b'9') to its equivalent in (0..=9).
    fn parse_digit(self) -> Result<u8, ParseDigitError>;
}

impl U8Ext for u8 {
    fn parse_digit(self) -> Result<u8, ParseDigitError> {
        self.checked_sub(b'0').ok_or(ParseDigitError).and_then(|b| {
            if b <= 9 {
                Ok(b)
            } else {
                Err(ParseDigitError)
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use utils::DisplayPanic;

    use super::*;

    #[test]
    fn parse_digit() {
        (0..=9).for_each(|n| {
            let byte = b'0' + n;
            assert_eq!(byte.parse_digit().unwrap(), n);
        });
    }

    #[test]
    #[should_panic(expected = "a digit must be in b'0'..=b'9'")]
    fn parse_digit_panic_low() {
        (b'0' - 1).parse_digit().unwrap_or_display_panic();
    }

    #[test]
    #[should_panic(expected = "a digit must be in b'0'..=b'9'")]
    fn parse_digit_panic_high() {
        (b'9' + 1).parse_digit().unwrap_or_display_panic();
    }
}
