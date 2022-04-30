use crate::iterator::Array;

use super::*;

/// The display, as described in the puzzle.
const DISPLAY: [[usize; SEGMENTS]; DIGITS] = [
    //       g, f, e, d, c, b, a
    /* 0 */ [1, 1, 1, 0, 1, 1, 1],
    /* 1 */ [0, 1, 0, 0, 1, 0, 0],
    /* 2 */ [1, 0, 1, 1, 1, 0, 1],
    /* 3 */ [1, 1, 0, 1, 1, 0, 1],
    /* 4 */ [0, 1, 0, 1, 1, 1, 0],
    /* 5 */ [1, 1, 0, 1, 0, 1, 1],
    /* 6 */ [1, 1, 1, 1, 0, 1, 1],
    /* 7 */ [0, 1, 0, 0, 1, 0, 1],
    /* 8 */ [1, 1, 1, 1, 1, 1, 1],
    /* 9 */ [1, 1, 0, 1, 1, 1, 1],
    // ones: 7, 9, 4, 7, 8, 6, 8
];

/// Encodes each digit and finds the number of ones per encoded segment.
pub fn encode() {
    let Array(encoded) = DISPLAY
        .iter()
        .enumerate()
        .map(|(digit, row)| {
            let enc = row.iter().rfold(0, |acc, x| (acc << 1) + x);
            assert_eq!(Digit(enc).decode(), digit);
            println!("{enc} => {digit},");
            enc
        })
        .collect::<Array<_, DIGITS>>();

    println!();

    let Array(ones) = encoded
        .bit_transpose::<SEGMENTS>()
        .into_iter()
        .map(usize::count_ones)
        .collect::<Array<_, SEGMENTS>>();

    println!("ones: {:?}", ones);
}
