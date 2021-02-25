// Copyright (C) 2021 xq-Tec GmbH

use crate::source::{BytePos, SrcRange};

#[derive(Debug)]
pub enum Message {
    // Lexer messages
    /// Extended identifier interrupted by end-of-line or end-of-file.
    ExtIdentIncomplete { start: BytePos },
    /// Invalid character in extended identifier.
    InvalidCharInExtIdent { start: BytePos, pos: BytePos },
    /// Character literal is not exactly one character long.
    InvalidCharLit { start: BytePos },
    /// Encountered unexpected character while lexing.
    UnexpectedChar { pos: BytePos },
    /// String literal interruped by end-of-line or end-of-file.
    EolInStrLit { start: BytePos },
    /// Invalid character in string literal.
    InvalidCharInStrLit { start: BytePos, pos: BytePos },
    /// Invalid exponent.
    InvalidExponent { start: BytePos, pos: BytePos },

    // Parser messages
    /// Parser encountered an unexpected token.
    UnexpectedToken {
        span: SrcRange,
        actual: &'static str,
        expected: Vec<&'static str>,
    },
}
