// Copyright (C) 2021 xq-Tec GmbH

mod token;

use std::sync::Arc;

use crate::message::Message;
use crate::source::{BytePos, MasterStrTable, Source, SrcChar, StrTable};
pub use token::{Kw, Token, TokenKind, KEYWORD_TABLE};

// NULL is used as a sentinel value value for end-of-file, so that we don't
// have to deal with Option<SrcChar> everywhere. We simply don't support
// files with embedded NULL characters, resp. cut them short.
const EOF_CHAR: SrcChar = 0x00;

pub fn lex(src: &mut Source, str_table: Arc<MasterStrTable>) -> (Vec<Token>, Vec<Message>) {
    // TODO clear src.lines
    let mut lexer = Lexer::new(src, str_table);
    let tokens = lexer.run();
    let msgs = lexer.msgs;
    (tokens, msgs)
}

struct Lexer<'a> {
    src: &'a mut Source,
    str_table: StrTable,
    msgs: Vec<Message>,

    /// Position within source file; used for BytePos in tokens.
    pos: BytePos,
    ch0: SrcChar,
    ch1: SrcChar,

    prev_was_integer: bool,
    /// Needed to determine how "'x'" should be interpreted. In most cases it's a
    /// character literal, but in something like "a'b'c" it's an apostrophe,
    /// followed by an identifier, followed by an apostrophe.
    /// Therefore, we use the following heuristic:
    /// If the previous token could precede the apostrophe in an attribute name,
    /// the apostrophe is presumed to be part of a name; otherwise, it's the start
    /// of a character literal.
    // TODO rename
    preceeded_attr_name: bool,

    eof_closes_line: bool,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a mut Source, str_table: Arc<MasterStrTable>) -> Lexer {
        let ch0 = src.get(BytePos(0)).unwrap_or(EOF_CHAR);
        let ch1 = src.get(BytePos(1)).unwrap_or(EOF_CHAR);
        Lexer {
            src,
            str_table: StrTable::new(str_table),
            msgs: vec![],
            pos: BytePos(0),
            ch0,
            ch1,
            prev_was_integer: false,
            preceeded_attr_name: false,
            eof_closes_line: false,
        }
    }

    fn run(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        loop {
            self.skip_ws();
            let tok_opt = match self.ch0 {
                b'\\' => self.ext_ident(),
                b'\'' => self.apostrophe(),
                b'"' => self.str_lit(),
                b'e' | b'E' if self.prev_was_integer => self.exponent(), // TODO only if no space was skipped
                EOF_CHAR => {
                    // TODO add error message if self.index != self.src.len()
                    return tokens;
                }
                ch if is_letter(ch) => Some(self.basic_ident()),
                ch if is_digit(ch) => Some(self.integer()),
                ch => self.symbol(ch),
            };

            if let Some(tok) = tok_opt {
                // The following can precede an apostrophe in an attribute name:
                // ident  char_lit  string_lit  ']'  ')'  '>>'  'all'
                self.preceeded_attr_name = matches!(
                    tok.kind,
                    TokenKind::Ident { .. }
                        | TokenKind::CharLit { .. }
                        | TokenKind::StrLit { .. }
                        | TokenKind::BracketClose
                        | TokenKind::ParenClose
                        | TokenKind::GtGt
                        | TokenKind::Kw(Kw::All)
                );
                tokens.push(tok);
            }
        }
    }

    // Steps over whitespace and comments, updates line info
    fn skip_ws(&mut self) {
        const NBSP: SrcChar = 0xa0;

        loop {
            match (self.ch0, self.ch1) {
                (b' ', _) | (b'\t', _) | (NBSP, _) => {
                    self.advance();
                    self.eof_closes_line = true;
                }
                (b'-', b'-') => {
                    // Comment
                    self.advance();
                    self.advance();
                    // Skip until end of line
                    while !is_eol(self.ch0) {
                        self.advance();
                    }
                    self.eof_closes_line = true;
                }
                (EOF_CHAR, _) => {
                    // Treat EOF as EOL, but don't advance. The EOF is caught in the
                    // next iteration of the main loop.
                    if self.eof_closes_line {
                        self.close_line();
                    }
                    return;
                }
                (b'\r', b'\n') => {
                    // Count \r\n combination only once
                    self.advance();
                    self.advance();
                    self.close_line();
                }
                (ch, _) if is_eol(ch) => {
                    self.advance();
                    self.close_line();
                }
                _ => {
                    self.eof_closes_line = true;
                    return;
                }
            }
        }
    }

    // § 15.4.2 'basic_identifier' (can also be a keyword)
    // Note: This function accepts identifiers that contain two underlines in a row.
    // Although this is forbidden by the VHDL language standard, it allows further parsing;
    // the error is raised later.
    fn basic_ident(&mut self) -> Token {
        // TODO: emit warning when identifier has two underlines in a row (forbidden by syntax)
        let start = self.pos;
        loop {
            self.advance();
            match self.ch0 {
                b'_' => continue,
                ch if is_letter_or_digit(ch) => continue,
                _ => {
                    // Anything else ends the identifier
                    let end = self.pos; // Current position is not part of identifier

                    let span = start.to(end);
                    let s = &self.src[span];
                    let norm = self.str_table.lookup(s, false);
                    let kind = if let Some(kw) = norm.as_keyword() {
                        TokenKind::Kw(kw)
                    } else {
                        TokenKind::Ident(norm)
                    };
                    return Token { span, kind };
                }
            }
        }
    }

    // § 15.4.3 'extended_identifier'
    fn ext_ident(&mut self) -> Option<Token> {
        let start = self.pos; // Position of opening backslash
        self.advance();

        loop {
            match (self.ch0, self.ch1) {
                (b'\\', b'\\') => {
                    // Escaped backslash => consume
                    self.advance();
                    self.advance();
                }
                (b'\\', _) => {
                    // Unescaped backslash => end of identifier
                    self.advance();
                    let end = self.pos;
                    let span = start.to(end);
                    let s = &self.src[span];
                    let norm = self.str_table.lookup(s, false);
                    return Some(Token {
                        span,
                        kind: TokenKind::Ident(norm),
                    });
                }
                (ch, _) if is_graphic_char(ch) => {
                    // Valid character => consume
                    self.advance();
                }
                (ch, _) if is_eol(ch) => {
                    // Unexpected end of line
                    self.msgs.push(Message::ExtIdentIncomplete { start: start });
                    return None;
                }
                (_, _) => {
                    // Other invalid character
                    self.msgs.push(Message::InvalidCharInExtIdent {
                        start: start,
                        pos: self.pos,
                    });
                    return None;
                }
            }
        }
    }

    fn integer(&mut self) -> Token {
        let start = self.pos;
        self.advance();
        while is_digit(self.ch0) || self.ch0 == b'_' {
            self.advance();
        }
        let end = self.pos;
        // TODO set to true only if no whitespace comes after integer
        self.prev_was_integer = true;
        let span = start.to(end);
        Token {
            span,
            kind: TokenKind::Integer(span),
        }
    }

    fn exponent(&mut self) -> Option<Token> {
        self.advance();
        let start = self.pos;

        // Sign
        if self.ch0 == b'+' || self.ch0 == b'-' {
            self.advance();
        }

        // First digit
        if is_digit(self.ch0) {
            self.advance();
        } else {
            self.msgs.push(Message::InvalidExponent {
                start: start,
                pos: self.pos,
            });
            return None;
        }

        // Remaining digits
        while is_digit(self.ch0) || self.ch0 == b'_' {
            self.advance();
        }
        let end = self.pos;
        let span = start.to(end);
        Some(Token {
            span,
            kind: TokenKind::Exponent(span),
        })
    }

    // TODO handle UTF-8 in string literals
    fn str_lit(&mut self) -> Option<Token> {
        let start = self.pos;
        self.advance();
        loop {
            match (self.ch0, self.ch1) {
                (b'"', b'"') => {
                    self.advance();
                    self.advance();
                }
                (b'"', _) => {
                    let end = self.pos;
                    self.advance();
                    let lit_span = (start + 1).to(end);
                    let span = start.to(end + 1);
                    return Some(Token {
                        span,
                        kind: TokenKind::StrLit(lit_span),
                    });
                }
                (ch, _) if is_graphic_char(ch) => {
                    self.advance();
                }
                (ch, _) if is_eol(ch) => {
                    self.msgs.push(Message::EolInStrLit { start });
                    return None;
                }
                (_, _) => {
                    self.msgs.push(Message::InvalidCharInStrLit {
                        start,
                        pos: self.pos,
                    });
                    return None;
                }
            }
        }
    }

    fn apostrophe(&mut self) -> Option<Token> {
        if self.preceeded_attr_name {
            Some(self.mk_tok1(TokenKind::Apostrophe))
        } else {
            let start = self.pos; // Position of opening backslash
            self.advance();
            match (self.ch0, self.ch1) {
                (ch, b'\'') => {
                    self.advance();
                    self.advance();
                    let end = self.pos;
                    let span = start.to(end);
                    Some(Token {
                        span,
                        kind: TokenKind::CharLit(ch),
                    })
                }
                _ => {
                    self.msgs.push(Message::InvalidCharLit { start: start });
                    None
                }
            }
        }
    }

    fn symbol(&mut self, ch0: SrcChar) -> Option<Token> {
        let tk = match ch0 {
            b'(' => self.mk_tok1(TokenKind::ParenOpen),
            b')' => self.mk_tok1(TokenKind::ParenClose),
            b'[' => self.mk_tok1(TokenKind::BracketOpen),
            b']' => self.mk_tok1(TokenKind::BracketClose),
            b'.' => self.mk_tok1(TokenKind::Period),
            b',' => self.mk_tok1(TokenKind::Comma),
            b';' => self.mk_tok1(TokenKind::Semicolon),
            b'|' => self.mk_tok1(TokenKind::Bar),
            b'^' => self.mk_tok1(TokenKind::Circumflex),
            b'=' => match self.ch1 {
                b'>' => self.mk_tok2(TokenKind::EqGt),
                _ => self.mk_tok1(TokenKind::Eq),
            },
            b'@' => self.mk_tok1(TokenKind::At),
            b'&' => self.mk_tok1(TokenKind::Ampersand),
            b':' => match self.ch1 {
                b'=' => self.mk_tok2(TokenKind::ColonEq),
                _ => self.mk_tok1(TokenKind::Colon),
            },
            b'<' => match self.ch1 {
                b'=' => self.mk_tok2(TokenKind::LtEq),
                b'<' => self.mk_tok2(TokenKind::LtLt),
                b'>' => self.mk_tok2(TokenKind::LtGt),
                _ => self.mk_tok1(TokenKind::Lt),
            },
            b'>' => match self.ch1 {
                b'=' => self.mk_tok2(TokenKind::GtEq),
                b'>' => self.mk_tok2(TokenKind::GtGt),
                _ => self.mk_tok1(TokenKind::Gt),
            },
            b'?' => return self.qm_symbol(),
            b'+' => self.mk_tok1(TokenKind::Plus),
            b'-' => self.mk_tok1(TokenKind::Minus),
            b'*' => match self.ch1 {
                b'*' => self.mk_tok2(TokenKind::Power),
                _ => self.mk_tok1(TokenKind::Asterisk),
            },
            b'/' => match self.ch1 {
                b'=' => self.mk_tok2(TokenKind::Neq),
                _ => self.mk_tok1(TokenKind::Slash),
            },
            _ => {
                self.msgs.push(Message::UnexpectedChar { pos: self.pos });
                return None;
            }
        };
        Some(tk)
    }

    fn qm_symbol(&mut self) -> Option<Token> {
        let start = self.pos;
        self.advance();
        let tk = match (self.ch0, self.ch1) {
            (b'?', _) => {
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmQm,
                }
            }

            (b'=', _) => {
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmEq,
                }
            }
            (b'/', b'=') => {
                self.advance();
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmNeq,
                }
            }

            (b'<', b'=') => {
                self.advance();
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmLtEq,
                }
            }
            (b'<', _) => {
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmLt,
                }
            }

            (b'>', b'=') => {
                self.advance();
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmGtEq,
                }
            }
            (b'>', _) => {
                self.advance();
                let span = start.to(self.pos);
                Token {
                    span,
                    kind: TokenKind::QmGt,
                }
            }

            (_, _) => {
                self.msgs.push(Message::UnexpectedChar { pos: self.pos });
                return None;
            }
        };
        Some(tk)
    }

    fn mk_tok1(&mut self, kind: TokenKind) -> Token {
        let span = self.pos.to(self.pos + 1);
        self.advance();
        Token { span, kind }
    }

    fn mk_tok2(&mut self, kind: TokenKind) -> Token {
        let span = self.pos.to(self.pos + 2);
        self.advance();
        self.advance();
        Token { span, kind }
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.ch0 = self.ch1;
        self.ch1 = self.src.get(self.pos + 1).unwrap_or(EOF_CHAR);
    }

    fn close_line(&mut self) {
        self.src.close_line(self.pos);
        self.eof_closes_line = false;
    }
}

const CHAR_CLASS_LETTER: u8 = 0x01;
const CHAR_CLASS_DIGIT: u8 = 0x02;
const CHAR_CLASS_GRAPHIC_CHAR: u8 = 0x04;
const CHAR_CLASS_EOL: u8 = 0x08;
const CHAR_CLASS_LETTER_OR_DIGIT: u8 = 0x10;

// The following table has been generated with char_class_table_gen.py
const CHAR_CLASS_TABLE: [u8; 256] = [
    0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x08, 0x08, 0x08, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x16, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15,
    0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15,
    0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x04, 0x04, 0x04, 0x04, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15,
    0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x04, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15,
    0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15,
    0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x04, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15,
];

fn is_letter(ch: SrcChar) -> bool {
    (CHAR_CLASS_TABLE[ch as usize] & CHAR_CLASS_LETTER) != 0
}

fn is_digit(ch: SrcChar) -> bool {
    (CHAR_CLASS_TABLE[ch as usize] & CHAR_CLASS_DIGIT) != 0
}

fn is_letter_or_digit(ch: SrcChar) -> bool {
    (CHAR_CLASS_TABLE[ch as usize] & CHAR_CLASS_LETTER_OR_DIGIT) != 0
}

fn is_graphic_char(ch: SrcChar) -> bool {
    (CHAR_CLASS_TABLE[ch as usize] & CHAR_CLASS_GRAPHIC_CHAR) != 0
}

fn is_eol(ch: SrcChar) -> bool {
    (CHAR_CLASS_TABLE[ch as usize] & CHAR_CLASS_EOL) != 0
}
