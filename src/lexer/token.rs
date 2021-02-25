// Copyright (C) 2021 xq-Tec GmbH

use std::fmt;

use crate::source::{src_to_str, SrcChar, SrcRange, SrcSlice, StrId};

#[derive(Copy, Clone)]
pub struct Token {
    pub span: SrcRange,
    pub kind: TokenKind,
}

impl Token {
    pub fn matches_literal(&self, lit: &str) -> bool {
        use TokenKind::*;
        match (self.kind, lit) {
            (Kw(kw), lit) => KEYWORD_TABLE[kw as usize].2 == lit,
            (ParenOpen, "(")
            | (ParenClose, ")")
            | (BracketOpen, "[")
            | (BracketClose, "]")
            | (Period, ".")
            | (Comma, ",")
            | (Semicolon, ";")
            | (Colon, ":")
            | (Bar, "|")
            | (Circumflex, "^")
            | (ColonEq, ":=")
            | (EqGt, "=>")
            | (Eq, "=")
            | (Neq, "/=")
            | (Lt, "<")
            | (Gt, ">")
            | (LtEq, "<=")
            | (GtEq, ">=")
            | (QmQm, "??")
            | (QmEq, "?=")
            | (QmNeq, "?/=")
            | (QmLt, "?<")
            | (QmLtEq, "?<=")
            | (QmGt, "?>")
            | (QmGtEq, "?>=")
            | (At, "@")
            | (Apostrophe, "'")
            | (Ampersand, "&")
            | (LtLt, "<<")
            | (GtGt, ">>")
            | (LtGt, "<>")
            | (Plus, "+")
            | (Minus, "-")
            | (Asterisk, "*")
            | (Slash, "/")
            | (Power, "**") => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// basic_identifier and extended_identifier; contains ID of normalized string.
    Ident(StrId),
    /// Reserved word; all identifiers that are reserved words are interpreted as keywords.
    Kw(Kw),

    /// character_literal
    CharLit(SrcChar),
    /// string_literal, string span excluding quotation marks
    StrLit(SrcRange),
    /// [0-9][0-9_]+
    Integer(SrcRange),
    /// ('E' | 'e') ('+' | '-')? [0-9][0-9_]+   (span excludes 'E'/'e')
    Exponent(SrcRange),

    ParenOpen,    // (
    ParenClose,   // )
    BracketOpen,  // [
    BracketClose, // ]
    Period,       // .
    Comma,        // ,
    Semicolon,    // ;
    Colon,        // :
    Bar,          // |
    Circumflex,   // ^
    ColonEq,      // :=
    EqGt,         // =>
    Eq,           // =
    Neq,          // /=
    Lt,           // <
    Gt,           // >
    LtEq,         // <=
    GtEq,         // >=
    QmQm,         // ??
    QmEq,         // ?=
    QmNeq,        // ?/=
    QmLt,         // ?<
    QmLtEq,       // ?<=
    QmGt,         // ?>
    QmGtEq,       // ?>=
    At,           // @
    Apostrophe,   // '
    Ampersand,    // &
    LtLt,         // <<
    GtGt,         // >>
    LtGt,         // <>
    Plus,         // +
    Minus,        // -
    Asterisk,     // *
    Slash,        // /
    Power,        // **
}

impl TokenKind {
    pub fn name(&self) -> &'static str {
        use TokenKind::*;
        match *self {
            Ident(_) => "identifier",
            Kw(kw) => KEYWORD_TABLE[kw as usize].2,
            CharLit(_) => "character literal",
            StrLit(_) => "string literal",
            Integer(_) => "integer literal",
            Exponent(_) => "exponent",
            ParenOpen => "(",
            ParenClose => ")",
            BracketOpen => "[",
            BracketClose => "]",
            Period => ".",
            Comma => ",",
            Semicolon => ";",
            Colon => ":",
            Bar => "|",
            Circumflex => "^",
            ColonEq => ":=",
            EqGt => "=>",
            Eq => "=",
            Neq => "/=",
            Lt => "<",
            Gt => ">",
            LtEq => "<=",
            GtEq => ">=",
            QmQm => "??",
            QmEq => "?=",
            QmNeq => "?/=",
            QmLt => "?<",
            QmLtEq => "?<=",
            QmGt => "?>",
            QmGtEq => "?>=",
            At => "@",
            Apostrophe => "'",
            Ampersand => "&",
            LtLt => "<<",
            GtGt => ">>",
            LtGt => "<>",
            Plus => "+",
            Minus => "-",
            Asterisk => "*",
            Slash => "/",
            Power => "**",
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TokenKind::Ident(norm) => write!(f, "ident({:?})", norm),
            TokenKind::Kw(kw) => write!(f, "kw: {}", KEYWORD_TABLE[kw as usize].2),
            TokenKind::Integer(span) => write!(f, "int {:?}", span),
            TokenKind::Exponent(span) => write!(f, "exponent \"{:?}\"", span),
            TokenKind::CharLit(ch) => write!(f, "'{}'", src_to_str(&[ch])),
            TokenKind::StrLit(span) => write!(f, "strlit {:?}", span),
            TokenKind::ParenOpen => write!(f, "("),
            TokenKind::ParenClose => write!(f, ")"),
            TokenKind::BracketOpen => write!(f, "["),
            TokenKind::BracketClose => write!(f, "]"),
            TokenKind::Period => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Bar => write!(f, "|"),
            TokenKind::Circumflex => write!(f, "^"),
            TokenKind::ColonEq => write!(f, ":="),
            TokenKind::EqGt => write!(f, "=>"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Neq => write!(f, "/="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::QmQm => write!(f, "??"),
            TokenKind::QmEq => write!(f, "?="),
            TokenKind::QmNeq => write!(f, "?/="),
            TokenKind::QmLt => write!(f, "?<"),
            TokenKind::QmLtEq => write!(f, "?<="),
            TokenKind::QmGt => write!(f, "?>"),
            TokenKind::QmGtEq => write!(f, "?>="),
            TokenKind::At => write!(f, "@"),
            TokenKind::Apostrophe => write!(f, "'"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::LtLt => write!(f, "<<"),
            TokenKind::GtGt => write!(f, ">>"),
            TokenKind::LtGt => write!(f, "<>"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Power => write!(f, "**"),
        }
    }
}

//------------------------------------------------------------------------------
// Keywords
//------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Kw {
    Abs = 0,
    Access,
    After,
    Alias,
    All,
    And,
    Architecture,
    Array,
    Assert,
    Assume,
    AssumeGuarantee,
    Attribute,
    Begin,
    Block,
    Body,
    Buffer,
    Bus,
    Case,
    Component,
    Configuration,
    Constant,
    Context,
    Cover,
    Default,
    Disconnect,
    Downto,
    Else,
    Elsif,
    End,
    Entity,
    Exit,
    Fairness,
    File,
    For,
    Force,
    Function,
    Generate,
    Generic,
    Group,
    Guarded,
    If,
    Impure,
    In,
    Inertial,
    Inout,
    Is,
    Label,
    Library,
    Linkage,
    Literal,
    Loop,
    Map,
    Mod,
    Nand,
    New,
    Next,
    Nor,
    Not,
    Null,
    Of,
    On,
    Open,
    Or,
    Others,
    Out,
    Package,
    Parameter,
    Port,
    Postponed,
    Procedure,
    Process,
    Property,
    Protected,
    Pure,
    Range,
    Record,
    Register,
    Reject,
    Release,
    Rem,
    Report,
    Restrict,
    RestrictGuarantee,
    Return,
    Rol,
    Ror,
    Select,
    Sequence,
    Severity,
    Signal,
    Shared,
    Sla,
    Sll,
    Sra,
    Srl,
    Strong,
    Subtype,
    Then,
    To,
    Transport,
    Type,
    Unaffected,
    Units,
    Until,
    Use,
    Variable,
    Vmode,
    Vprop,
    Vunit,
    Wait,
    When,
    While,
    With,
    Xnor,
    Xor,
}

pub static KEYWORD_TABLE: &[(Kw, &SrcSlice, &str)] = &[
    (Kw::Abs, b"abs", "abs"),
    (Kw::Access, b"access", "access"),
    (Kw::After, b"after", "after"),
    (Kw::Alias, b"alias", "alias"),
    (Kw::All, b"all", "all"),
    (Kw::And, b"and", "and"),
    (Kw::Architecture, b"architecture", "architecture"),
    (Kw::Array, b"array", "array"),
    (Kw::Assert, b"assert", "assert"),
    (Kw::Assume, b"assume", "assume"),
    (Kw::AssumeGuarantee, b"assume_guarantee", "assume_guarantee"),
    (Kw::Attribute, b"attribute", "attribute"),
    (Kw::Begin, b"begin", "begin"),
    (Kw::Block, b"block", "block"),
    (Kw::Body, b"body", "body"),
    (Kw::Buffer, b"buffer", "buffer"),
    (Kw::Bus, b"bus", "bus"),
    (Kw::Case, b"case", "case"),
    (Kw::Component, b"component", "component"),
    (Kw::Configuration, b"configuration", "configuration"),
    (Kw::Constant, b"constant", "constant"),
    (Kw::Context, b"context", "context"),
    (Kw::Cover, b"cover", "cover"),
    (Kw::Default, b"default", "default"),
    (Kw::Disconnect, b"disconnect", "disconnect"),
    (Kw::Downto, b"downto", "downto"),
    (Kw::Else, b"else", "else"),
    (Kw::Elsif, b"elsif", "elsif"),
    (Kw::End, b"end", "end"),
    (Kw::Entity, b"entity", "entity"),
    (Kw::Exit, b"exit", "exit"),
    (Kw::Fairness, b"fairness", "fairness"),
    (Kw::File, b"file", "file"),
    (Kw::For, b"for", "for"),
    (Kw::Force, b"force", "force"),
    (Kw::Function, b"function", "function"),
    (Kw::Generate, b"generate", "generate"),
    (Kw::Generic, b"generic", "generic"),
    (Kw::Group, b"group", "group"),
    (Kw::Guarded, b"guarded", "guarded"),
    (Kw::If, b"if", "if"),
    (Kw::Impure, b"impure", "impure"),
    (Kw::In, b"in", "in"),
    (Kw::Inertial, b"inertial", "inertial"),
    (Kw::Inout, b"inout", "inout"),
    (Kw::Is, b"is", "is"),
    (Kw::Label, b"label", "label"),
    (Kw::Library, b"library", "library"),
    (Kw::Linkage, b"linkage", "linkage"),
    (Kw::Literal, b"literal", "literal"),
    (Kw::Loop, b"loop", "loop"),
    (Kw::Map, b"map", "map"),
    (Kw::Mod, b"mod", "mod"),
    (Kw::Nand, b"nand", "nand"),
    (Kw::New, b"new", "new"),
    (Kw::Next, b"next", "next"),
    (Kw::Nor, b"nor", "nor"),
    (Kw::Not, b"not", "not"),
    (Kw::Null, b"null", "null"),
    (Kw::Of, b"of", "of"),
    (Kw::On, b"on", "on"),
    (Kw::Open, b"open", "open"),
    (Kw::Or, b"or", "or"),
    (Kw::Others, b"others", "others"),
    (Kw::Out, b"out", "out"),
    (Kw::Package, b"package", "package"),
    (Kw::Parameter, b"parameter", "parameter"),
    (Kw::Port, b"port", "port"),
    (Kw::Postponed, b"postponed", "postponed"),
    (Kw::Procedure, b"procedure", "procedure"),
    (Kw::Process, b"process", "process"),
    (Kw::Property, b"property", "property"),
    (Kw::Protected, b"protected", "protected"),
    (Kw::Pure, b"pure", "pure"),
    (Kw::Range, b"range", "range"),
    (Kw::Record, b"record", "record"),
    (Kw::Register, b"register", "register"),
    (Kw::Reject, b"reject", "reject"),
    (Kw::Release, b"release", "release"),
    (Kw::Rem, b"rem", "rem"),
    (Kw::Report, b"report", "report"),
    (Kw::Restrict, b"restrict", "restrict"),
    (
        Kw::RestrictGuarantee,
        b"restrict_guarantee",
        "restrict_guarantee",
    ),
    (Kw::Return, b"return", "return"),
    (Kw::Rol, b"rol", "rol"),
    (Kw::Ror, b"ror", "ror"),
    (Kw::Select, b"select", "select"),
    (Kw::Sequence, b"sequence", "sequence"),
    (Kw::Severity, b"severity", "severity"),
    (Kw::Signal, b"signal", "signal"),
    (Kw::Shared, b"shared", "shared"),
    (Kw::Sla, b"sla", "sla"),
    (Kw::Sll, b"sll", "sll"),
    (Kw::Sra, b"sra", "sra"),
    (Kw::Srl, b"srl", "srl"),
    (Kw::Strong, b"strong", "strong"),
    (Kw::Subtype, b"subtype", "subtype"),
    (Kw::Then, b"then", "then"),
    (Kw::To, b"to", "to"),
    (Kw::Transport, b"transport", "transport"),
    (Kw::Type, b"type", "type"),
    (Kw::Unaffected, b"unaffected", "unaffected"),
    (Kw::Units, b"units", "units"),
    (Kw::Until, b"until", "until"),
    (Kw::Use, b"use", "use"),
    (Kw::Variable, b"variable", "variable"),
    (Kw::Vmode, b"vmode", "vmode"),
    (Kw::Vprop, b"vprop", "vprop"),
    (Kw::Vunit, b"vunit", "vunit"),
    (Kw::Wait, b"wait", "wait"),
    (Kw::When, b"when", "when"),
    (Kw::While, b"while", "while"),
    (Kw::With, b"with", "with"),
    (Kw::Xnor, b"xnor", "xnor"),
    (Kw::Xor, b"xor", "xor"),
];
