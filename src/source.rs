// Copyright (C) 2021 xq-Tec GmbH

use lazy_static::lazy_static;
use parking_lot::Mutex;
use std::fmt;
use std::ops;
use std::path::PathBuf;
use std::sync::Arc;

use crate::lexer::{Kw, KEYWORD_TABLE};
use crate::Map;

pub type SrcChar = u8;
pub type SrcBuf = Vec<SrcChar>;
pub type SrcSlice = [SrcChar];

pub struct Source {
    path: Arc<PathBuf>,
    bytes: SrcBuf,

    // Line #i spans from lines[i] to lines[i+1].
    lines: Vec<BytePos>,
}

impl Source {
    pub fn new(path: Arc<PathBuf>, bytes: SrcBuf) -> Source {
        Source {
            path,
            bytes,
            lines: vec![BytePos(0)],
        }
    }

    pub fn from_file(path: Arc<PathBuf>) -> std::io::Result<Source> {
        std::fs::read(&*path).map(|bytes| Source::new(path, bytes))
    }

    pub fn path(&self) -> &Arc<PathBuf> {
        &self.path
    }

    pub fn get(&self, pos: BytePos) -> Option<SrcChar> {
        self.bytes.get(pos.idx()).cloned()
    }

    // Close the current line and start a new one.
    // start: BytePos of first character in new line.
    pub fn close_line(&mut self, start: BytePos) {
        self.lines.push(start);
    }

    // Returns a tuple (line, column), each 1-based, of the given byte position.
    pub fn resolve_line_col(&self, pos: BytePos) -> (u32, u32) {
        debug_assert!(*self.lines.first().unwrap() <= pos);
        debug_assert!(pos <= *self.lines.last().unwrap());
        match self.lines.binary_search(&pos) {
            Ok(k) => (k as u32 + 1, 1),
            Err(k) => (k as u32, pos.0 - self.lines[k - 1].0 + 1),
        }
    }
}

impl ops::Index<SrcRange> for Source {
    type Output = SrcSlice;

    fn index(&self, index: SrcRange) -> &SrcSlice {
        &self.bytes[index.start.idx()..index.end.idx()]
    }
}

impl ops::Index<&SrcRange> for Source {
    type Output = SrcSlice;

    fn index(&self, index: &SrcRange) -> &SrcSlice {
        &self.bytes[index.start.idx()..index.end.idx()]
    }
}

pub fn src_to_str(s: &SrcSlice) -> &str {
    // TODO proper decoding
    std::str::from_utf8(s).unwrap()
}

// Maps each ISO/IEC 8859-1 upper case character to its lower case equivalent.
// * A...Z is replaced with a...z
// * À...Ö is replaced with à...ö
// * Ø...Þ is replaced with ø...þ
static LOWERCASE_MAP: [SrcChar; 256] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
    0x40, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xd7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
];

pub fn lowercase(ch: SrcChar) -> SrcChar {
    LOWERCASE_MAP[ch as usize]
}

#[derive(Copy, Clone, Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn idx(&self) -> usize {
        self.0 as usize
    }

    pub fn to(&self, rhs: BytePos) -> SrcRange {
        SrcRange {
            start: *self,
            end: rhs,
        }
    }
}

impl ops::Add<u32> for BytePos {
    type Output = BytePos;

    fn add(self, rhs: u32) -> BytePos {
        BytePos(self.0 + rhs)
    }
}

impl ops::AddAssign<u32> for BytePos {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl ops::Sub<u32> for BytePos {
    type Output = BytePos;

    fn sub(self, rhs: u32) -> BytePos {
        BytePos(self.0 - rhs)
    }
}

impl ops::Sub<BytePos> for BytePos {
    type Output = u32;

    fn sub(self, rhs: BytePos) -> u32 {
        self.0 - rhs.0
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SrcRange {
    pub start: BytePos,
    pub end: BytePos,
}

impl fmt::Debug for SrcRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}..{}>", self.start.0, self.end.0)
    }
}

/// Span within a source file.
///
/// `start` <= `end` and `end` is not part of the span.
#[derive(Clone)]
pub struct Span {
    pub src: Arc<Source>,
    pub range: SrcRange,
}

impl ops::Deref for Span {
    type Target = SrcSlice;

    fn deref(&self) -> &SrcSlice {
        &self.src[&self.range]
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let start = self.range.start;
        let end = self.range.end;
        let (l1, c1) = self.src.resolve_line_col(start);
        if end - start < 2 {
            write!(f, "{}:{}", l1, c1)
        } else {
            let (l2, c2) = self.src.resolve_line_col(end - 1);
            write!(f, "{}:{}...{}:{}", l1, c1, l2, c2)
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrId(u32);

impl StrId {
    pub fn as_keyword(&self) -> Option<Kw> {
        KEYWORD_TABLE.get(self.0 as usize).map(|&(kw, ..)| kw)
    }
}

pub struct MasterStrTable {
    /// Maps normalized strings to IDs.
    map: Mutex<Map<SrcBuf, StrId>>,
}

impl MasterStrTable {
    pub fn new() -> MasterStrTable {
        MasterStrTable {
            map: Mutex::new(STR_TABLE_TEMPLATE.clone()),
        }
    }

    fn lookup(&self, s_norm: &SrcSlice, hash: u64) -> StrId {
        let mut map = self.map.lock();
        let len = map.len() as u32;
        use hashbrown::hash_map::RawEntryMut::*;
        match map.raw_entry_mut().from_key_hashed_nocheck(hash, s_norm) {
            Occupied(entry) => *entry.get(),
            Vacant(entry) => {
                *entry
                    .insert_hashed_nocheck(hash, s_norm.into(), StrId(len))
                    .1
            }
        }
    }
}

#[derive(Clone)]
pub struct StrTable {
    master: Arc<MasterStrTable>,
    /// Maps non-normalized strings to IDs.
    map: Map<SrcBuf, StrId>,
    /// Temporary buffer for normalizing strings.
    tmp_buf: SrcBuf,
}

impl StrTable {
    // Create new string table and initialize it with the list of reserved words.
    pub fn new(master: Arc<MasterStrTable>) -> StrTable {
        StrTable {
            master,
            map: STR_TABLE_TEMPLATE.clone(),
            tmp_buf: Vec::with_capacity(128),
        }
    }

    pub fn lookup(&mut self, s: &SrcSlice, is_norm: bool) -> StrId {
        let hash = Self::make_hash(s);
        use hashbrown::hash_map::RawEntryMut::*;
        match self.map.raw_entry_mut().from_key_hashed_nocheck(hash, s) {
            Occupied(entry) => *entry.get(),
            Vacant(entry) => {
                let (s_norm, hash_norm) = if is_norm {
                    (s, hash)
                } else {
                    // Normalize string, then compute normalized hash
                    self.tmp_buf.clear();
                    self.tmp_buf.extend(s.iter().map(|&c| lowercase(c)));
                    (&*self.tmp_buf, Self::make_hash(&self.tmp_buf))
                };
                // Lookup ID of normalized string
                let str_id = self.master.lookup(s_norm, hash_norm);
                // Store non-normalized string with ID of normalized string
                entry.insert_hashed_nocheck(hash, s.into(), str_id);
                str_id
            }
        }
    }

    fn make_hash(s: &SrcSlice) -> u64 {
        use std::hash::{BuildHasher, Hash, Hasher};
        let mut state = fnv::FnvBuildHasher::default().build_hasher();
        s.hash(&mut state);
        state.finish()
    }
}

lazy_static! {
    static ref STR_TABLE_TEMPLATE: Map<SrcBuf, StrId> = {
        KEYWORD_TABLE
            .iter()
            .enumerate()
            .map(|(i, &(kw, s, _))| {
                let id = StrId(i as u32);
                debug_assert_eq!(i, kw as usize);
                (s.into(), id)
            })
            .collect()
    };
}
