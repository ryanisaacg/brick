use std::fmt;

// TODO: don't include whole-source view in debug
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Provenance {
    source_name: &'static str,
    #[allow(dead_code)]
    source_text: &'static str,
    line: u32,
    offset: u32,
}

impl Provenance {
    pub fn new(
        source_name: &'static str,
        source_text: &'static str,
        line: u32,
        offset: u32,
    ) -> Provenance {
        Provenance {
            source_name,
            source_text,
            line,
            offset,
        }
    }
}

impl fmt::Debug for Provenance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.offset)
    }
}

impl fmt::Display for Provenance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}:{}", self.source_name, self.line, self.offset)
    }
}
