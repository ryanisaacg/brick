use std::fmt;

// TODO: worth it to do PartialOrd?
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct SourceRange {
    source_name: &'static str,
    #[allow(dead_code)]
    source_text: &'static str,
    start_line: u32,
    start_offset: u32,
    end_line: u32,
    end_offset: u32,
}

impl SourceRange {
    pub fn new(start: SourceMarker, end: SourceMarker) -> SourceRange {
        SourceRange {
            source_name: start.source_name,
            source_text: start.source_text,
            start_line: start.line,
            start_offset: start.offset,
            end_line: end.line,
            end_offset: end.offset,
        }
    }

    pub fn start(&self) -> SourceMarker {
        SourceMarker {
            source_name: self.source_name,
            source_text: self.source_text,
            line: self.start_line,
            offset: self.start_offset,
        }
    }

    pub fn end(&self) -> SourceMarker {
        SourceMarker {
            source_name: self.source_name,
            source_text: self.source_text,
            line: self.end_line,
            offset: self.end_offset,
        }
    }

    pub fn set_end(&mut self, end: SourceMarker) {
        self.end_line = end.line;
        self.end_offset = end.offset;
    }

    fn is_one_char(&self) -> bool {
        self.start_line == self.end_line && self.start_offset == self.end_offset
    }
}

impl fmt::Debug for SourceRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_one_char() {
            write!(f, "{}:{}", self.start_line, self.start_offset)
        } else {
            write!(
                f,
                "{}:{} - {}:{}",
                self.start_line, self.start_offset, self.end_line, self.end_offset
            )
        }
    }
}

impl fmt::Display for SourceRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_one_char() {
            write!(
                f,
                "{}@{}:{}",
                self.source_name, self.start_line, self.start_offset
            )
        } else {
            write!(
                f,
                "{}@{}:{} - {}:{}",
                self.source_name,
                self.start_line,
                self.start_offset,
                self.end_line,
                self.end_offset
            )
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SourceMarker {
    source_name: &'static str,
    #[allow(dead_code)]
    source_text: &'static str,
    line: u32,
    offset: u32,
}

impl SourceMarker {
    pub fn new(
        source_name: &'static str,
        source_text: &'static str,
        line: u32,
        offset: u32,
    ) -> SourceMarker {
        SourceMarker {
            source_name,
            source_text,
            line,
            offset,
        }
    }
}

impl fmt::Debug for SourceMarker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.offset)
    }
}

impl fmt::Display for SourceMarker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}:{}", self.source_name, self.line, self.offset)
    }
}
