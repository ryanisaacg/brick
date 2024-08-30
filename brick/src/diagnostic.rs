use std::fmt::Display;

use crate::SourceRange;

pub trait Diagnostic {
    fn contents(&self) -> DiagnosticContents;
}

pub enum DiagnosticContents {
    Scalar(DiagnosticMarker),
    Vector(Vec<DiagnosticMarker>),
}

impl Default for DiagnosticContents {
    fn default() -> Self {
        Self::Vector(Vec::new())
    }
}

impl DiagnosticContents {
    pub fn extend(&mut self, other: DiagnosticContents) {
        if let DiagnosticContents::Vector(vector) = &other {
            if vector.is_empty() {
                return;
            }
        }
        let mut combined = match std::mem::take(self) {
            DiagnosticContents::Scalar(marker) => vec![marker],
            DiagnosticContents::Vector(vector) => vector,
        };
        match other {
            DiagnosticContents::Scalar(scalar) => combined.push(scalar),
            DiagnosticContents::Vector(other_contents) => {
                combined.extend(other_contents);
            }
        }
        *self = DiagnosticContents::Vector(combined);
    }
}

impl Display for DiagnosticContents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagnosticContents::Scalar(marker) => marker.fmt(f),
            DiagnosticContents::Vector(contents) => {
                for line in contents.iter() {
                    writeln!(f, "{line}")?;
                }
                Ok(())
            }
        }
    }
}

pub struct DiagnosticMarker {
    pub range: SourceRange,
    pub message: &'static str,
    pub context: Option<String>,
    pub severity: Severity,
}

impl DiagnosticMarker {
    pub fn error(range: SourceRange, message: &'static str) -> DiagnosticMarker {
        DiagnosticMarker {
            range,
            message,
            context: None,
            severity: Severity::Error,
        }
    }

    pub fn error_context(
        range: SourceRange,
        message: &'static str,
        ctx: String,
    ) -> DiagnosticMarker {
        DiagnosticMarker {
            range,
            message,
            context: Some(ctx),
            severity: Severity::Error,
        }
    }

    pub fn info(range: SourceRange, message: &'static str) -> DiagnosticMarker {
        DiagnosticMarker {
            range,
            message,
            context: None,
            severity: Severity::Info,
        }
    }
}

impl Display for DiagnosticMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let severity = match self.severity {
            Severity::Error => "error",
            Severity::Info => "info",
        };
        write!(f, "[{severity}] {}: {}", self.range, self.message,)?;
        if let Some(context) = self.context.as_ref() {
            write!(f, ": {}", context)?;
        }
        writeln!(f)
    }
}

pub enum Severity {
    Error,
    Info,
}
