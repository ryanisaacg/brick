use crate::SourceRange;

pub trait Diagnostic {
    fn contents(&self) -> DiagnosticContents;
}

pub enum DiagnosticContents {
    Scalar(DiagnosticMarker),
    Vector(Vec<DiagnosticMarker>),
}

pub struct DiagnosticMarker {
    pub range: SourceRange,
    pub message: &'static str,
    pub severity: Severity,
}

impl DiagnosticMarker {
    pub fn error(range: SourceRange, message: &'static str) -> DiagnosticMarker {
        DiagnosticMarker {
            range,
            message,
            severity: Severity::Error,
        }
    }

    pub fn info(range: SourceRange, message: &'static str) -> DiagnosticMarker {
        DiagnosticMarker {
            range,
            message,
            severity: Severity::Info,
        }
    }
}

pub enum Severity {
    Error,
    Info,
}
