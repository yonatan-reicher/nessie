use crate::reporting::annotation::Region;

/// An error report.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Report {
    pub message: &'static str,
    pub region: Region,
    pub notes: Vec<String>,
    pub suggestion: Vec<String>,
}

