use crate::reporting::annotation::Region;

/// An error report.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Report {
    message: String,
    region: Region,
    suggestion: Vec<String>,
    notes: Vec<String>,
}
