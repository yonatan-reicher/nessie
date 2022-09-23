use crate::reporting::annotation::Region;
use std::fmt::{self, Display, Formatter};

/// An error report.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Report {
    pub message: String,
    pub region: Region,
    pub notes: Vec<String>,
    pub suggestion: Vec<String>,
}

pub struct ReportWithSource<'a, 'b> {
    report: &'a Report,
    source: &'b str,
}

impl Report {
    pub fn with_source<'a, 'b>(&'a self, source: &'b str) -> ReportWithSource<'a, 'b> {
        ReportWithSource {
            report: self,
            source,
        }
    }
}

impl<'a, 'b> Display for ReportWithSource<'a, 'b> {
    fn fmt(&self, mut f: &mut Formatter) -> fmt::Result {
        let ReportWithSource { report, source } = self;
        // Write the message.
        writeln!(f, "{}", report.message)?;
        // Show the code.
        report.region.show_region(source, &mut f)?;
        // Show the notes bellow.
        for note in &report.notes {
            writeln!(f, "{}", note)?;
            writeln!(f)?;
        }
        // Show the suggestions.
        for suggestion in &report.suggestion {
            writeln!(f, "Suggestion: {}", suggestion)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reporting::annotation::{Position, Region};
    use indoc::indoc;

    #[test]
    pub fn simple_report() {
        let report = Report {
            message: "Something went wrong. :(".into(),
            region: Region(
                Position { line: 1, column: 5 },
                Position { line: 1, column: 7 },
            ),
            notes: vec![],
            suggestion: vec![],
        };
        let code = indoc! {"
        def main:
            print(\"hello world!\");
        "};
        let buf = format!("{}", report.with_source(code));
        assert_eq!(
            &buf,
            indoc! {"
                Something went wrong. :(
                2 |        print(\"hello world!\");
                            ^^^
            "},
        );
    }
}
