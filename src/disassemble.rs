use std::io::{self, Write};
use crate::chunk::Chunk;


pub fn disassemble<W>(mut out: W, chunk: &Chunk) -> io::Result<()>
where W: Write {
    chunk_header(&mut out, chunk)?;
    
    let mut offset = 0;
    while offset < chunk.instructions().len() {
        disassemble_instruction(&mut out, chunk, offset, "")?;
        offset += 1;
    }
    Ok(())
}

pub fn chunk_header<W>(mut out: W, chunk: &Chunk) -> io::Result<()>
where W: Write
{
    writeln!(out, "== {} ==", chunk.name().unwrap_or("<unknown>"))?;
    writeln!(out, "constants: {}, instructions: {}", chunk.constants().len(), chunk.instructions().len())?;
    Ok(())
}

pub fn disassemble_instruction<W>(mut out: W, chunk: &Chunk, offset: usize, more: &str)
-> io::Result<()> where W: Write {
    let line_string = {
        let lines = chunk.instruction_lines();
        if offset == 0 || lines[offset] != lines[offset - 1] {
            (lines[offset] + 1).to_string()
        } else {
            "  |".to_string()
        }
    };
    let op = format!("{}", chunk.instructions()[offset]);
    writeln!(out, "{:04} {:>4} {:<20} {}", offset, line_string, op, more)?;

    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::Instruction;
    use indoc::indoc;

    
    /// Trim the end of each line of the given string.
    fn trim_line_ends(s: &str) -> String {
        s.lines().map(|line| line.trim_end()).collect::<Vec<_>>().join("\n")
        + (if s.ends_with('\n') { "\n" } else { "" })
    }

    #[test]
    fn test_disassamble() {
        let mut chunk = Chunk::new();
        chunk.write(Instruction::Return, 123);
        chunk.write(Instruction::Constant(0), 123);

        let mut out = Vec::new();
        disassemble(&mut out, &chunk).expect("disassamble failed");
        let out = String::from_utf8(out).expect("output is not utf8");
        let out = trim_line_ends(&out);

        assert_eq!(
            out,
            indoc!("
                == <unknown> ==
                constants: 0, instructions: 2
                0000  124 Return
                0001    | Constant(0)
            ")
        );
    }
}
