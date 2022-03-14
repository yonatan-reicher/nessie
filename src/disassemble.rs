use std::fmt::Display;
use std::io::{self, Write};
use crate::chunk::{Chunk, Instruction};


pub fn disassamble<W>(out: &mut W, chunk: &Chunk, name: &str) -> io::Result<()>
where W: Write {
    writeln!(out, "== {} ==", name)?;
    
    let mut offset = 0;
    while offset < chunk.instructions().len() {
        disassamble_instruction(out, chunk, offset)?;
        offset += 1;
        writeln!(out)?;
    }
    Ok(())
}

pub fn disassamble_instruction<W>(out: &mut W, chunk: &Chunk, offset: usize)
-> io::Result<()> where W: Write {
    // read the opcode
    let op = chunk.instructions()[offset];
    let line_string = {
        let lines = chunk.instruction_lines();
        if offset == 0 || lines[offset] != lines[offset - 1] {
            lines[offset].to_string()
        } else {
            "  |".to_string()
        }
    };
    write!(out, "{:04} {:>4} {}", offset, line_string, op)?;

    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_disassamble() {
        let mut out = Vec::new();

        let mut chunk = Chunk::new();
        chunk.write(Instruction::Return, 123);
        chunk.write(Instruction::Constant(0), 123);

        disassamble(&mut out, &chunk, "test").expect("disassamble failed");

        assert_eq!(
            String::from_utf8(out).unwrap(),
            indoc!("
                == test ==
                0000  123 Return
                0001    | Constant(0)
            ")
        );
    }
}
