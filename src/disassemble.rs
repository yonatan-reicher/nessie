use std::fmt::Display;
use std::io::{self, Write};
use crate::chunk::{Chunk, OpCode};


struct Offsets {
    instruction: usize,
    data: usize,
}


pub fn disassamble<W>(out: &mut W, chunk: &Chunk, name: &str) -> io::Result<()>
where W: Write {
    writeln!(out, "== {} ==", name)?;
    
    let mut offsets = Offsets {
        instruction: 0,
        data: 0,
    };
    while offsets.instruction < chunk.instructions.len() {
        disassamble_line(out, chunk, &mut offsets)?;
        write!(out, "\n")?;
    }
    Ok(())
}

fn disassamble_line<W>(out: &mut W, chunk: &Chunk, offsets: &mut Offsets)
-> io::Result<()> where W: Write {
    // read the opcode
    let op = chunk.instructions[offsets.instruction];
    write!(out, "{:04} {}", offsets.instruction, op)?;
    offsets.instruction += 1;

    match op {
        OpCode::Return => (),
        OpCode::Constant => {
            let constant = chunk.data[offsets.data];
            offsets.data += 1;
            write!(out, " {}", constant)?;
        },
    }

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
        chunk.write(OpCode::Return, &[]);
        chunk.write(OpCode::Constant, &[1]);

        disassamble(&mut out, &chunk, "test").expect("disassamble failed");

        assert_eq!(
            String::from_utf8(out).unwrap(),
            indoc!("
                == test ==
                0000 Return
                0001 Constant 1
            ")
        );
    }
}
