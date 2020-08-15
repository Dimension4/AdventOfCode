use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(FromPrimitive)]
enum OpCode {
    Add = 1,
    Multiply = 2,
    Halt = 99
}

pub fn interpret(mem: &mut Vec<i32>, pos: usize) -> Result<i32, String> {
    let op_code = mem[pos];

    match FromPrimitive::from_i32(op_code) {
        Some(OpCode::Add) => {
            let dest = mem[pos + 3] as usize;
            mem[dest] = mem[mem[pos + 1] as usize] + mem[mem[pos + 2] as usize];
            interpret(mem, pos + 4)
        }
        Some(OpCode::Multiply) => {
            let dest = mem[pos + 3] as usize;
            mem[dest] = mem[mem[pos + 1] as usize] * mem[mem[pos + 2] as usize];
            interpret(mem, pos + 4)
        }
        Some(OpCode::Halt) => Ok(mem[0]),
        None => Err(format!("Invalid op-code: {}. Aborting.", op_code))
    }
}

pub fn solve(mem: &mut Vec<i32>, pos: usize, result: i32) -> Result<i32, String> {
    for noun in 0..100 {
        for verb in 0..100 {
            let mut tmp_mem = mem.clone();
            tmp_mem[1] = noun;
            tmp_mem[2] = verb;
            match interpret(&mut tmp_mem, pos) {
                Ok(res) if res == result => {
                    return Ok(noun * 100 + verb);
                }
                _ => {}
            }
        }
    }

    Err("Program could not be solved.".to_string())
}