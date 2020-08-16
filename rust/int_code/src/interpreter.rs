use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::{fmt::Debug, io::Write};

pub fn interpret(mem: &mut [i32], pos: usize) -> Result<i32, String> {
    let mut prog = decode(mem, pos)?;
    match prog.op_code {
        OpCode::Add => prog.apply_binary(std::ops::Add::add),
        OpCode::Multiply => prog.apply_binary(std::ops::Mul::mul),
        OpCode::Input => {
            let val = read_user_input("Input: ");
            prog.store(0, val);
            prog.ins_ptr += 2;
        }
        OpCode::Output => {
            println!("Output: {}", prog.load(0));
            prog.ins_ptr += 2;
        }
        OpCode::JumpIfTrue => prog.jump(true),
        OpCode::JumpIfFalse => prog.jump(false),
        OpCode::LessThan => prog.apply_binary(|a, b| if a < b { 1 } else { 0 }),
        OpCode::EqualsTo => prog.apply_binary(|a, b| if a == b { 1 } else { 0 }),
        OpCode::Halt => return Ok(mem[0]),
    }

    interpret(prog.memory, prog.ins_ptr)
}

pub fn solve(mem: &mut [i32], pos: usize, result: i32) -> Result<i32, String> {
    for noun in 0..100 {
        for verb in 0..100 {
            let mut tmp_mem = mem.to_vec();
            tmp_mem[1] = noun;
            tmp_mem[2] = verb;
            if interpret(&mut tmp_mem, pos) == Ok(result) {
                return Ok(noun * 100 + verb);
            }
        }
    }
    Err("Program could not be solved.".to_string())
}

#[derive(FromPrimitive, Debug)]
enum OpCode {
    Add = 1,
    Multiply = 2,
    Input = 3,
    Output = 4,
    JumpIfTrue = 5,
    JumpIfFalse = 6,
    LessThan = 7,
    EqualsTo = 8,
    Halt = 99
}

#[derive(FromPrimitive, Copy, Clone, Debug)]
enum ParamMode {
    Positional = 0,
    Immediate = 1
}
struct OpState<'a> {
    op_code: OpCode,
    param_modes: [ParamMode; 3],
    memory: &'a mut [i32],
    ins_ptr: usize,
}

impl<'a> OpState<'a> {
    fn load(&self, index: usize) -> i32 {
        let pos = self.ins_ptr + index + 1;
        match self.param_modes[index] {
            ParamMode::Positional => self.memory[self.memory[pos] as usize],
            ParamMode::Immediate => self.memory[pos]
        }
    }

    fn store(&mut self, index: usize, value: i32) {
        let pos = self.ins_ptr + index + 1;
        match self.param_modes[index] {
            ParamMode::Positional => self.memory[self.memory[pos] as usize] = value,
            ParamMode::Immediate => self.memory[pos] = value
        };
    }

    fn jump(&mut self, test: bool) {
        self.ins_ptr = match self.load(0) != 0 {
            x if x == test => self.load(1) as usize,
            _ => self.ins_ptr + 3
        };
    }

    fn apply_binary<T: Into<i32>>(&mut self, op: fn(i32, i32) -> T) {
        let res = op(self.load(0), self.load(1));
        self.store(2, res.into());
        self.ins_ptr += 4;
    }
}

impl<'a> Debug for OpState<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("instruction: {}, op-code: {:?}, param-modes: {:?}", self.ins_ptr, self.op_code, self.param_modes))
    }
}

fn decode(memory: &mut [i32], ins_ptr: usize) -> Result<OpState, String> {
    let mut instruction = memory[ins_ptr];
    let op_code = OpCode::from_i32(instruction % 100).ok_or(format!("Invalid op-code {} at {}. Aborting.", instruction % 100, ins_ptr))?;

    let mut param_modes: [ParamMode; 3] = [ParamMode::Positional; 3];
    instruction /= 100;
    for (i, mode) in param_modes.iter_mut().enumerate() {
        *mode = ParamMode::from_i32(instruction % 10).ok_or(format!("Invalid parameter mode {} for parameter {} at {}. Aborting.", instruction % 10, i, ins_ptr))?;
        instruction /= 10;
    };

    Ok(OpState{op_code, param_modes, memory, ins_ptr})
}

fn read_user_input(msg: &str) -> i32 {
    print!("{}", msg);
    let _ = std::io::stdout().flush();
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).expect("Unable to read from stdin.");
    match buf.trim().parse() {
        Ok(x) => x,
        Err(_) => {
            eprintln!("Invalid input.");
            read_user_input(msg)
        }
    }
}
