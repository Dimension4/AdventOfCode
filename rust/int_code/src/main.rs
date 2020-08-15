mod interpreter;

use std::env;

fn create_memory(input: &[String]) -> Vec<i32> {
    input.join("").split(",").map(|s| s.parse::<i32>().unwrap()).collect()
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let res = match args[1].as_str() {
        "-solve" => {
            let result: i32 = args[2].parse().unwrap();
            let mut mem = create_memory(&args[3..]);
            interpreter::solve(&mut mem, 0, result)
        }
        _ => {
            let mut mem = create_memory(&args[1..]);
            interpreter::interpret(&mut mem, 0)
        }
    };

    match res {
        Ok(result) => println!("Result: {}", result),
        Err(msg) => eprintln!("{}", msg)
    }
}
