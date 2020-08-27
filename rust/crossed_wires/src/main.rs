use std::env;
use nalgebra::{Vector2, Point2};

enum Metric {
    Manhatten,
    Travel
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let solve_for = match args[1].as_str() {
        "-manhatten" => Metric::Manhatten,
        "-travel" => Metric::Travel,
        s => panic!("Invalid target metric: {}", s)
    };

    let programs: Vec<Vec<&str>> = args[2..].iter().map(|path| path.split(",").collect()).collect();

    let path1: Vec<Vector2<f32>> = programs[0].iter().map(parse).collect();
    let path2: Vec<Vector2<f32>> = programs[1].iter().map(parse).collect();
    let result = solve(&path1, &path2, solve_for);

    println!("{}", result);
}

fn parse(instruction: &&str) -> Vector2<f32> {
    match (instruction.chars().nth(0), instruction[1..].parse::<i32>()) {
        (Some('R'), Ok(dist)) => Vector2::new(dist as f32, 0f32),
        (Some('L'), Ok(dist)) => Vector2::new(-dist as f32, 0f32),
        (Some('U'), Ok(dist)) => Vector2::new(0f32, dist as f32),
        (Some('D'), Ok(dist)) => Vector2::new(0f32, -dist as f32),
        _ => panic!("Invalid instruction: {}", instruction),
    }
}

fn solve(path1: &[Vector2<f32>], path2: &[Vector2<f32>], solve_for: Metric) -> f32 {
    let mut result = f32::INFINITY;
    let mut pos1 = Point2::new(0f32,0f32);
    let mut track1 = 0f32;

    for &delta1 in path1 {
        let mut pos2 = Point2::new(0f32,0f32);
        let mut track2 = 0f32;

        for &delta2 in path2 {
            let pos_diff = pos2 - pos1;
            let denom = cross(&delta1, &delta2);
            let t = cross(&pos_diff, &delta2) / denom;
            let u = cross(&pos_diff, &delta1) / denom;

            if t >= 0f32 && t <= 1f32 && u >= 0f32 && u <= 1f32 {
                let intersection = pos1 + delta1.scale(t);
                let new_result = match solve_for {
                    Metric::Manhatten => intersection.x.abs() + intersection.y.abs(),
                    Metric::Travel => track1 + track2 + delta1.scale(t).abs().sum() + delta2.scale(u).abs().sum()
                };

                if new_result > 0f32 {
                    result = result.min(new_result);
                }
            }

            track2 += delta2.abs().sum();
            pos2 += delta2;
        }
        track1 += delta1.abs().sum();
        pos1 += delta1;
    }

    result
}

fn cross(a: &Vector2<f32>, b: &Vector2<f32>) -> f32 {
    a.x * b.y - a.y * b.x
}