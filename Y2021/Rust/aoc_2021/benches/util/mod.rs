macro_rules! benchmark {
    ($module:ident::$puzzle:ident) => {
        use std::path::Path;

        use criterion::Criterion;

        use aoc_2021::{puzzles::$module::$puzzle, Puzzle};

        pub fn $module(c: &mut Criterion) {
            let input = Path::new(stringify!($puzzle)).join("input.txt");
            let file = &*utils::input_dir(input).unwrap();

            c.bench_function(stringify!($module), |b| b.iter(|| $puzzle.solve_file(file)));
        }

        criterion::criterion_group!(benches, $module);
        criterion::criterion_main!(benches);
    };
}

pub(crate) use benchmark;
