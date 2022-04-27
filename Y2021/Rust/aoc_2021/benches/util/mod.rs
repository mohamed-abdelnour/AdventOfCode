macro_rules! benchmark {
    ($module:ident::$puzzle:ident) => {
        use criterion::Criterion;

        use aoc_2021::puzzles::$module::$puzzle;
        use aoc_2021::Puzzle;

        pub fn $module(c: &mut Criterion) {
            c.bench_function(stringify!($module), |b| {
                let file = &*format!("../Inputs/{}/input.txt", stringify!($puzzle));
                b.iter(|| $puzzle.solve_file(file))
            });
        }

        criterion::criterion_group!(benches, $module);
        criterion::criterion_main!(benches);
    };
}

pub(crate) use benchmark;
