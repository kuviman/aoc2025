#!/usr/bin/env kast
include "../common.ks";
std.sys.chdir(std.path.dirname(__FILE__));
let input = std.fs.read_file(input_path);
let abs = x => if x < 0 then -x else x;
let max = (a, b) => if a > b then a else b;
let min = (a, b) => if a < b then a else b;
const Shape = newtype {
    .rows :: ArrayList.t[String],
};
let mut shapes :: ArrayList.t[Shape] = ArrayList.new();
let print_shape = (shape :: &Shape) => (
    for &s in ArrayList.iter(&shape^.rows) do (
        print(s);
    );
);
let mut trivial = 0;
let mut actually_need_to_solve = 0;
let mut max_extra_space_needed = 0;
let mut min_optimal_free_area = 1000000000;
let mut answer = 0;
let solve = (.width :: Int32, .height :: Int32, .amounts :: ArrayList.t[Int32]) => with_return (
    let mut shape_tiles = ArrayList.new();
    for &shape in ArrayList.iter(&shapes) do (
        let mut tiles = 0;
        for &s in ArrayList.iter(&shape.rows) do (
            for c in String.iter(s) do (
                if c == '#' then (
                    tiles += 1;
                );
            );
        );
        
        ArrayList.push_back(&mut shape_tiles, tiles);
    );
    let mut dumb_area = 0;
    let mut optimal_area = 0;
    let mut i = 0;
    for &amount in ArrayList.iter(&amounts) do (
        dumb_area += 3 * 3 * amount;
        optimal_area += (ArrayList.at(&shape_tiles, i))^ * amount;
        i += 1;
    );
    let area = width * height;
    let extra_space_needed = dumb_area - area;
    let optimal_free_area = area - optimal_area;
    if optimal_free_area < 0 then (
        trivial += 1;
        return;
    );
    if extra_space_needed <= 0 then (
        answer += 1;
        trivial += 1;
        return;
    );
    
    actually_need_to_solve += 1;
    max_extra_space_needed = max(max_extra_space_needed, extra_space_needed);
    min_optimal_free_area = min(min_optimal_free_area, optimal_free_area);
    print(
        "[INFO] solving for "
        + to_string(width)
        + "x"
        + to_string(height)
        + ": "
        + ArrayList.to_string(&amounts, &x => to_string(x))
    );
    
    print("dumb area = " + to_string(dumb_area));
    print("optimal area = " + to_string(optimal_area));
    print("optimal free area = " + to_string(optimal_free_area));
    print("area = " + to_string(area));
    print("extra_space_needed = " + to_string(extra_space_needed));
);
let end_of_input = () => (
    dbg.print(
        {
            .trivial,
            .actually_need_to_solve,
            .max_extra_space_needed,
            .min_optimal_free_area,
            .answer,
        }
    );
);
let new_shape = () -> Shape => { .rows = ArrayList.new() };
let mut current_shape :: Shape = new_shape();
for line in String.lines(input) do (
    if String.length(line) == 0 then continue;
    if line |> String.index_of(':') == -1 then (
        ArrayList.push_back(&mut current_shape.rows, line);
    ) else (
        let { before_colon, after_colon } = String.split_once(line, ':');
        if ArrayList.length(&current_shape.rows) != 0 then (
            print("[INFO] read shape:");
            print_shape(&current_shape);
            ArrayList.push_back(&mut shapes, current_shape);
            current_shape = new_shape();
        );
        if String.length(after_colon) != 0 then (
            let { width, height } = String.split_once(before_colon, 'x');
            let width = width |> parse;
            let height = height |> parse;
            let mut amounts = ArrayList.new();
            for part in String.split(after_colon, ' ') do (
                if String.length(part) == 0 then continue;
                ArrayList.push_back(&mut amounts, part |> parse);
            );
            
            solve(.width, .height, .amounts);
        )
    );
);

end_of_input();

assert_answers(
    answer,
    .example = { .part1 = -1, .part2 = -1 },
    .part1 = 495,
    .part2 = -1,
);
