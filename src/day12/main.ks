#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let abs = x => if x < 0 then -x else x;
let max = (a, b) => if a > b then a else b;
let min = (a, b) => if a < b then a else b;

const Shape = type (
    .rows :: List.t[String],
);

let shapes :: List.t[Shape] = List.create ();

let print_shape = (shape :: &Shape) => (
    List.iter (&shape^.rows, &s => print s);
);

let trivial = 0;
let actually_need_to_solve = 0;
let max_extra_space_needed = 0;
let min_optimal_free_area = 1000000000;
let answer = 0;
let solve = (.width :: Int32, .height :: Int32, .amounts :: List.t[Int32]) => with_return (
    let shape_tiles = List.create ();
    List.iter (
        &shapes,
        &shape => (
            let tiles = 0;
            List.iter (
                &shape.rows,
                &s => String.iter (
                    s,
                    c => (
                        if c == '#' then (
                            tiles += 1;
                        );
                    ),
                ),
            );
            List.push_back (&shape_tiles, tiles);
        ),
    );
    
    let dumb_area = 0;
    let optimal_area = 0;
    let i = 0;
    List.iter (
        &amounts,
        &amount => (
            dumb_area += 3 * 3 * amount;
            optimal_area += (List.at (&shape_tiles, i))^ * amount;
            i += 1;
        ),
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
    max_extra_space_needed = max (max_extra_space_needed, extra_space_needed);
    min_optimal_free_area = min (min_optimal_free_area, optimal_free_area);
    print (
        "[INFO] solving for "
        + to_string width
        + "x"
        + to_string height
        + ": "
        + std.collections.Treap.to_string (&amounts.inner, &x => to_string x)
    );
    print ("dumb area = " + to_string dumb_area);
    print ("optimal area = " + to_string optimal_area);
    print ("optimal free area = " + to_string optimal_free_area);
    print ("area = " + to_string area);
    print ("extra_space_needed = " + to_string extra_space_needed);
);

let end_of_input = () => (
    dbg.print (
        .trivial,
        .actually_need_to_solve,
        .max_extra_space_needed,
        .min_optimal_free_area,
        .answer,
    );
);

let new_shape = () -> Shape => (.rows = List.create ());
let current_shape :: Shape = new_shape ();
String.lines (
    input,
    line => with_return (
        if String.length line == 0 then return;
        if String.index_of (':', line) == -1 then (
            List.push_back (&current_shape.rows, line);
        ) else (
            let before_colon, after_colon = String.split_once (line, ':');
            if List.length (&current_shape.rows) != 0 then (
                print "[INFO] read shape:";
                print_shape (&current_shape);
                List.push_back (&shapes, current_shape);
                current_shape = new_shape ();
            );
            if String.length after_colon != 0 then (
                let width, height = String.split_once (before_colon, 'x');
                let width = width |> parse;
                let height = height |> parse;
                let amounts = List.create ();
                String.split (
                    after_colon,
                    ' ',
                    part => with_return (
                        if String.length part == 0 then return;
                        List.push_back (&amounts, part |> parse);
                    ),
                );
                solve (.width, .height, .amounts);
            )
        );
    ),
);
end_of_input ();

assert_answers (
    answer,
    .example = (.part1 = -1, .part2 = -1),
    .part1 = 495,
    .part2 = -1,
);
