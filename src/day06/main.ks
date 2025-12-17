#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let int32_as_Int64 = value => (
    value |> to_string |> parse
);

const Op = type (
    | :Add
    | :Multiply
    | :Unknown
);

const Problem = type (
    .numbers :: List.t[Int64],
    .op :: Op,
);

let mut problems = List.create ();

if part1 then (
    String.lines (
        input,
        line => (
            let mut idx = 0;
            String.split (
                line,
                ' ',
                part => with_return (
                    if String.length part == 0 then return;
                    if idx >= List.length &problems then (
                        List.push_back (
                            &mut problems,
                            (
                                .numbers = List.create (),
                                .op = :Unknown,
                            ),
                        );
                    );
                    let problem = List.at_mut (&mut problems, idx);
                    if part == "+" then (
                        problem^.op = :Add;
                    ) else if part == "*" then (
                        problem^.op = :Multiply;
                    ) else (
                        List.push_back (&mut problem^.numbers, parse part);
                    );
                    idx += 1;
                ),
            )
        ),
    );
) else (
    let mut lines = List.create ();
    String.lines (
        input,
        line => (
            if String.length line != 0 then (
                List.push_back (&mut lines, line);
            );
        ),
    );
    let line_length = String.length (List.at (&lines, 0))^;
    let mut start_column = 0;
    while start_column < line_length do (
        let mut end_column = start_column;
        while end_column < line_length do (
            let mut full_of_spaces = true;
            List.iter (
                &lines,
                &line => (
                    if String.at (line, end_column) != ' ' then (
                        full_of_spaces = false;
                    );
                )
            );
            if full_of_spaces then break;
            end_column += 1;
        );
        let mut numbers = List.create ();
        let mut op :: Op = :Unknown;
        for column in start_column..end_column do (
            let mut number = 0;
            List.iter (
                &lines,
                &line => (
                    let c = String.at (line, column);
                    if c == '+' then (
                        op = :Add;
                    ) else if c == '*' then (
                        op = :Multiply;
                    ) else if c != ' ' then (
                        number = number * 10 + Char.to_digit c;
                    );
                ),
            );
            List.push_back (&mut numbers, int32_as_Int64 number);
        );
        List.push_back (&mut problems, (.numbers, .op));
        start_column = end_column + 1;
    );
);

let calculate_problem = (problem :: &Problem) -> Int64 => (
    let start, combine = match problem^.op with (
        | :Add => (int32_as_Int64 0, std.op.add[Int64])
        | :Multiply => (int32_as_Int64 1, std.op.mul[Int64])
    );
    let mut result = start;
    List.iter (
        &problem^.numbers,
        &number => (
            result = combine (result, number);
        ),
    );
    result
);

let mut answer = 0 |> int32_as_Int64;
List.iter (
    &problems,
    problem => (
        answer += calculate_problem problem;
    ),
);
dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "4277556", .part2 = parse "3263827"),
    .part1 = parse "6299564383938",
    .part2 = parse "11950004808442",
);
