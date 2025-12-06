#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let int32_as_int64 = value => (
    value |> to_string |> parse
);

const Op = type (
    | :Add
    | :Multiply
    | :Unknown
);

const Problem = type (
    .numbers :: list.t[int64],
    .op :: Op,
);

let problems :: list.t[Problem] = list.create ();

if part1 then (
    String.lines (
        input,
        line => (
            let idx = 0;
            String.split (
                line,
                ' ',
                part => with_return (
                    if String.length part == 0 then return;
                    if idx >= list.length &problems then (
                        list.push_back (
                            &problems,
                            (
                                .numbers = list.create (),
                                .op = :Unknown,
                            ),
                        );
                    );
                    let problem = list.at (&problems, idx);
                    if part == "+" then (
                        problem^.op = :Add;
                    ) else if part == "*" then (
                        problem^.op = :Multiply;
                    ) else (
                        list.push_back (&problem^.numbers, parse part);
                    );
                    idx += 1;
                ),
            )
        ),
    );
) else (
    let lines = list.create ();
    String.lines (
        input,
        line => (
            if String.length line != 0 then (
                list.push_back (&lines, line);
            );
        ),
    );
    let line_length = String.length (list.at (&lines, 0))^;
    let start_column = 0;
    while start_column < line_length do (
        let end_column = start_column;
        while end_column < line_length do (
            let full_of_spaces = true;
            list.iter (
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
        let numbers = list.create ();
        let op :: Op = :Unknown;
        for column in start_column..end_column do (
            let number = 0;
            list.iter (
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
            list.push_back (&numbers, int32_as_int64 number);
        );
        list.push_back (&problems, (.numbers, .op));
        start_column = end_column + 1;
    );
);

let calculate_problem = (problem :: &Problem) -> int64 => (
    let start, combine = match problem^.op with (
        | :Add => (int32_as_int64 0, std.op.add[int64])
        | :Multiply => (int32_as_int64 1, std.op.mul[int64])
    );
    let result = start;
    list.iter (
        &problem^.numbers,
        &number => (
            result = combine (result, number);
        ),
    );
    result
);

let answer = 0 |> int32_as_int64;
list.iter (
    &problems,
    problem => (
        answer += calculate_problem problem;
    ),
);
dbg.print answer;
