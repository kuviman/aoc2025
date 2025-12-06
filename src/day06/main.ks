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
