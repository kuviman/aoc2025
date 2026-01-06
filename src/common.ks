use std.prelude.*;
let mut part1 = true;
let mut input_path = "example.txt";
(
    dbg.print (.argc = std.sys.argc ());
    for i in 1..std.sys.argc () do (
        let arg = std.sys.argv_at i;
        dbg.print (.arg);
        if arg == "--part2" then (
            part1 = false;
        ) else if arg == "--part1" then (
            part1 = true;
        ) else (
            input_path = arg;
        );
    );
);
let part2 = not part1;

let assert = x => (
    if not x then (
        panic "assertion failed";
    );
);

let assert_answers = (
    answer,
    .example :: (.part1 :: _, .part2 :: _),
    .part1 = expected_answer_part1,
    .part2 = expected_answer_part2,
) => (
    let expected = if input_path != "input.txt" then (
        example
    ) else (
        .part1 = expected_answer_part1,
        .part2 = expected_answer_part2,
    );
    let expected = if part1 then expected.part1 else expected.part2;
    if answer != expected then (
        print (
            "Expected "
            + to_string expected
            + ", got "
            + to_string answer
        );
        print (input_path + " " + (if part1 then "part1" else "part2"));
        std.sys.exit (-1);
    );
);

use String.parse;
