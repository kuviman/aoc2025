#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
const Direction = type (
    | :Left
    | :Right
);
const Instruction = type (
    .direction :: Direction,
    .distance :: int32,
);
let instruction_delta = (instruction :: Instruction) -> int32 => (
    match instruction.direction with (
        | :Left => 0 - instruction.distance
        | :Right => instruction.distance
    )
);
const Input = list.t[Instruction];
let parse_instruction = s -> Instruction => (
    let direction_char = String.at (s, 0);
    let direction :: Direction = if direction_char == 'R' then (
        :Right
    ) else if direction_char == 'L' then (
        :Left
    ) else (
        panic "expected L or R"
    );
    let distance = String.substring (s, 1, String.length s - 1)
        |> parse;
    (
        .direction,
        .distance,
    )
);
let read_input = () -> Input => (
    let input = std.fs.read_file input_path;
    let result = list.create ();
    let line_idx = 0;
    String.lines (
        input,
        line => (
            if line != "" then (
                std.dbg.print line_idx;
                line_idx += 1;
                # std.dbg.print line;
                list.push_back (
                    &result,
                    parse_instruction line,
                );
            );
        )
    );
    result
);
let input = read_input ();
# list.iter (input, std.dbg.print[_])
let current_position = 50;
let answer = 0;
let rotate = (delta) => (
    if part2 then (
        if delta > 0 then (
            answer += (current_position + delta) / 100;
        ) else (
            let zero = 0;
            let target = current_position + delta;
            while zero >= target do (
                if current_position != zero then (
                    answer += 1
                );
                zero -= 100;
            );
        );
    );
    current_position += delta;
    while current_position < 0 do (
        current_position += 100;
    );
    while current_position >= 100 do (
        current_position -= 100;
    );
    current_position
);
list.iter (
    &input,
    &instruction => (
        rotate <| instruction_delta instruction;
        # std.dbg.print current_position;
        if part1 then (
            if current_position == 0 then (
                answer += 1;
            );
        );
    ),
);
std.dbg.print answer;
