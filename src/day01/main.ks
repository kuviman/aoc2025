#!/usr/bin/env kast
include "../common.ks";
std.sys.chdir (std.path.dirname __FILE__);
const Direction = type (
    | :Left
    | :Right
);
const Instruction = type (
    .direction :: Direction,
    .distance :: Int32,
);
let instruction_delta = (instruction :: Instruction) -> Int32 => (
    match instruction.direction with (
        | :Left => -instruction.distance
        | :Right => instruction.distance
    )
);
const Input = List.t[Instruction];
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
        |> String.parse;
    (
        .direction,
        .distance,
    )
);
let read_input = () -> Input => (
    let input = std.fs.read_file input_path;
    let mut result = List.create ();
    let mut line_idx :: Int32 = 0;
    for line in String.lines input do (
        if line == "" then continue;
        std.dbg.print line_idx;
        line_idx += 1;
        # std.dbg.print line;
        List.push_back (
            &mut result,
            parse_instruction line,
        );
    );
    result
);
let input = read_input ();
# List.iter (input, std.dbg.print[_])
let mut current_position = 50;
let mut answer = 0;
let rotate = (delta) => (
    if part2 then (
        if delta > 0 then (
            answer += (current_position + delta) / 100;
        ) else (
            let mut zero = 0;
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
for &instruction in List.iter &input do (
    rotate <| instruction_delta instruction;
    # std.dbg.print current_position;
    if part1 then (
        if current_position == 0 then (
            answer += 1;
        );
    );
);
std.dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = 3, .part2 = 6),
    .part1 = 1086,
    .part2 = 6268,
);
