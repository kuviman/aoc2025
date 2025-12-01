#!/usr/bin/env kast
use std.prelude.*;
use std.String;
@syntax "add_assign" 15 wrap never = _ " " "+=" " " _;
impl syntax (a += b) = `(
    $a = $a + $b
);
@syntax "sub_assign" 15 wrap never = _ " " "-=" " " _;
impl syntax (a -= b) = `(
    $a = $a - $b
);
@syntax "while" 7.5 wrap never = "while" " " cond " " "do" " " body;
impl syntax (while cond do body) = `(
    loop (
        if $cond then $body else break
    )
);
std.sys.chdir (std.path.dirname __FILE__);
const list = std.collections.treap;
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
    let direction_char = String.get_at (s, 0);
    let direction :: Direction = if direction_char == 'R' then (
        :Right
    ) else if direction_char == 'L' then (
        :Left
    ) else (
        # TODO panic
        _
    );
    let distance = String.substring (s, 1, String.length s - 1)
        |> string_to_int32;
    (
        .direction,
        .distance,
    )
);
let read_input = () -> Input => (
    std.sys.argc () |> std.dbg.print;
    let path = if std.sys.argc () >= 2 then (
        std.sys.argv_at 1
    ) else (
        "example.txt"
    );
    let input = std.fs.read_file path;
    let result = list.create ();
    let line_idx = 0;
    String.lines (
        input,
        line => (
            if line != "" then (
                std.dbg.print line_idx;
                line_idx += 1;
                # std.dbg.print line;
                result = list.merge (
                    result,
                    list.singleton <| parse_instruction line,
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
    if delta > 0 then (
        answer += (current_position + delta) / 100;
    ) else (
        let zero = 0;
        let target = current_position + delta;
        while zero >= target do (
            if current_position != zero then answer += 1;
            zero -= 100;
        );
    );
    current_position += delta;
    while current_position < 0 do current_position += 100;
    while current_position >= 100 do current_position -= 100;
    current_position
);
list.iter (
    input,
    instruction => (
        rotate <| instruction_delta instruction;
        # std.dbg.print current_position;
        # if current_position == 0 then (
        #     answer += 1;
        # )
    ),
);
std.dbg.print answer;
