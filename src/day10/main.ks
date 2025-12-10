#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

const State = int32;
const Machine = type (
    .target_state :: State,
    .buttons :: list.t[State],
    .joltages :: list.t[int32],
);
let parse_machine = s -> Machine => (
    let target_state = 0;
    let buttons = list.create ();
    let joltages = list.create ();
    String.split (
        s,
        ' ',
        part => (
            let parens = String.at (part, 0);
            let inside = String.substring (part, 1, String.length part - 2);
            if parens == '[' then (
                String.iter (
                    inside,
                    c => (
                        let c = if c == '#' then 1 else 0;
                        target_state = std.op.bit_shift_left (target_state, 1) + c;
                    ),
                );
            ) else if parens == '(' then (
                let affected_lights = 0;
                String.split (
                    inside,
                    ',',
                    idx => (
                        let idx :: int32 = idx |> parse;
                        affected_lights = std.op.bit_or (
                            affected_lights,
                            std.op.bit_shift_left (1, idx),
                        );
                    ),
                );
                list.push_back (&buttons, affected_lights);
            ) else if parens == '{' then (
                String.split (
                    inside,
                    ',',
                    x => list.push_back (&joltages, x |> parse),
                );
            ) else (
                panic "unexpected char"
            );
        ),
    );
    (.target_state, .buttons, .joltages)
);

String.lines (
    input,
    line => with_return (
        if String.length line == 0 then return;
        let machine = parse_machine line;
        # dbg.print machine;
    ),
);
