#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

const State = int32;
const Machine = type (
    .lights :: int32,
    .target_state :: State,
    .buttons :: list.t[State],
    .joltages :: list.t[int32],
);
let parse_machine = s -> Machine => (
    let lights = 0;
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
                lights = String.length inside;
                
                @syntax "for_range_rev" 7.5 wrap never = "for" " " var " " "in" " " "(" start " " ".." " " end ")" "." "rev" "(" ")" " " "do" " " body;
                impl syntax (for i in (start .. end).rev() do body) = `(
                    let _loop_var = $end;
                    while _loop_var > $start do (
                        _loop_var -= 1;
                        let $i = _loop_var;
                        $body;
                    )
                );
                
                for i in (0 .. String.length inside).rev() do (
                    let c = String.at (inside, i);
                    let c = if c == '#' then 1 else 0;
                    target_state = std.op.bit_shift_left (target_state, 1) + c;
                );
                # print ("[INFO] read target_state=" + to_string target_state);
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
    (.lights, .target_state, .buttons, .joltages)
);

let abs = x => if x < 0 then -x else x;

let idx = 0;
let answer = 0;
String.lines (
    input,
    line => with_return (
        if String.length line == 0 then return;
        let machine = parse_machine line;
        
        idx += 1;
        print ("[INFO] read machine #" + to_string idx);
        
        use std.collections.queue;
        let q = queue.create ();
        let d = list.create ();
        for _ in 0..std.op.bit_shift_left (1, machine.lights) do (
            list.push_back (&d, 0);
        );
        
        print "[INFO] starting bfs";
        (list.at (&d, 0))^ = 1;
        queue.push (&q, 0);
        (list.at (&d, machine.target_state))^ = -1;
        queue.push (&q, machine.target_state);
        let machine_answer = -1;
        while queue.length &q != 0 do (
            let state = queue.pop &q;
            # if state == machine.target_state then break;
            # dbg.print state;
            let state_d = (list.at (&d, state))^;
            let dir = if state_d < 0 then -1 else +1;
            list.iter (
                &machine.buttons,
                &changes => (
                    let new_state = std.op.bit_xor (state, changes);
                    let new_state_d = list.at (&d, new_state);
                    if new_state_d^ == 0 then (
                        new_state_d^ = state_d + dir;
                        queue.push (&q, new_state);
                    ) else if (new_state_d^ < 0) != (state_d < 0) then (
                        machine_answer = abs (state_d) + abs (new_state_d^) - 1;
                        break;
                    );
                )
            );
        );
        if machine_answer < 0 then (
            panic "couldnt solve the machine";
        );
        answer += machine_answer;
        # dbg.print machine;
    ),
);
dbg.print answer;
