#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

use std.collections.queue;

@syntax "as" 62 wrap never = value " " "as" " " type;
impl syntax (value as ty) = `(
    string_to_int64 (int32_to_string $value) :: $ty
);

let k = if part1 then 2 else 12;

# let f = () => (
# list |> map (x => if x < 0 then return :Error)
# );
let answer = 0 as int64;
String.lines (
    input,
    line => with_return (
        let n = String.length line;
        if n == 0 then return;
        
        dbg.print line;
        
        let start = 0;
        let line_answer = 0 as int64;
        
        let data = (
            let current_start = 0;
            let current_end = 0;
            let positions_by_digit = list.create[queue.t[int32]] ();
            for _ in 0..10 do (
                list.push_back (
                    &positions_by_digit,
                    queue.create (),
                );
            );
            let move_start_to = i => (
                while current_start < i do (
                    let pos = current_start;
                    let digit = String.at (line, pos)
                        |> Char.to_digit;
                    let positions = list.at (&positions_by_digit, digit);
                    let first = queue.pop positions;
                    if first != pos then (
                        panic "bug";
                    );
                    # dbg.print ("pop", digit, pos);
                    # queue.iter (list.at (positions_by_digit, digit), dbg.print[_]);
                    current_start += 1;
                );
            );
            let move_end_to = i => (
                while current_end < i do (
                    let pos = current_end;
                    let digit = String.at (line, pos)
                        |> Char.to_digit;
                    let positions = list.at (&positions_by_digit, digit);
                    queue.push (
                        positions,
                        pos,
                    );
                    # dbg.print ("push", digit, pos);
                    # queue.iter (list.at (positions_by_digit, digit), dbg.print[_]);
                    current_end += 1;
                );
            );
            let max_in_range = () -> (.index :: int32, .value :: int32) => with_return (
                let digit = 9;
                while digit >= 0 do (
                    let positions = list.at (&positions_by_digit, digit);
                    if queue.length positions != 0 then (
                        return (
                            .index = (queue.front positions)^,
                            .value = digit,
                        );
                    );
                    digit -= 1;
                );
                panic "nothing found"
            );
            (
                .move_start_to,
                .move_end_to,
                .max_in_range,
            )
        );
        
        for i in 0..k do (
            data.move_end_to (n - k + i + 1);
            let (
                .index = new_start,
                .value = next_digit,
            ) = data.max_in_range ();
            data.move_start_to (new_start + 1);
            line_answer = line_answer * 10 as int64 + next_digit as int64;
        );
        
        dbg.print line_answer;
        answer += line_answer;
    ),
);
dbg.print answer;
