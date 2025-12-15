#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

use std.collections.Queue;

@syntax "as_int64" 62 wrap never = value " " "as_int64";
impl syntax (value as_int64) = `(
    parse (to_string $value)
);

let k = if part1 then 2 else 12;

# let f = () => (
# List |> map (x => if x < 0 then return :Error)
# );
let answer :: Int64 = 0 as_int64;
String.lines (
    input,
    line => with_return (
        let n = String.length line;
        if n == 0 then return;
        
        dbg.print line;
        
        let start = 0;
        let line_answer = 0 as_int64;
        
        let data = (
            let current_start = 0;
            let current_end = 0;
            let positions_by_digit = List.create[Queue.t[Int32]] ();
            for _ in 0..10 do (
                List.push_back (
                    &positions_by_digit,
                    Queue.create (),
                );
            );
            let move_start_to = i => (
                while current_start < i do (
                    let pos = current_start;
                    let digit = String.at (line, pos)
                        |> Char.to_digit;
                    let positions = List.at (&positions_by_digit, digit);
                    let first = Queue.pop positions;
                    if first != pos then (
                        panic "bug";
                    );
                    # dbg.print ("pop", digit, pos);
                    # Queue.iter (List.at (positions_by_digit, digit), dbg.print[_]);
                    current_start += 1;
                );
            );
            let move_end_to = i => (
                while current_end < i do (
                    let pos = current_end;
                    let digit = String.at (line, pos)
                        |> Char.to_digit;
                    let positions = List.at (&positions_by_digit, digit);
                    Queue.push (
                        positions,
                        pos,
                    );
                    # dbg.print ("push", digit, pos);
                    # Queue.iter (List.at (positions_by_digit, digit), dbg.print[_]);
                    current_end += 1;
                );
            );
            let max_in_range = () -> (.index :: Int32, .value :: Int32) => with_return (
                let digit = 9;
                while digit >= 0 do (
                    let positions = List.at (&positions_by_digit, digit);
                    if Queue.length positions != 0 then (
                        return (
                            .index = (Queue.front positions)^,
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
            line_answer = line_answer * 10 as_int64 + next_digit as_int64;
        );
        
        dbg.print line_answer;
        answer += line_answer;
    ),
);
dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "357", .part2 = parse "3121910778619"),
    .part1 = parse "17144",
    .part2 = parse "170371185255900",
);
