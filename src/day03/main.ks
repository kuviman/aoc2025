#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let answer = 0;
String.lines (
    input,
    line => (
        let max = 0;
        for i in 0..String.length line do (
            for j in i + 1..String.length line do (
                let first_digit = String.substring (line, i, 1)
                    |> string_to_int32;
                let second_digit = String.substring (line, j, 1)
                    |> string_to_int32;
                let cur = first_digit * 10 + second_digit;
                if cur > max then (
                    max = cur;
                );
            )
        );
        answer += max;
    ),
);
dbg.print answer;
