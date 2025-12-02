#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;
# TODO lang
let answer = "0" |> string_to_int64;
String.split (
    input,
    ',',
    range => (
        let start, end = String.split_once (range, '-');
        let start = start |> String.trim |> string_to_int64;
        let end = end |> String.trim |> string_to_int64;
        dbg.print (start, end, end - start);
        let x = (
            # let s = start |> int64_to_string;
            # let i = (String.length s) / 2;
            # String.substring (s, i, String.length s - i)
            #     |> string_to_int64
            let len = (String.length (int64_to_string start) + 1) / 2;
            let s = "1";
            while len > 1 do (
                s += "0";
                len -= 1;
            );
            s |> string_to_int64
        );
        # dbg.print x;
        # TODO lang
        let one = "1" |> string_to_int64;
        loop (
            let x_s = int64_to_string x;
            let combined = (x_s + x_s) |> string_to_int64;
            if combined > end then break;
            if combined >= start then (
                dbg.print combined;
                answer += combined;
            );
            x += one;
        );
    ),
);
dbg.print answer;
