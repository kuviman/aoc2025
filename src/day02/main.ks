#!/usr/bin/env kast
use (include "../common.ks").*;
@syntax "for_range" 7.5 wrap never = "for" " " pattern " " "in" " " start " " ".." " " end " " "do" " " body;
impl syntax (for pattern in start .. end do body) = `(
    let _loop_var = $start;
    while _loop_var < $end do (
        let $pattern = _loop_var;
        $body;
        _loop_var += 1;
    )
);
const Set = (
    module:
    use std.collections.treap;
    const set = [T] treap.t[T];
    const t = set;
    const create = [T] () -> set[T] => treap.create ();
    const add = [T] (s :: set[T], x :: T) -> set[T] => (
        treap.merge (s, treap.singleton x)
    );
    const contains = [T] (s :: set[T], x :: T) -> bool => (
        unwindable block (
            treap.iter (
                s,
                elem => (
                    if elem == x then unwind block true;
                ),
            );
            false
        )
    );
);
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
        let max_times = if part1 then (
            2
        ) else (
            end |> int64_to_string |> String.length
        );
        let visited = Set.create ();
        for times in 2 .. (max_times + 1) do (
            let x = (
                # let s = start |> int64_to_string;
                # let i = (String.length s) / 2;
                # String.substring (s, i, String.length s - i)
                #     |> string_to_int64
                let len = (String.length (int64_to_string start) + times - 1) / times;
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
                let combined_s = "";
                for _ in 0 .. times do (
                    combined_s += x_s;
                );
                let combined = combined_s |> string_to_int64;
                if combined > end then break;
                if combined >= start and not (Set.contains (visited, combined)) then (
                    visited = Set.add (visited, combined);
                    dbg.print combined;
                    answer += combined;
                );
                x += one;
            );
        );
    ),
);
dbg.print answer;
