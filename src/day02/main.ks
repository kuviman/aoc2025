#!/usr/bin/env kast
use (include "../common.ks").*;
const Set = (
    module:
    use std.collections.treap;
    const set = [T] treap.t[T];
    const t = set;
    const create = [T] () -> set[T] => treap.create ();
    const add = [T] (s :: set[T], x :: T) -> set[T] => (
        treap.join (s, treap.singleton x)
    );
    const contains = [T] (s :: set[T], x :: T) -> bool => (
        unwindable block (
            treap.iter (
                &s,
                &elem => (
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
let answer = "0" |> parse;
String.split (
    input,
    ',',
    range => (
        let start, end = String.split_once (range, '-');
        let start = start |> String.trim |> parse;
        let end = end |> String.trim |> parse;
        dbg.print (start, end, end - start);
        let max_times = if part1 then (
            2
        ) else (
            end |> to_string |> String.length
        );
        let visited = Set.create ();
        for times in 2..(max_times + 1) do (
            let x = (
                # let s = start |> to_string;
                # let i = (String.length s) / 2;
                # String.substring (s, i, String.length s - i)
                #     |> parse
                let len = (String.length (to_string start) + times - 1) / times;
                let s = "1";
                while len > 1 do (
                    s += "0";
                    len -= 1;
                );
                s |> parse
            );
            # dbg.print x;
            # TODO lang
            let one = "1" |> parse;
            loop (
                let x_s = to_string x;
                let combined_s = "";
                for _ in 0..times do (
                    combined_s += x_s;
                );
                let combined = combined_s |> parse;
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
