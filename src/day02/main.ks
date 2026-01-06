#!/usr/bin/env kast
include "../common.ks";
const Set = (
    module:
    use std.collections.Treap;
    const set = Treap.t;
    const t = set;
    const create = [T] () -> set[T] => Treap.create ();
    const add = [T] (s :: set[T], x :: T) -> set[T] => (
        Treap.join (s, Treap.singleton x)
    );
    const contains = [T] (s :: set[T], x :: T) -> Bool => with_return (
        for &elem in Treap.iter &s do (
            if elem == x then return true;
        );
        false
    );
);
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;
# TODO lang
let mut answer :: Int64 = "0" |> parse;
for range in String.split (input, ',') do (
    let start, end = String.split_once (range, '-');
    let start = start |> String.trim |> parse;
    let end = end |> String.trim |> parse;
    dbg.print (start, end, end - start);
    let max_times = if part1 then (
        2
    ) else (
        end |> to_string |> String.length
    );
    let mut visited = Set.create ();
    for times in 2..(max_times + 1) do (
        let mut x :: Int32 = (
            # let s = start |> to_string;
            # let i = (String.length s) / 2;
            # String.substring (s, i, String.length s - i)
            #     |> parse
            let mut len = (String.length (to_string start) + times - 1) / times;
            let mut s = "1";
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
            let mut combined_s = "";
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
);
dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "1227775554", .part2 = parse "4174379265"),
    .part1 = parse "22062284697",
    .part2 = parse "46666175279",
);
