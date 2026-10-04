#!/usr/bin/env kast
include "../common.ks";
use std.StringBuilder;
const Set = (
    module:
    use std.collections.Treap;
    const set = Treap.t;
    const t = set;
    const new = [T] () -> set[T] => Treap.new();
    const add = [T] (s :: set[T], x :: T) -> set[T] => (
        Treap.join(s, Treap.singleton(x))
    );
    const contains = [T] (s :: &set[T], x :: T) -> Bool => with_return (
        for &elem in Treap.iter(s) do (
            if elem == x then return true;
        );
        false
    );
);

std.sys.chdir(std.path.dirname(__FILE__));
let input = std.fs.read_file(input_path);
# TODO lang
let mut answer :: Int64 = "0" |> parse;
for range in String.split(&input |> as_str, ',') do (
    let { start, end } = String.split_once(range, '-');
    let start = start |> String.trim |> parse;
    let end = end |> String.trim |> parse;
    dbg.print({ start, end, end - start });
    let max_times = if part1 then (
        2
    ) else (
        let end = end |> to_string;
        &end |> as_str |> String.length
    );
    let mut visited = Set.new();
    for times in 2..(max_times + 1) do (
        let mut x :: Int32 = (
            # let s = start |> to_string;
            # let i = (String.length s) / 2;
            # String.substring (s, i, String.length s - i)
            #     |> parse
            let mut len = (String.length(&to_string(start) |> as_str) + times - 1) / times;
            let mut s = String.from_str("1");
            while len > 1 do (
                s = String.concat_owned(s, String.from_str("0"));
                len -= 1;
            );
            &s |> as_str |> parse
        );
        
        # dbg.print x;
        # TODO lang
        loop (
            let x_s = to_string(x);
            let mut combined_s = StringBuilder.new();
            for _ in 0..times do (
                &mut combined_s |> StringBuilder.add_str(&x_s |> as_str);
            );
            let combined_s = combined_s |> StringBuilder.into_string;
            let combined = &combined_s |> as_str |> parse;
            if combined > end then break;
            if combined >= start and not (Set.contains(&visited, combined)) then (
                visited = Set.add(visited, combined);
                dbg.print(combined);
                answer += combined;
            );
            x += 1;
        );
    );
);

dbg.print(answer);

assert_answers(
    answer,
    .example = { .part1 = parse("1227775554"), .part2 = parse("4174379265") },
    .part1 = parse("22062284697"),
    .part2 = parse("46666175279"),
);
