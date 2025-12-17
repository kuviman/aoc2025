#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

@syntax "as_Int64" 62 wrap never = value " " "as_Int64";
impl syntax (value as_Int64) = `(
    parse (to_string $value)
);

use std.collections.Treap;

const segment_set = (
    module:
    const segment = type (Int64, Int64);
    const t = Treap.t[segment];
    
    const create = () -> t => (
        Treap.create ()
    );
    
    const split = (set :: t, x :: Int64) -> (t, t) => (
        Treap.split (
            set,
            node => (
                let start, end = node^.value;
                if x <= start then (
                    :RightSubtree
                ) else if x >= end then (
                    :LeftSubtree
                ) else (
                    :Node ((start, x), (x, end))
                )
            )
        )
    );
    
    const add = (set :: t, (start, end) :: segment) -> t => (
        let left, right = split (set, start);
        let middle, right = split (right, end);
        Treap.join (left, Treap.join (Treap.singleton (start, end), right))
    );
    
    const contains = (set :: &t, point :: Int64) -> Bool => (
        match set^ with (
            | :Empty => false
            | :Node node => (
                let start, end = node.value;
                if point < start then (
                    contains (&node.left, point)
                ) else if point >= end then (
                    contains (&node.right, point)
                ) else (
                    true
                )
            )
        )
    );
    
    const total_length = (set :: &t) -> Int64 => (
        match set^ with (
            | :Empty => 0 as_Int64
            | :Node node => (
                let start, end = node.value;
                (end - start)
                + total_length &node.left
                + total_length &node.right
            )
        )
    );
);

let mut ranges = segment_set.create ();
let mut ids = List.create ();

let mut parsing_ranges = true;
String.lines (
    input,
    line => (
        if String.length line == 0 then (
            parsing_ranges = false;
        ) else (
            if parsing_ranges then (
                let start, end = String.split_once (line, '-');
                let start = start |> parse;
                let end = end |> parse;
                ranges = segment_set.add (ranges, (start, end + 1 as_Int64));
            ) else (
                let id = parse line;
                List.push_back (&mut ids, id);
            );
        );
    ),
);

let answer = if part1 then (
    let mut answer = 0 as_Int64;
    List.iter (
        &ids,
        &id => (
            if segment_set.contains (&ranges, id) then (
                answer += 1 as_Int64;
            );
        ),
    );
    answer
) else (
    segment_set.total_length (&ranges)
);

dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "3", .part2 = parse "14"),
    .part1 = parse "664",
    .part2 = parse "350780324308385",
);
