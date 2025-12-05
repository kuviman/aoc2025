#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

@syntax "as" 62 wrap never = value " " "as" " " type;
impl syntax (value as ty) = `(
    string_to_int64 (int32_to_string $value) :: $ty
);

use std.collections.treap;

const segment_set = (
    module:
    const segment = type (int64, int64);
    const t = treap.t[segment];
    
    const create = () -> t => (
        treap.create ()
    );
    
    const split = (set :: t, x :: int64) -> (t, t) => (
        treap.split(set, node => (
            let start, end = node^.value;
            if x <= start then (
                :LeftSubtree
            ) else if x >= end then (
                :RightSubtree
            ) else (
                :Node ((start, x), (x, end))
            )
        ))
    );
    
    const add = (set :: t, (start, end) :: segment) -> t => (
        let left, right = split (set, start);
        let middle, right = split (right, end);
        treap.merge (left, treap.merge (treap.singleton (start, end), right))
    );
    
    const contains = (set :: &t, point :: int64) -> bool => (
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
    
    const total_length = (set :: &t) -> int64 => (
        match set^ with (
            | :Empty => 0 as int64
            | :Node node => (
                let start, end = node.value;
                (end - start)
                + total_length &node.left
                + total_length &node.right
            )
        )
    );
);

let ranges = segment_set.create ();
let ids = list.create ();

let parsing_ranges = true;
String.lines (
    input,
    line => (
        if String.length line == 0 then (
            parsing_ranges = false;
        ) else (
            if parsing_ranges then (
                let start, end = String.split_once (line, '-');
                let start = start |> string_to_int64;
                let end = end |> string_to_int64;
                ranges = segment_set.add (ranges, (start, end + 1 as int64));
            ) else (
                let id = string_to_int64 line;
                list.push_back (&ids, id);
            );
        );
    ),
);

if part1 then (
    let answer = 0;
    list.iter (
        &ids,
        &id => (
            if segment_set.contains (&ranges, id) then (
                answer += 1;
            );
        ),
    );
    dbg.print answer;
) else (
    let answer = segment_set.total_length (&ranges);
    dbg.print answer;
);
