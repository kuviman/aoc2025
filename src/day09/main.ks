#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let as_int64 :: int32 -> int64 = x => (x |> to_string |> parse);

const Coords = type (
    .x :: int64,
    .y :: int64,
);

let tiles :: list.t[Coords] = list.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 then (
            let x, y = String.split_once (line, ',');
            let x = x |> parse;
            let y = y |> parse;
            let coords :: Coords = (.x, .y);
            list.push_back (&tiles, coords);
        );
    ),
);

# TODO make lang easier to use int64 literals
let zero = as_int64 0;
let one = as_int64 1;

let abs = x => (
    if x < zero then (
        # TODO unary op
        zero - x
    ) else (
        x
    )
);

let n = list.length &tiles;
let answer = zero;
for i in 0..n do (
    for j in 0..i do (
        let a = list.at (&tiles, i);
        let b = list.at (&tiles, j);
        let area = (abs (b^.x - a^.x) + one) * (abs (b^.y - a^.y) + one);
        if area > answer then (
            answer = area;
        );
    );
);

dbg.print answer;
