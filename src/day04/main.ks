#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

use std.collections.Queue;

let map = List.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 then (
            let chars = List.create ();
            String.iter (
                line,
                c => (
                    List.push_back (&chars, c);
                )
            );
            List.push_back (&map, chars);
        );
    )
);

let at = (i, j) -> &Char => (
    List.at (List.at (&map, i), j)
);

let n = List.length &map;
let m = List.length (List.at (&map, 0));

let in_bounds = (i, j) => (
    (0 <= i and i < n)
    and (0 <= j and j < m)
);

let q = Queue.create ();
for i in 0..n do (
    for j in 0..m do (
        Queue.push (&q, (i, j));
    );
);

let answer = 0;
while Queue.length &q != 0 do (
    let i, j = Queue.pop &q;
    if (at (i, j))^ != '@' then (
        continue;
    );
    let adj_papers = 0;
    for ai in i - 1..i + 2 do (
        for aj in j - 1..j + 2 do (
            if (
                not (ai == i and aj == j)
                and in_bounds (ai, aj)
            ) then (
                if (at (ai, aj))^ == '@' then (
                    adj_papers += 1;
                );
            );
        );
    );
    if adj_papers < 4 then (
        if part2 then (
            print ("removed at " + to_string i + ", " + to_string j);
            (at (i, j))^ = '.';
            for ai in i - 1..i + 2 do (
                for aj in j - 1..j + 2 do (
                    if in_bounds (ai, aj) then (
                        Queue.push (&q, (ai, aj));
                    );
                );
            );
        );
        answer += 1;
    );
);
dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = 13, .part2 = 43),
    .part1 = 1457,
    .part2 = 8310,
);
