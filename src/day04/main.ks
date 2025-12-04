#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

use std.collections.queue;

let map = list.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 then (
            let chars = list.create ();
            String.iter (
                line,
                c => (
                    chars = list.push_back (chars, c);
                )
            );
            map = list.push_back (map, chars);
        );
    )
);

let at = (i, j) -> char => (
    list.at (list.at (map, i), j)
);
let set_at = (i, j, c) => (
    map = list.set_at (map, i, (list.set_at (list.at (map, i), j, c)));
);

let n = list.length map;
let m = list.length (list.at (map, 0));

let in_bounds = (i, j) => (
    (0 <= i and i < n)
    and (0 <= j and j < m)
);

let q = queue.create ();
for i in 0..n do (
    for j in 0..m do (
        q = queue.push (q, (i, j));
    );
);

let answer = 0;
while queue.length q != 0 do (
    let (i, j), new_q = queue.pop q;
    q = new_q;
    if at (i, j) != '@' then (
        continue;
    );
    let adj_papers = 0;
    for ai in i - 1..i + 2 do (
        for aj in j - 1..j + 2 do (
            if (
                not (ai == i and aj == j)
                and in_bounds (ai, aj)
            ) then (
                if at (ai, aj) == '@' then (
                    adj_papers += 1;
                );
            );
        );
    );
    if adj_papers < 4 then (
        if part2 then (
            print ("removed at " + int32_to_string i + ", " + int32_to_string j);
            set_at (i, j, '.');
            for ai in i - 1..i + 2 do (
                for aj in j - 1..j + 2 do (
                    if in_bounds (ai, aj) then (
                        q = queue.push (q, (ai, aj));
                    );
                );
            );
        );
        answer += 1;
    );
);
dbg.print answer;
