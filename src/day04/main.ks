#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let map = list.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 then (
            map = list.push_back (map, line);
        );
    )
);

let at = (i, j) -> char => String.at (list.at (map, i), j);

let n = list.length map;
let m = String.length (list.at (map, 0));

let answer = 0;
for i in 0..n do (
    dbg.print ("line " + int32_to_string i);
    for j in 0..m do (
        let adj_papers = 0;
        if at (i, j) != '@' then (
            continue;
        );
        for ai in i - 1..i + 2 do (
            for aj in j - 1..j + 2 do (
                if (
                    not (ai == i and aj == j)
                    and (0 <= ai and ai < n)
                    and (0 <= aj and aj < m)
                ) then (
                    if at (ai, aj) == '@' then (
                        adj_papers += 1;
                    );
                );
            );
        );
        # dbg.print (i, j, .adj_papers);
        if adj_papers < 4 then (
            answer += 1;
        );
    );
);
dbg.print answer;
