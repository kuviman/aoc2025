#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let ranges = list.create ();
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
                list.push_back (&ranges, (start, end));
            ) else (
                let id = string_to_int64 line;
                list.push_back (&ids, id);
            );
        );
    ),
);

let answer = 0;
list.iter (
    &ids,
    &id => (
        let found = unwindable block (
            list.iter (
                &ranges,
                &(start, end) => (
                    if start <= id and id <= end then (
                        unwind block true;
                    );
                )
            );
            false
        );
        if found then (
            answer += 1;
        );
    ),
);
dbg.print answer;
# dbg.print (.ranges, .ids);
