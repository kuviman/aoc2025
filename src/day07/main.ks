#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let map = list.create ();

String.lines (
    input,
    line => (
        if String.length line != 0 then (
            list.push_back (&map, line);
        );
    ),
);
let first_line = list.at (&map, 0);
let n = list.length &map;
let m = String.length (list.at (&map, 0))^;

if part1 then (
    let answer = 0;
    let current_beams = list.create ();
    for i in 0..m do (
        list.push_back (&current_beams, String.at (first_line^, i) == 'S');
    );
    for i in 0..n - 1 do (
        let next_beams = list.create ();
        for j in 0..m do (
            list.push_back (&next_beams, false);
        );
        for j in 0..m do (
            if (list.at (&current_beams, j))^ then (
                let line_below = list.at (&map, i + 1);
                let char_below = String.at (line_below^, j);
                if char_below == '^' then (
                    answer += 1;
                    (list.at (&next_beams, j - 1))^ = true;
                    (list.at (&next_beams, j + 1))^ = true;
                ) else (
                    (list.at (&next_beams, j))^ = true;
                );
            );
        );
        current_beams = next_beams;
    );
    dbg.print answer;
) else (
    @syntax "as_int64" 62 wrap never = value " " "as_int64";
    impl syntax (value as_int64) = `(
        parse (to_string $value)
    );
    
    let next_row_answers = list.create ();
    for _ in 0..m do (
        list.push_back (&next_row_answers, 0 as_int64);
    );
    
    @syntax "for_range_rev" 7.5 wrap never = "for" " " var " " "in" " " "(" start " " ".." " " end ")" "." "rev" "(" ")" " " "do" " " body;
    impl syntax (for i in (start .. end).rev() do body) = `(
        let _loop_var = $end;
        while _loop_var > $start do (
            _loop_var -= 1;
            let $i = _loop_var;
            $body;
        )
    );
    
    for i in (0 .. n - 1).rev() do (
        # dbg.print "inside loop";
        let current_row_answers = list.create ();
        for j in 0..m do (
            let current_cell_answer = 0 as_int64;
            let line_below = list.at (&map, i + 1);
            let char_below = String.at (line_below^, j);
            if char_below == '^' then (
                current_cell_answer = (
                    1 as_int64
                    + (list.at (&next_row_answers, j - 1))^
                    + (list.at (&next_row_answers, j + 1))^
                );
            ) else (
                current_cell_answer = (list.at (&next_row_answers, j))^;
            );
            # print ("at " + to_string i + ", " + to_string j + " = " + to_string current_cell_answer);
            list.push_back (&current_row_answers, current_cell_answer);
        );
        next_row_answers = current_row_answers;
    );
    
    for j in 0..m do (
        if String.at (first_line^, j) == 'S' then (
            let answer :: int64 = (list.at (&next_row_answers, j))^ + 1 as_int64;
            dbg.print answer;
        );
    );
);
