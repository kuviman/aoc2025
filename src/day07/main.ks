#!/usr/bin/env kast
include "../common.ks";
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

@syntax "as_Int64" 62 wrap never = value " " "as_Int64";
impl syntax (value as_Int64) = `(
    parse (to_string $value)
);

let mut map = List.create ();

for line in String.lines input do (
    if String.length line != 0 then (
        List.push_back (&mut map, line);
    );
);
let first_line = List.at (&map, 0);
let n = List.length &map;
let m = String.length (List.at (&map, 0))^;

let answer :: Int64 = unwindable block (
    if part1 then (
        let mut answer = 0 as_Int64;
        let mut current_beams = List.create ();
        for i in 0..m do (
            List.push_back (&mut current_beams, String.at (first_line^, i) == 'S');
        );
        for i in 0..n - 1 do (
            let mut next_beams = List.create ();
            for j in 0..m do (
                List.push_back (&mut next_beams, false);
            );
            for j in 0..m do (
                if (List.at (&current_beams, j))^ then (
                    let line_below = List.at (&map, i + 1);
                    let char_below = String.at (line_below^, j);
                    if char_below == '^' then (
                        answer += 1 as_Int64;
                        (List.at_mut (&mut next_beams, j - 1))^ = true;
                        (List.at_mut (&mut next_beams, j + 1))^ = true;
                    ) else (
                        (List.at_mut (&mut next_beams, j))^ = true;
                    );
                );
            );
            current_beams = next_beams;
        );
        unwind block answer;
    ) else (
        let mut next_row_answers = List.create ();
        for _ in 0..m do (
            List.push_back (&mut next_row_answers, 0 as_Int64);
        );
        
        @syntax "for_range_rev" 7.5 wrap never = "for" " " var " " "in" " " "(" start " " ".." " " end ")" "." "rev" "(" ")" " " "do" " " body;
        impl syntax (for i in (start .. end).rev() do body) = `(
            let mut _loop_var = $end;
            while _loop_var > $start do (
                _loop_var -= 1;
                let $i = _loop_var;
                $body;
            )
        );
        
        for i in (0 .. n - 1).rev() do (
            # dbg.print "inside loop";
            let mut current_row_answers = List.create ();
            for j in 0..m do (
                let mut current_cell_answer = 0 as_Int64;
                let line_below = List.at (&map, i + 1);
                let char_below = String.at (line_below^, j);
                if char_below == '^' then (
                    current_cell_answer = (
                        1 as_Int64
                        + (List.at (&next_row_answers, j - 1))^
                        + (List.at (&next_row_answers, j + 1))^
                    );
                ) else (
                    current_cell_answer = (List.at (&next_row_answers, j))^;
                );
                # print ("at " + to_string i + ", " + to_string j + " = " + to_string current_cell_answer);
                List.push_back (&mut current_row_answers, current_cell_answer);
            );
            next_row_answers = current_row_answers;
        );
        
        for j in 0..m do (
            if String.at (first_line^, j) == 'S' then (
                let answer :: Int64 = (List.at (&next_row_answers, j))^ + 1 as_Int64;
                unwind block answer;
            );
        );
    );
    -1 as_Int64
);

dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "21", .part2 = parse "40"),
    .part1 = parse "1524",
    .part2 = parse "32982105837605",
);
