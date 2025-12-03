#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

@syntax "as" 62 wrap never = value " " "as" " " type;
impl syntax (value as ty) = `(
    string_to_int64 (int32_to_string $value) :: $ty
);

@syntax "with_return" 60 wrap never = "with_return" " " body;
impl syntax (with_return body) = `(
    unwindable _returnable (
        $body
    )
);
@syntax "return_without_value" 60 wrap never = "return";
@syntax "return_with_value" 60 wrap never = "return" " " value;
impl syntax (return) = `(
    unwind _returnable ()
);
impl syntax (return value) = `(
    unwind _returnable $value
);

let k = if part1 then 2 else 12;

const list = std.collections.treap;

# let f = () => (
# list |> map (x => if x < 0 then return :Error)
# );
let answer = 0 as int64;
String.lines (
    input,
    line => with_return (
        let n = String.length line;
        if n == 0 then return;
        
        let start = 0;
        let line_answer = 0 as int64;
        
        let data = (
            let current_start = 0;
            let current_end = 0;
            let positions_by_digit = std.collections.list.create[list.t[int32]] ();
            # for _ in 0..10 do (
            #     positions_by_digit = list.merge (
            #         positions_by_digit,
            #         list.singleton (list.create ()),
            #     );
            # );
            # let non_empty_digits :: list.t[int32] = list.create ();
            let move_start_to = i => (
                current_start = i;
            );
            let move_end_to = i => (
                current_end = i;
            );
            let max_in_range = () -> (.index :: int32, .value :: int32) => (
                let max_i, max_value = 0 - 1, 0 - 1;
                for i in current_start..current_end do (
                    let value = String.get_at (line, i) |> Char.to_digit;
                    if value > max_value then (
                        max_value = value;
                        max_i = i;
                    );
                );
                (
                    .index = max_i,
                    .value = max_value,
                )
            );
            (
                .move_start_to,
                .move_end_to,
                .max_in_range,
            )
        );
        
        for i in 0..k do (
            data.move_end_to (n - k + i + 1);
            let (
                .index = new_start,
                .value = next_digit,
            ) = data.max_in_range ();
            data.move_start_to (new_start + 1);
            line_answer = line_answer * 10 as int64 + next_digit as int64;
        );
        
        dbg.print (line, line_answer);
        answer += line_answer;
    ),
);
dbg.print answer;
