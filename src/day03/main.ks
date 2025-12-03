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

const list = (
    module:
    use std.collections.treap.*;
    
    const update = [T] (a :: treap[T], idx :: int32, f :: T -> T) -> treap[T] => (
        set_at (a, idx, f (get_at (a, idx)))
    );
);
const queue = (
    module:
    use list.*;
    const queue = t;
    
    const push = [T] (q :: queue[T], value :: T) -> queue[T] => (
        merge (q, singleton value)
    );
    const pop = [T] (q :: queue[T]) -> (T, queue[T]) => (
        let first, rest = split_at (q, 1);
        get_at (first, 0), rest
    );
    const front = [T] (q :: queue[T]) -> T => (
        get_at (q, 0)
    );
);

# let f = () => (
# list |> map (x => if x < 0 then return :Error)
# );
let answer = 0 as int64;
String.lines (
    input,
    line => with_return (
        let n = String.length line;
        if n == 0 then return;
        
        dbg.print line;
        
        let start = 0;
        let line_answer = 0 as int64;
        
        let data = (
            let current_start = 0;
            let current_end = 0;
            let positions_by_digit = list.create[queue.t[int32]] ();
            for _ in 0..10 do (
                positions_by_digit = list.merge (
                    positions_by_digit,
                    list.singleton (queue.create ()),
                );
            );
            let move_start_to = i => (
                while current_start < i do (
                    let pos = current_start;
                    let digit = String.get_at (line, pos)
                        |> Char.to_digit;
                    positions_by_digit = list.update (
                        positions_by_digit,
                        digit,
                        positions => (
                            let first, rest = queue.pop positions;
                            if first != pos then (
                                panic "bug";
                            );
                            rest
                        ),
                    );
                    # dbg.print ("pop", digit, pos);
                    # queue.iter (list.get_at (positions_by_digit, digit), dbg.print[_]);
                    current_start += 1;
                );
            );
            let move_end_to = i => (
                while current_end < i do (
                    let pos = current_end;
                    let digit = String.get_at (line, pos)
                        |> Char.to_digit;
                    positions_by_digit = list.update (
                        positions_by_digit,
                        digit,
                        positions => (
                            queue.push (
                                positions,
                                pos,
                            )
                        ),
                    );
                    # dbg.print ("push", digit, pos);
                    # queue.iter (list.get_at (positions_by_digit, digit), dbg.print[_]);
                    current_end += 1;
                );
            );
            let max_in_range = () -> (.index :: int32, .value :: int32) => with_return (
                let digit = 9;
                while digit >= 0 do (
                    let positions = list.get_at (positions_by_digit, digit);
                    if queue.count positions != 0 then (
                        return (
                            .index = queue.front positions,
                            .value = digit,
                        );
                    );
                    digit -= 1;
                );
                panic "nothing found"
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
        
        dbg.print line_answer;
        answer += line_answer;
    ),
);
dbg.print answer;
