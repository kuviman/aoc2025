#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

@syntax "for_range_rev" 7.5 wrap never = "for" " " var " " "in" " " "(" start " " ".." " " end ")" "." "rev" "(" ")" " " "do" " " body;
impl syntax (for i in (start .. end).rev() do body) = `(
    let _loop_var = $end;
    while _loop_var > $start do (
        _loop_var -= 1;
        let $i = _loop_var;
        $body;
    )
);

let verbose = false;

const State = int32;
const Machine = type (
    .lights :: int32,
    .target_state :: State,
    .buttons :: list.t[State],
    .joltages :: list.t[int32],
);
let parse_machine = s -> Machine => (
    let lights = 0;
    let target_state = 0;
    let buttons = list.create ();
    let joltages = list.create ();
    String.split (
        s,
        ' ',
        part => (
            let parens = String.at (part, 0);
            let inside = String.substring (part, 1, String.length part - 2);
            if parens == '[' then (
                lights = String.length inside;
                
                for i in (0 .. String.length inside).rev() do (
                    let c = String.at (inside, i);
                    let c = if c == '#' then 1 else 0;
                    target_state = std.op.bit_shift_left (target_state, 1) + c;
                );
                # print ("[INFO] read target_state=" + to_string target_state);
            ) else if parens == '(' then (
                let affected_lights = 0;
                String.split (
                    inside,
                    ',',
                    idx => (
                        let idx :: int32 = idx |> parse;
                        affected_lights = std.op.bit_or (
                            affected_lights,
                            std.op.bit_shift_left (1, idx),
                        );
                    ),
                );
                list.push_back (&buttons, affected_lights);
            ) else if parens == '{' then (
                String.split (
                    inside,
                    ',',
                    x => list.push_back (&joltages, x |> parse),
                );
            ) else (
                panic "unexpected char"
            );
        ),
    );
    (.lights, .target_state, .buttons, .joltages)
);

let abs = x => if x < 0 then -x else x;
let max = (a, b) => if a > b then a else b;
let min = (a, b) => if a < b then a else b;

let solve_part1 = (machine :: Machine) => (
    use std.collections.queue;
    let q = queue.create ();
    let d = list.create ();
    for _ in 0..std.op.bit_shift_left (1, machine.lights) do (
        list.push_back (&d, 0);
    );
    
    print "[INFO] starting bfs";
    (list.at (&d, 0))^ = 1;
    queue.push (&q, 0);
    (list.at (&d, machine.target_state))^ = -1;
    queue.push (&q, machine.target_state);
    let machine_answer = -1;
    while queue.length &q != 0 do (
        let state = queue.pop &q;
        # if state == machine.target_state then break;
        # dbg.print state;
        let state_d = (list.at (&d, state))^;
        let dir = if state_d < 0 then -1 else +1;
        list.iter (
            &machine.buttons,
            &changes => (
                let new_state = std.op.bit_xor (state, changes);
                let new_state_d = list.at (&d, new_state);
                if new_state_d^ == 0 then (
                    new_state_d^ = state_d + dir;
                    queue.push (&q, new_state);
                ) else if (new_state_d^ < 0) != (state_d < 0) then (
                    machine_answer = abs (state_d) + abs (new_state_d^) - 1;
                    break;
                );
            )
        );
    );
    if machine_answer < 0 then (
        panic "couldnt solve the machine";
    );
    machine_answer
);

const Matrix = (
    module:
    const t = type (.rows :: list.t[list.t[int32]]);
    
    const create = (n, m) -> t => (
        let rows = list.create ();
        for _ in 0..n do (
            let row = list.create ();
            for _ in 0..m do (
                list.push_back (&row, 0);
            );
            list.push_back (&rows, row);
        );
        (.rows)
    );
    
    const at_mut = (a :: &t, i :: int32, j :: int32) -> &int32 => (
        list.at (list.at (&a^.rows, i), j)
    );
    const at = (a :: &t, i, j) => (at_mut (a, i, j))^;
    
    const row = (a :: &t, i) => (
        list.at (&a^.rows, i)
    );
    
    const print = (a :: &t) => (
        list.iter (
            &a^.rows,
            row => (
                let s = "";
                list.iter (
                    row,
                    &x => (
                        s += " " + to_string x;
                    ),
                );
                std.io.print s;
            )
        );
    );
    
    const size = (a :: &t) -> (int32, int32) => (
        list.length &a^.rows,
        list.length (list.at (&a^.rows, 0))
    );
);

let swap = [T] (a :: &T, b :: &T) => (
    a^, b^ = b^, a^;
);

# const gcd = (a :: int32, b :: int32) -> (int32, .ma :: int32, .mb :: int32) => (
#     if a == 0 then (
#         (b, .ma = 0, .mb = 1)
#     ) else if b == 0 then (
#         (a, .ma = 1, .mb = 2)
#     ) else (
#     )
# );
# const Mod = (
#     module:
#     # We will be working with ints modulo P
#     #  P is prime. this lets us define int division
#     # a / b = c
#     # (c * b mod P) = a
#     const P = 313;
#     const sub = (a, b) => (
#         let c = a - b;
#         if c >= 0 then c else c + P
#     );
#     const add = (a, b) => (
#         let c = a + b;
#         if c < P then c else c - P
#     );
#     const mul = (a, b) => (
#         (a * b) % P
#     );
#     const inv = (x) => (
#         # x * y = k * P + 1
#         # x * y + k * P = 1
#         # gcd(x, P) = 1
#     );
#     const div = (a, b) => (
#         mul (a, inv (b))
#     );
# );
let gauss_elimination = (a :: &Matrix.t) => (
    if verbose then (
        print "[INFO] before gauss";
        Matrix.print a;
    );
    let n, m = Matrix.size a;
    let i, j = 0, 0;
    let free = list.create ();
    let pivot = list.create ();
    
    loop (
        if i >= n or j >= m then break;
        let non_zero_row = -1;
        for row in i..n do (
            if Matrix.at (a, row, j) != 0 then (
                non_zero_row = row;
            );
        );
        if non_zero_row == -1 then (
            if j + 1 < m then (
                list.push_back (&free, j);
            );
            j += 1;
            continue;
        );
        if j + 1 < m then (
            list.push_back (&pivot, (.row = i, .var = j));
        );
        let row = Matrix.row (a, i);
        swap (row, Matrix.row (a, non_zero_row));
        let at_row = (list.at (row, j))^;
        for lower in i + 1..n do (
            let lower = Matrix.row (a, lower);
            if (list.at (lower, j))^ % at_row != 0 then (
                # we can't safely divide, so we scale
                # the whole row up to compensate
                list.iter (lower, x => (x^ *= at_row));
            );
            let mult = (list.at (lower, j))^ / at_row;
            for k in j..m do (
                let at_row = (list.at (row, k))^;
                let at_lower = (list.at (lower, k));
                at_lower^ -= at_row * mult;
            );
        );
        i += 1;
        j += 1;
    );
    for j in j..m - 1 do (
        list.push_back (&free, j);
    );
    if verbose then (
        print "[INFO] after gauss";
        Matrix.print a;
        print "====";
    );
    
    print ("free vars = " + std.collections.treap.to_string (&free.inner, &x => to_string x));
    
    (.free, .pivot)
);

let pow = (x, n) => (
    let result = 1;
    for _ in 0..n do (
        result *= x;
    );
    result
);

const treap = (
    module:
    const iter_rev = [T] (a :: std.collections.treap.t[T], f) => (
        match a with (
            | :Empty => ()
            | :Node node => (
                iter_rev (node.right, f);
                f &node.value;
                iter_rev (node.left, f);
            )
        )
    )
);

let brute = (
    module:
    let machine_answer = 0;
    let n = 0;
    let m = 0;
    let a = Matrix.create (0, 0);
    let vars = list.create ();
    let free = list.create ();
    let pivot = list.create ();
    let current_answer = 0;
    let check = () => with_return (
        # print "TRY:";
        # list.iter (
        #     &free,
        #     &var => (
        #         let value = (list.at (&vars, var))^;
        #         # print (to_string var + " = " + to_string value);
        #         current_answer += value;
        #     ),
        # );
        # 300 * 300 * 300 * 10 * 10 * 100000
        # print "pivot:";
        treap.iter_rev (
            pivot.inner,
            &(.var, .row) => (
                let row = Matrix.row (&a, row);
                let result = (list.at (row, m - 1))^;
                for j in var + 1..m - 1 do (
                    result -= (list.at (&vars, j))^ * (list.at (row, j))^;
                );
                let k = (list.at (row, var))^;
                if result % k != 0 then return;
                result /= k;
                if result < 0 then return;
                current_answer += result;
                if current_answer >= machine_answer then return;
                # print (to_string var + " = " + to_string result);
                (list.at (&vars, var))^ = result;
            ),
        );
        # print ("current = " + to_string current_answer);
        machine_answer = min (machine_answer, current_answer);
    );
    let force = (i, .max) => (
        if i < 0 then (
            let save = current_answer;
            check ();
            current_answer = save;
        ) else (
            for x in 0..max + 1 do (
                # print ("BRUTE " + to_string i + " = " + to_string x);
                (list.at (&vars, (list.at (&free, i))^))^ = x;
                current_answer += x;
                force (i - 1, .max = max - x);
                current_answer -= x;
            );
        )
    );
);

let max_combinations = 0;
let solve_part2 = (machine :: Machine) -> int32 => with_return (
    # return 0;
    let n = list.length &machine.joltages;
    let m = list.length &machine.buttons + 1;
    let a = Matrix.create (n, m);
    (
        let i = 0;
        list.iter (
            &machine.buttons,
            &button => (
                for j in 0..n do (
                    if std.op.bit_and (button, 1) != 0 then (
                        (Matrix.at_mut (&a, j, i))^ = 1;
                    );
                    button = std.op.bit_shift_right (button, 1);
                );
                i += 1;
            ),
        );
        let i = 0;
        list.iter (
            &machine.joltages,
            &x => (
                (Matrix.at_mut (&a, i, m - 1))^ = x;
                i += 1;
            ),
        );
    );
    
    let (.free, .pivot) = gauss_elimination (&a);
    
    let max_joltage = 0;
    list.iter (&machine.joltages, &x => (max_joltage = max (max_joltage, x)));
    print ("max_joltage = " + to_string max_joltage);
    let combinations = pow (max_joltage, list.length &free);
    max_combinations = max (max_combinations, combinations);
    print ("combinations(max^free) = " + to_string combinations);
    
    let vars = list.create ();
    for _ in 0..m - 1 do (
        list.push_back (&vars, 0);
    );
    
    brute.machine_answer = 1000000000;
    brute.a = a;
    brute.n = n;
    brute.m = m;
    brute.vars = vars;
    brute.free = free;
    brute.pivot = pivot;
    brute.force (list.length &free - 1, .max = max_joltage);
    
    print ("answer = " + to_string brute.machine_answer);
    brute.machine_answer
);

let max_buttons = 0;
let max_joltages = 0;
let max_joltage = 0;
let idx = 0;
let answer = 0;
String.lines (
    input,
    line => with_return (
        if String.length line == 0 then return;
        let machine = parse_machine line;
        
        max_buttons = max (max_buttons, list.length &machine.buttons);
        max_joltages = max (max_joltages, list.length &machine.joltages);
        list.iter (
            &machine.joltages,
            &x => (
                max_joltage = max (max_joltage, x);
            ),
        );
        
        idx += 1;
        print ("[INFO] read machine #" + to_string idx);
        
        if part1 then (
            answer += solve_part1 machine;
        ) else (
            answer += solve_part2 machine;
        );
    ),
);
dbg.print (.max_buttons, .max_joltages, .max_joltage, .max_combinations);
dbg.print answer;
assert_answers (
    answer,
    .example = (.part1 = 7, .part2 = 33),
    .part1 = 527,
    .part2 = 19810,
);
