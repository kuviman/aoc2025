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

const State = Int32;
const Machine = type (
    .lights :: Int32,
    .target_state :: State,
    .buttons :: List.t[State],
    .joltages :: List.t[Int32],
);
let parse_machine = s -> Machine => (
    let lights = 0;
    let target_state = 0;
    let buttons = List.create ();
    let joltages = List.create ();
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
                        let idx :: Int32 = idx |> parse;
                        affected_lights = std.op.bit_or (
                            affected_lights,
                            std.op.bit_shift_left (1, idx),
                        );
                    ),
                );
                List.push_back (&buttons, affected_lights);
            ) else if parens == '{' then (
                String.split (
                    inside,
                    ',',
                    x => List.push_back (&joltages, x |> parse),
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
    use std.collections.Queue;
    let q = Queue.create ();
    let d = List.create ();
    for _ in 0..std.op.bit_shift_left (1, machine.lights) do (
        List.push_back (&d, 0);
    );
    
    print "[INFO] starting bfs";
    (List.at (&d, 0))^ = 1;
    Queue.push (&q, 0);
    (List.at (&d, machine.target_state))^ = -1;
    Queue.push (&q, machine.target_state);
    let machine_answer = -1;
    while Queue.length &q != 0 do (
        let state = Queue.pop &q;
        # if state == machine.target_state then break;
        # dbg.print state;
        let state_d = (List.at (&d, state))^;
        let dir = if state_d < 0 then -1 else +1;
        List.iter (
            &machine.buttons,
            &changes => (
                let new_state = std.op.bit_xor (state, changes);
                let new_state_d = List.at (&d, new_state);
                if new_state_d^ == 0 then (
                    new_state_d^ = state_d + dir;
                    Queue.push (&q, new_state);
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
    const t = type (.rows :: List.t[List.t[Int32]]);
    
    const create = (n, m) -> t => (
        let rows = List.create ();
        for _ in 0..n do (
            let row = List.create ();
            for _ in 0..m do (
                List.push_back (&row, 0);
            );
            List.push_back (&rows, row);
        );
        (.rows)
    );
    
    const at_mut = (a :: &t, i :: Int32, j :: Int32) -> &Int32 => (
        List.at (List.at (&a^.rows, i), j)
    );
    const at = (a :: &t, i, j) => (at_mut (a, i, j))^;
    
    const row = (a :: &t, i) => (
        List.at (&a^.rows, i)
    );
    
    const print = (a :: &t) => (
        List.iter (
            &a^.rows,
            row => (
                let s = "";
                List.iter (
                    row,
                    &x => (
                        s += " " + to_string x;
                    ),
                );
                std.io.print s;
            )
        );
    );
    
    const size = (a :: &t) -> (Int32, Int32) => (
        List.length &a^.rows,
        List.length (List.at (&a^.rows, 0))
    );
);

let swap = [T] (a :: &T, b :: &T) => (
    a^, b^ = b^, a^;
);

# const gcd = (a :: Int32, b :: Int32) -> (Int32, .ma :: Int32, .mb :: Int32) => (
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
    let free = List.create ();
    let pivot = List.create ();
    
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
                List.push_back (&free, j);
            );
            j += 1;
            continue;
        );
        if j + 1 < m then (
            List.push_back (&pivot, (.row = i, .var = j));
        );
        let row = Matrix.row (a, i);
        swap (row, Matrix.row (a, non_zero_row));
        let at_row = (List.at (row, j))^;
        for lower in i + 1..n do (
            let lower = Matrix.row (a, lower);
            if (List.at (lower, j))^ % at_row != 0 then (
                # we can't safely divide, so we scale
                # the whole row up to compensate
                List.iter (lower, x => (x^ *= at_row));
            );
            let mult = (List.at (lower, j))^ / at_row;
            for k in j..m do (
                let at_row = (List.at (row, k))^;
                let at_lower = (List.at (lower, k));
                at_lower^ -= at_row * mult;
            );
        );
        i += 1;
        j += 1;
    );
    for j in j..m - 1 do (
        List.push_back (&free, j);
    );
    if verbose then (
        print "[INFO] after gauss";
        Matrix.print a;
        print "====";
    );
    
    print ("free vars = " + std.collections.Treap.to_string (&free.inner, &x => to_string x));
    
    (.free, .pivot)
);

let pow = (x, n) => (
    let result = 1;
    for _ in 0..n do (
        result *= x;
    );
    result
);

const Treap = (
    module:
    const iter_rev = [T] (a :: std.collections.Treap.t[T], f) => (
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
    let vars = List.create ();
    let free = List.create ();
    let pivot = List.create ();
    let current_answer = 0;
    let check = () => with_return (
        # print "TRY:";
        # List.iter (
        #     &free,
        #     &var => (
        #         let value = (List.at (&vars, var))^;
        #         # print (to_string var + " = " + to_string value);
        #         current_answer += value;
        #     ),
        # );
        # 300 * 300 * 300 * 10 * 10 * 100000
        # print "pivot:";
        Treap.iter_rev (
            pivot.inner,
            &(.var, .row) => (
                let row = Matrix.row (&a, row);
                let result = (List.at (row, m - 1))^;
                for j in var + 1..m - 1 do (
                    result -= (List.at (&vars, j))^ * (List.at (row, j))^;
                );
                let k = (List.at (row, var))^;
                if result % k != 0 then return;
                result /= k;
                if result < 0 then return;
                current_answer += result;
                if current_answer >= machine_answer then return;
                # print (to_string var + " = " + to_string result);
                (List.at (&vars, var))^ = result;
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
                (List.at (&vars, (List.at (&free, i))^))^ = x;
                current_answer += x;
                force (i - 1, .max = max - x);
                current_answer -= x;
            );
        )
    );
);

let max_combinations = 0;
let solve_part2 = (machine :: Machine) -> Int32 => with_return (
    # return 0;
    let n = List.length &machine.joltages;
    let m = List.length &machine.buttons + 1;
    let a = Matrix.create (n, m);
    (
        let i = 0;
        List.iter (
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
        List.iter (
            &machine.joltages,
            &x => (
                (Matrix.at_mut (&a, i, m - 1))^ = x;
                i += 1;
            ),
        );
    );
    
    let (.free, .pivot) = gauss_elimination (&a);
    
    let max_joltage = 0;
    List.iter (&machine.joltages, &x => (max_joltage = max (max_joltage, x)));
    print ("max_joltage = " + to_string max_joltage);
    let combinations = pow (max_joltage, List.length &free);
    max_combinations = max (max_combinations, combinations);
    print ("combinations(max^free) = " + to_string combinations);
    
    let vars = List.create ();
    for _ in 0..m - 1 do (
        List.push_back (&vars, 0);
    );
    
    brute.machine_answer = 1000000000;
    brute.a = a;
    brute.n = n;
    brute.m = m;
    brute.vars = vars;
    brute.free = free;
    brute.pivot = pivot;
    brute.force (List.length &free - 1, .max = max_joltage);
    
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
        
        max_buttons = max (max_buttons, List.length &machine.buttons);
        max_joltages = max (max_joltages, List.length &machine.joltages);
        List.iter (
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
