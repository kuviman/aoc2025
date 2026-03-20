#!/usr/bin/env kast
include "../common.ks";
std.sys.chdir(std.path.dirname(__FILE__));
let input = std.fs.read_file(input_path);

let verbose = false;
const State = Int32;
const Machine = newtype {
    .lights :: Int32,
    .target_state :: State,
    .buttons :: ArrayList.t[State],
    .joltages :: ArrayList.t[Int32],
};
let parse_machine = s -> Machine => (
    let mut lights = 0;
    let mut target_state = 0;
    let mut buttons = ArrayList.new();
    let mut joltages = ArrayList.new();
    for part in String.split(s, ' ') do (
        let parens = String.at(part, 0);
        let inside = String.substring(part, 1, String.length(part) - 2);
        if parens == '[' then 
        # print ("[INFO] read target_state=" + to_string target_state);
        (
            lights = String.length(inside);
            
            for i in (0..String.length(inside)).rev() do (
                let c = String.at(inside, i);
                let c = if c == '#' then 1 else 0;
                target_state = std.op.bit_shift_left(target_state, 1) + c;
            );
        ) else if parens == '(' then (
            let mut affected_lights = 0;
            for idx in String.split(inside, ',') do (
                let idx :: Int32 = idx |> parse;
                affected_lights = std.op.bit_or(
                    affected_lights,
                    std.op.bit_shift_left(1, idx),
                );
            );
            
            ArrayList.push_back(&mut buttons, affected_lights);
        ) else if parens == '{' then (
            for x in String.split(inside, ',') do (
                ArrayList.push_back(&mut joltages, x |> parse);
            );
        ) else (
            panic("unexpected char")
        );
    );
    {
        .lights,
        .target_state,
        .buttons,
        .joltages
    }
);
let abs = x => if x < 0 then -x else x;
let max = (a, b) => if a > b then a else b;
let min = (a, b) => if a < b then a else b;
let solve_part1 = (machine :: Machine) => (
    use std.collections.Queue;
    let mut q = Queue.new();
    let mut d = ArrayList.new();
    for _ in 0..std.op.bit_shift_left(1, machine.lights) do (
        ArrayList.push_back(&mut d, 0);
    );
    
    print("[INFO] starting bfs");
    (ArrayList.at_mut(&mut d, 0))^ = 1;
    Queue.push(&mut q, 0);
    (ArrayList.at_mut(&mut d, machine.target_state))^ = -1;
    Queue.push(&mut q, machine.target_state);
    let mut machine_answer = -1;
    unwindable bfs (
        while Queue.length(&q) != 0 do (
            dbg.print({ .before_pop = Queue.length(&q) });
            let state = Queue.pop(&mut q);
            dbg.print({ .after_pop = Queue.length(&q) });
            # if state == machine.target_state then break;
            # dbg.print (.q, .state);
            let state_d = (ArrayList.at(&d, state))^;
            let dir = if state_d < 0 then -1 else +1;
            for { i, &changes } in ArrayList.iter(&machine.buttons) |> std.iter.enumerate do (
                let new_state = std.op.bit_xor(state, changes);
                let new_state_d = ArrayList.at_mut(&mut d, new_state);
                if new_state_d^ == 0 then (
                    # dbg.print (.state, .dir, .new_state, .button = i, .changes);
                    new_state_d^ = state_d + dir;
                    Queue.push(&mut q, new_state);
                ) else if (new_state_d^ < 0) != (state_d < 0) then (
                    machine_answer = abs(state_d) + abs(new_state_d^) - 1;
                    unwind bfs ();
                );
            );
        );
    );
    if machine_answer < 0 then (
        panic("couldnt solve the machine");
    );
    
    machine_answer
);
const Matrix = (
    module:
    const t = newtype { .rows :: ArrayList.t[ArrayList.t[Int32]] };
    const new = (n, m) -> t => (
        let mut rows = ArrayList.new();
        for _ in 0..n do (
            let mut row = ArrayList.new();
            for _ in 0..m do (
                ArrayList.push_back(&mut row, 0);
            );
            
            ArrayList.push_back(&mut rows, row);
        );
        { .rows }
    );
    const at_mut = (a :: &mut t, i :: Int32, j :: Int32) -> &mut Int32 => (
        ArrayList.at_mut(ArrayList.at_mut(&mut a^.rows, i), j)
    );
    const at = (a :: &t, i, j) => (
        (ArrayList.at(ArrayList.at(&a^.rows, i), j))^
    );
    const row_mut = (a :: &mut t, i) -> &mut ArrayList.t[Int32] => (
        ArrayList.at_mut(&mut a^.rows, i)
    );
    const print = (a :: &t) => (
        for row in ArrayList.iter(&a^.rows) do (
            let mut s = "";
            for &x in ArrayList.iter(row) do (
                s += " " + to_string(x);
            );
            
            std.io.print(s);
        );
    );
    const size = (a :: &t) -> { Int32, Int32 } => {
        ArrayList.length(&a^.rows),
        ArrayList.length(ArrayList.at(&a^.rows, 0))
    };
);
let swap = [T] (a :: &mut T, b :: &mut T) => (
    { a^, b^ } = { b^, a^ };
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
let gauss_elimination = (a :: &mut Matrix.t) => (
    if verbose then (
        print("[INFO] before gauss");
        Matrix.print(&a^);
    );
    let { n, m } = Matrix.size(&a^);
    let { mut i, mut j } = { 0, 0 };
    let mut free = ArrayList.new();
    let mut pivot = ArrayList.new();
    loop (
        if i >= n or j >= m then break;
        let mut non_zero_row = -1;
        for row in i..n do (
            if Matrix.at(&a^, row, j) != 0 then (
                non_zero_row = row;
            );
        );
        if non_zero_row == -1 then (
            if j + 1 < m then (
                ArrayList.push_back(&mut free, j);
            );
            
            j += 1;
            continue;
        );
        if j + 1 < m then (
            ArrayList.push_back(&mut pivot, { .row = i, .var = j });
        );
        let row = Matrix.row_mut(a, i);
        swap(row, Matrix.row_mut(a, non_zero_row));
        let at_row = (ArrayList.at(&row^, j))^;
        for lower in i + 1..n do (
            let lower = Matrix.row_mut(a, lower);
            if (ArrayList.at(&lower^, j))^ % at_row != 0 then (
                # we can't safely divide, so we scale
                # the whole row up to compensate
                for x in ArrayList.iter_mut(lower) do (
                    x^ *= at_row;
                );
            );
            let mult = (ArrayList.at(&lower^, j))^ / at_row;
            for k in j..m do (
                let at_row = (ArrayList.at(&row^, k))^;
                let at_lower = (ArrayList.at_mut(lower, k));
                at_lower^ -= at_row * mult;
            );
        );
        
        i += 1;
        j += 1;
    );
    for j in j..m - 1 do (
        ArrayList.push_back(&mut free, j);
    );
    if verbose then (
        print("[INFO] after gauss");
        Matrix.print(&a^);
        print("====");
    );
    
    print("free vars = " + ArrayList.to_string(&free, &x => to_string(x)));
    {
        .free,
        .pivot,
    }
);
let pow = (x, n) => (
    let mut result = 1;
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
            | :Node (node) => (
                iter_rev(node.right, f);
                f(&node.value);
                iter_rev(node.left, f);
            )
        )
    )
);
let mut brute = (
    module:
    let mut machine_answer = 0;
    let mut n = 0;
    let mut m = 0;
    let mut a = Matrix.new(0, 0);
    let mut vars = ArrayList.new();
    let mut free = ArrayList.new();
    let mut pivot = ArrayList.new();
    let mut current_answer = 0;
    let check = () => with_return (
        # print "TRY:";
        # ArrayList.iter (
        #     &free,
        #     &var => (
        #         let value = (ArrayList.at (&vars, var))^;
        #         # print (to_string var + " = " + to_string value);
        #         current_answer += value;
        #     ),
        # );
        # 300 * 300 * 300 * 10 * 10 * 100000
        # print "pivot:";
        for i in (0..ArrayList.length(&pivot)).rev() do (
            let &{ .var, .row } = ArrayList.at(&pivot, i);
            let row = Matrix.row_mut(&mut a, row);
            let mut result = (ArrayList.at(&row^, m - 1))^;
            for j in var + 1..m - 1 do (
                result -= (ArrayList.at(&vars, j))^ * (ArrayList.at(&row^, j))^;
            );
            let k = (ArrayList.at(&row^, var))^;
            if result % k != 0 then return;
            result /= k;
            if result < 0 then return;
            current_answer += result;
            if current_answer >= machine_answer then return;
            # print (to_string var + " = " + to_string result);
            (
                ArrayList.at_mut(&mut vars, var)
            )^ = result;
        );
        
        # print ("current = " + to_string current_answer);
        machine_answer = min(machine_answer, current_answer);
    );
    let force = (i, .max) => (
        if i < 0 then (
            let save = current_answer;
            check();
            current_answer = save;
        ) else (
            for x in 0..max + 1 do (
                # print ("BRUTE " + to_string i + " = " + to_string x);
                let free_var_idx = (ArrayList.at(&free, i))^;
                (ArrayList.at_mut(&mut vars, free_var_idx))^ = x;
                current_answer += x;
                force(i - 1, .max = max - x);
                current_answer -= x;
            );
        )
    );
);
let mut max_combinations = 0;
let solve_part2 = (machine :: Machine) -> Int32 => with_return (
    # return 0;
    let n = ArrayList.length(&machine.joltages);
    let m = ArrayList.length(&machine.buttons) + 1;
    let mut a = Matrix.new(n, m);
    (
        let mut i = 0;
        for &(mut button) in ArrayList.iter(&machine.buttons) do (
            for j in 0..n do (
                if std.op.bit_and(button, 1) != 0 then (
                    (Matrix.at_mut(&mut a, j, i))^ = 1;
                );
                
                button = std.op.bit_shift_right(button, 1);
            );
            
            i += 1;
        );
        let mut i = 0;
        for &x in ArrayList.iter(&machine.joltages) do (
            (Matrix.at_mut(&mut a, i, m - 1))^ = x;
            i += 1;
        );
    );
    let { .free, .pivot } = gauss_elimination(&mut a);
    let mut max_joltage = 0;
    for &x in ArrayList.iter(&machine.joltages) do (
        max_joltage = max(max_joltage, x);
    );
    
    print("max_joltage = " + to_string(max_joltage));
    let combinations = pow(max_joltage, ArrayList.length(&free));
    max_combinations = max(max_combinations, combinations);
    print("combinations(max^free) = " + to_string(combinations));
    let mut vars = ArrayList.new();
    for _ in 0..m - 1 do (
        ArrayList.push_back(&mut vars, 0);
    );
    
    brute.machine_answer = 1000000000;
    brute.a = a;
    brute.n = n;
    brute.m = m;
    brute.vars = vars;
    brute.free = free;
    brute.pivot = pivot;
    brute.force(ArrayList.length(&free) - 1, .max = max_joltage);
    
    print("answer = " + to_string(brute.machine_answer));
    brute.machine_answer
);
let mut max_buttons = 0;
let mut max_joltages = 0;
let mut max_joltage = 0;
let mut idx = 0;
let mut answer = 0;
for line in String.lines(input) do (
    if String.length(line) == 0 then continue;
    let machine = parse_machine(line);
    
    max_buttons = max(max_buttons, ArrayList.length(&machine.buttons));
    max_joltages = max(max_joltages, ArrayList.length(&machine.joltages));
    for &x in ArrayList.iter(&machine.joltages) do (
        max_joltage = max(max_joltage, x);
    );
    
    idx += 1;
    print("[INFO] read machine #" + to_string(idx));
    let machine_answer = if part1 then (
        solve_part1(machine)
    ) else (
        
        solve_part2(machine)
    );
    
    dbg.print({ .idx, .machine_answer });
    print("[INFO] machine answer #" + to_string(idx) + " = " + to_string(machine_answer));
    answer += machine_answer;
);

dbg.print({ .max_buttons, .max_joltages, .max_joltage, .max_combinations });
dbg.print(answer);
assert_answers(
    answer,
    .example = { .part1 = 7, .part2 = 33 },
    .part1 = 527,
    .part2 = 19810,
);
