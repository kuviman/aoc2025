#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let as_int64 :: int32 -> int64 = x => (x |> to_string |> parse);

const Point = type (
    .x :: int64,
    .y :: int64,
    .z :: int64,
);

# ./main.ks 100 --part2 input.txt
let max_points = if std.sys.argc () >= 4 then (
    std.sys.argv_at 1 |> parse
) else (
    10000
);

let points :: list.t[Point] = list.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 and list.length &points < max_points then (
            let coords = list.create ();
            String.split (
                line,
                ',',
                part => (
                    list.push_back (&coords, part |> parse);
                )
            );
            let point = (
                .x = (list.at (&coords, 0))^,
                .y = (list.at (&coords, 1))^,
                .z = (list.at (&coords, 2))^,
            );
            list.push_back (
                &points,
                point,
            );
        );
    ),
);

print "[INFO] Input read";

# list.iter (&points, &point => dbg.print point);
let pairs_to_connect = if input_path == "example.txt" then 10 else 1000;

let n = list.length &points;
let sqr = x => x * x;
let sqr_distance = (&a, &b) => (
    sqr (a.x - b.x) + sqr (a.y - b.y) + sqr (a.z - b.z)
);

const PairSet = (
    module:
    use std.collections.treap;
    
    const data = type (int32, int32, .sqr_d :: int64);
    const t = type (.inner :: treap.t[data]);
    
    const create = () -> t => (
        .inner = treap.create (),
    );
    
    const length = (set :: &t) -> int32 => (
        treap.length &set^.inner
    );
    
    const add = (set :: &t, d :: data) => (
        let left, right = treap.split (
            set^.inner,
            node => (
                if d.sqr_d < node^.value.sqr_d then (
                    :LeftSubtree
                ) else (
                    :RightSubtree
                )
            ),
        );
        set^.inner = treap.join (
            left,
            treap.join (
                treap.singleton d,
                right,
            ),
        );
    );
    
    const keep_minimum = (set :: &t, n :: int32) => (
        if length set > n then (
            let left, _right = treap.split_at (set^.inner, n);
            set^.inner = left;
        );
    );
    
    const iter = (set :: &t, f) => (
        treap.iter (&set^.inner, f);
    );
);
let pairs = PairSet.create ();

for i in 0..n do (
    print (
        "[INFO] Progress "
        + (to_string i)
        + "/"
        + (to_string n)
        + ", pairs="
        + (to_string <| PairSet.length &pairs)
    );
    for j in 0..i do (
        let p_i = list.at (&points, i);
        let p_j = list.at (&points, j);
        let sqr_d = sqr_distance (p_i, p_j);
        # dbg.print (p_i^, p_j^, .sqr_d, .pairs_to_connect);
        PairSet.add (&pairs, (i, j, .sqr_d));
        if part1 then (
            PairSet.keep_minimum (&pairs, pairs_to_connect);
        );
    );
);

const DSU = (
    module:
    
    const root = type (
        .id :: int32,
        .count :: int32,
    );
    const node = type (
        | :Root (
            .data :: root,
        )
        | :NonRoot (
            .closer_to_root :: &node,
        )
    );
    
    let next_id = 0;
    let new_node = () -> node => (
        next_id += 1;
        :Root (
            .data = (
                .id = next_id,
                .count = 1,
            ),
        )
    );
    
    const find_root_node = (v :: &node) -> &node => (
        match v^ with (
            | :Root _ => v
            | :NonRoot (.closer_to_root) => (
                let root = find_root_node closer_to_root;
                v^ = :NonRoot (.closer_to_root = root);
                root
            )
        )
    );
    
    const find_root = (v :: &node) -> &root => (
        match (find_root_node v)^ with (
            | :Root root => &root.data
            | :NonRoot _ => panic "bug"
        )
    );
    
    const merge = (a :: &node, b :: &node) -> bool => (
        let a = find_root_node a;
        let b = find_root_node b;
        let root_a = find_root a;
        let root_b = find_root b;
        if root_a^.id != root_b^.id then (
            let attach = (a, b) => (
                let root_a = find_root a;
                let root_b = find_root b;
                root_b^.count += root_a^.count;
                a^ = :NonRoot (.closer_to_root = b);
            );
            if root_a^.count < root_b^.count then (
                attach (a, b);
            ) else (
                attach (b, a)
            );
            true
        ) else (
            false
        )
    );
    
    const is_same = (a :: &node, b :: &node) -> bool => (
        let root_a = find_root a;
        let root_b = find_root b;
        root_a^.id == root_b^.id
    );
);

let nodes = list.create ();
for i in 0..n do (
    list.push_back (&nodes, DSU.new_node ())
);

let answer_part2 = 0 |> as_int64;
PairSet.iter (
    &pairs,
    &(i, j, .sqr_d) => (
        let a = list.at (&points, i);
        let b = list.at (&points, j);
        # dbg.print ("merge", a^, b^);
        # print (
        #     "[INFO] merge "
        #     + (to_string i)
        #     + " and "
        #     + (to_string j)
        #     + " with sqr_d="
        #     + (to_string sqr_d)
        # );
        if DSU.merge (list.at (&nodes, i), list.at (&nodes, j)) then (
            answer_part2 = a^.x * b^.x;
        );
    ),
);

let answer = if part1 then (
    let visited = list.create ();
    for i in 0..n do (
        list.push_back (&visited, false);
    );
    
    const sort_by = [T] (a :: &list.t[T], .less :: (&T, &T) -> bool) => (
        use std.collections.treap;
        let t = treap.create ();
        list.iter (
            a,
            &x => (
                let left, right = treap.split (
                    t,
                    node => (
                        if less (&x, &node^.value) then (
                            :LeftSubtree
                        ) else (
                            :RightSubtree
                        )
                    ),
                );
                t = treap.join (
                    left,
                    treap.join (
                        treap.singleton x,
                        right,
                    ),
                );
            ),
        );
        a^.inner = t;
    );
    
    let component_sizes = list.create ();
    list.iter (
        &nodes,
        node => (
            let root = DSU.find_root node;
            # dbg.print root^;
            # dbg.print <| (list.length &visited, root^.id - 1);
            let visited = list.at (&visited, root^.id - 1);
            if not visited^ then (
                visited^ = true;
                list.push_back (&component_sizes, root^.count);
                # dbg.print root^.count;
            );
        ),
    );
    sort_by (&component_sizes, .less = (&a, &b) => a > b);
    
    let answer = 1;
    for i in 0..3 do (
        answer *= (list.at (&component_sizes, i))^;
    );
    
    as_int64 answer
) else (
    answer_part2
);

dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "40", .part2 = parse "25272"),
    .part1 = parse "123420",
    .part2 = parse "673096646",
);
