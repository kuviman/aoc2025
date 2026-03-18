#!/usr/bin/env kast
include "../common.ks";
std.sys.chdir(std.path.dirname(__FILE__));
let input = std.fs.read_file(input_path);
let as_Int64 :: Int32 -> Int64 = x => (x |> to_string |> parse);
const Point = newtype {
    .x :: Int64,
    .y :: Int64,
    .z :: Int64,
};

# ./main.ks 100 --part2 input.txt
let max_points = if std.sys.argc() >= 4 then (
    std.sys.argv_at(1) |> parse
) else (
    
    10000
);
let mut points :: ArrayList.t[Point] = ArrayList.new();
for line in String.lines(input) do (
    if String.length(line) != 0 and ArrayList.length(&points) < max_points then (
        let mut coords = ArrayList.new();
        for part in String.split(line, ',') do (
            ArrayList.push_back(&mut coords, part |> parse);
        );
        let point = {
            .x = ArrayList.at(&coords, 0)^,
            .y = ArrayList.at(&coords, 1)^,
            .z = ArrayList.at(&coords, 2)^,
        };
        
        ArrayList.push_back(
            &mut points,
            point,
        );
    );
);

print("[INFO] Input read");
let pairs_to_connect = if input_path == "example.txt" then 10 else 1000;
let n = ArrayList.length(&points);
let sqr = x => x * x;
let sqr_distance = (&a, &b) => (
    sqr(a.x - b.x) + sqr(a.y - b.y) + sqr(a.z - b.z)
);
const PairSet = (
    module:
    use std.collections.Treap;
    const data = newtype { Int32, Int32, .sqr_d :: Int64 };
    const t = newtype { .inner :: Treap.t[data] };
    const new = () -> t => {
        .inner = Treap.new(),
    };
    const length = (set :: &t) -> Int32 => (
        Treap.length(&set^.inner)
    );
    const add = (set :: &mut t, d :: data) => (
        let { left, right } = Treap.split(
            set^.inner,
            node => (
                if d.sqr_d < node^.value.sqr_d then (
                    :RightSubtree
                ) else (
                    :LeftSubtree
                )
            ),
        );
        
        set^.inner = Treap.join(
            left,
            Treap.join(
                Treap.singleton(d),
                right,
            ),
        );
    );
    const keep_minimum = (set :: &mut t, n :: Int32) => (
        if length(&set^) > n then (
            let { left, _right } = Treap.split_at(set^.inner, n);
            set^.inner = left;
        );
    );
    const iter = (set :: &t) => (
        Treap.iter(&set^.inner)
    );
);
let mut pairs = PairSet.new();
for i in 0..n do (
    print(
        "[INFO] Progress "
        + (to_string(i))
        + "/"
        + (to_string(n))
        + ", pairs="
        + (to_string <| PairSet.length(&pairs))
    );
    for j in 0..i do (
        let p_i = ArrayList.at(&points, i);
        let p_j = ArrayList.at(&points, j);
        let sqr_d = sqr_distance(p_i, p_j);
        # dbg.print (p_i^, p_j^, .sqr_d, .pairs_to_connect);
        PairSet.add(&mut pairs, { i, j, .sqr_d });
        if part1 then (
            PairSet.keep_minimum(&mut pairs, pairs_to_connect);
        );
    );
);
const DSU = (
    module:
    const root = newtype {
        .id :: Int32,
        .count :: Int32,
    };
    const node = newtype (
        | :Root { .data :: root }
        | :NonRoot { .closer_to_root :: &mut node }
    );
    let mut next_id = 0;
    let new_node = () -> node => (
        next_id += 1;
        :Root {
            .data = {
                .id = next_id,
                .count = 1,
            },
        }
    );
    const find_root_node = (v :: &mut node) -> &mut node => (
        match v^ with (
            | :Root _ => v
            | :NonRoot { .closer_to_root } => (
                let root = find_root_node(closer_to_root);
                v^ = :NonRoot { .closer_to_root = root };
                root
            )
        )
    );
    const find_root = (v :: &mut node) -> &mut root => (
        match (find_root_node(v))^ with (
            # TODO ref mut
            | :Root (mut root) => &mut root.data
            | :NonRoot (_) => panic("bug")
        )
    );
    const merge = (a :: &mut node, b :: &mut node) -> Bool => (
        let a = find_root_node(a);
        let b = find_root_node(b);
        let root_a = find_root(a);
        let root_b = find_root(b);
        if root_a^.id != root_b^.id then (
            let attach = (a, b) => (
                let root_a = find_root(a);
                let root_b = find_root(b);
                root_b^.count += root_a^.count;
                a^ = :NonRoot { .closer_to_root = b };
            );
            if root_a^.count < root_b^.count then (
                attach(a, b);
            ) else (
                
                attach(b, a)
            );
            true
        ) else (
            false
        )
    );
    const is_same = (a :: &mut node, b :: &mut node) -> Bool => (
        let root_a = find_root(a);
        let root_b = find_root(b);
        root_a^.id == root_b^.id
    );
);
let mut nodes = ArrayList.new();
for i in 0..n do (
    ArrayList.push_back(&mut nodes, DSU.new_node());
);

# for &point in ArrayList.iter &points do (
#     dbg.print point;
# );
let mut answer_part2 = 0 |> as_Int64;
for &{ i, j, .sqr_d } in PairSet.iter(&pairs) do (
    # dbg.print (.points = points.inner, .i);
    let a = ArrayList.at(&points, i);
    let b = ArrayList.at(&points, j);
    # dbg.print ("merge", a^, b^);
    # print (
    #     "[INFO] merge "
    #     + (to_string i)
    #     + " and "
    #     + (to_string j)
    #     + " with sqr_d="
    #     + (to_string sqr_d)
    # );
    if DSU.merge(ArrayList.at_mut(&mut nodes, i), ArrayList.at_mut(&mut nodes, j)) then (
        answer_part2 = a^.x * b^.x;
    );
);
let answer = if part1 then (
    let mut visited = ArrayList.new();
    for i in 0..n do (
        ArrayList.push_back(&mut visited, false);
    );
    const sort_by = [T] (a :: &mut ArrayList.t[T], .less :: (&T, &T) -> Bool) => (
        use std.collections.Treap;
        let mut t = Treap.new();
        for &x in ArrayList.iter(&a^) do (
            let { left, right } = Treap.split(
                t,
                node => (
                    if less(&x, &node^.value) then (
                        :RightSubtree
                    ) else (
                        :LeftSubtree
                    )
                ),
            );
            
            t = Treap.join(
                left,
                Treap.join(
                    Treap.singleton(x),
                    right,
                ),
            );
        );
        
        a^.inner = t;
    );
    let mut component_sizes = ArrayList.new();
    for node in ArrayList.iter_mut(&mut nodes) do (
        let root = DSU.find_root(node);
        # dbg.print root^;
        # dbg.print <| (ArrayList.length &visited, root^.id - 1);
        let visited = ArrayList.at_mut(&mut visited, root^.id - 1);
        if not visited^ then (
            # dbg.print (.component = root^.count);
            visited^ = true;
            ArrayList.push_back(&mut component_sizes, root^.count);
        );
    );
    
    sort_by(&mut component_sizes, .less = (&a, &b) => a > b);
    let mut answer = 1;
    for i in 0..3 do (
        answer *= (ArrayList.at(&component_sizes, i))^;
    );
    
    as_Int64(answer)
) else (
    
    answer_part2
);

dbg.print(answer);

assert_answers(
    answer,
    .example = { .part1 = parse("40"), .part2 = parse("25272") },
    .part1 = parse("123420"),
    .part2 = parse("673096646"),
);
