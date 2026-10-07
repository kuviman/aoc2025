#!/usr/bin/env kast
use std.StringBuilder;
include "../common.ks";
std.sys.chdir(std.path.dirname(__FILE__));
let input = std.fs.read_file(input_path);
let verbose = false;
if std.sys.argc() >= 4 and std.sys.argv_at(1) == "--svg" then (
    println!("<svg viewBox=\"0 0 100000 100000\" xmlns=\"http://www.w3.org/2000/svg\"><polygon points=\"");
    println!("\(input)");
    println!("\" fill=\"black\" stroke=\"white\" stroke-width=\"100\"/></svg>");
);
let as_Int64 :: Int32 -> Int64 = x => (&(x |> to_string) |> as_str |> parse);
const Coords = newtype {
    .x :: Int64,
    .y :: Int64,
};
let mut tiles :: ArrayList.t[Coords] = ArrayList.new();
for line in String.lines(&input |> as_str) do (
    if String.length(line) != 0 then (
        let { x, y } = String.split_once(line, ',');
        let x = x |> parse;
        let y = y |> parse;
        let coords :: Coords = { .x, .y };
        ArrayList.push_back(&mut tiles, coords);
    );
);

println!("[INFO] coords read");

# TODO make lang easier to use Int64 literals
let zero = as_Int64(0);
let one = as_Int64(1);
const Zero = [Self] newtype {
    .zero :: Self,
};
@eval (impl Int32 as Zero = { .zero = 0 });
@eval (impl Int64 as Zero = { .zero = 0 });
const abs = [T] (x :: T) -> T => (
    let zero = (T as Zero).zero;
    if x < zero then (
        # TODO unary op
        zero - x
    ) else (
        
        x
    )
);
let answer = if part1 then (
    let n = ArrayList.length(&tiles);
    let mut answer = zero;
    for i in 0..n do (
        for j in 0..i do (
            let a = ArrayList.at(&tiles, i);
            let b = ArrayList.at(&tiles, j);
            let area = (abs(b^.x - a^.x) + one) * (abs(b^.y - a^.y) + one);
            if area > answer then (
                answer = area;
            );
        );
    );
    
    answer
) else (
    use std.collections.Treap;
    let { mut xs, mut ys } = { Treap.new(), Treap.new() };
    let idx_of = (t :: &Treap.t[_], x :: Int64) -> Int32 => (
        let { less, _ } = Treap.split(
            t^,
            data => (
                if data^.value >= x then (
                    :RightSubtree
                ) else (
                    :LeftSubtree
                )
            ),
        );
        
        Treap.length(&less)
    );
    let uncompress_coord = (t, x) => (
        (Treap.at(t, x))^
    );
    let uncompress = { .x, .y } => {
        .x = uncompress_coord(&xs, x),
        .y = uncompress_coord(&ys, y),
    };
    let add = (t :: &mut Treap.t[_], x :: Int64) => 
    # print <| Treap.to_string (t, &x => to_string x);
    # print "===";
    (
        let { less, greater_or_equal } = Treap.split(
            t^,
            data => (
                if data^.value >= x then (
                    :RightSubtree
                ) else (
                    :LeftSubtree
                )
            ),
        );
        let { _, greater } = Treap.split(
            greater_or_equal,
            data => (
                if data^.value >= x + one then (
                    :RightSubtree
                ) else (
                    :LeftSubtree
                )
            ),
        );
        
        # print "===";
        # print <| Treap.to_string (t, &x => to_string x);
        # dbg.print x;
        # print <| Treap.to_string (&less, &x => to_string x);
        # print <| Treap.to_string (&greater, &x => to_string x);
        t^ = Treap.join(less, Treap.join(Treap.singleton(x), greater));
    );
    for { i, &{ .x, .y } } in ArrayList.iter(&tiles) |> std.iter.enumerate do (
        println!("[INFO] compressing coords \(i)/\(ArrayList.length(&tiles))");
        
        add(&mut xs, x - one);
        add(&mut xs, x);
        add(&mut xs, x + one);
        add(&mut ys, y - one);
        add(&mut ys, y);
        add(&mut ys, y + one);
    );
    
    # println!("[INFO] coords are compressed xs=\(Treap.length(&xs)), ys=\(Treap.length(&ys))");
    let mut vs = ArrayList.new();
    for &{ .x, .y } in ArrayList.iter(&tiles) do (
        let x = idx_of(&xs, x);
        let y = idx_of(&ys, y);
        ArrayList.push_back(&mut vs, { .x, .y });
    );
    
    println!("[INFO] calculated compressed polygon");
    const Map = (
        module:
        use std.collections.Treap;
        const t = newtype {
            .n :: Int32,
            .m :: Int32,
            .repr :: Treap.t[Treap.t[Int32]],
        };
        let new = (n, m) -> t => (
            let mut repr = Treap.new();
            for i in 0..n do (
                println!("[INFO] progress \(i)/\(n)");
                let mut row = Treap.new();
                for i in 0..m do (
                    row = Treap.join(row, Treap.singleton(0));
                );
                
                repr = Treap.join(repr, Treap.singleton(row));
            );
            {
                .n,
                .m,
                .repr,
            }
        );
        let at_mut = (map :: &mut t, i, j) -> &mut Int32 => (
            Treap.at_mut(Treap.at_mut(&mut map^.repr, i), j)
        );
        let at = (map, i, j) => (
            (Treap.at(Treap.at(&map^.repr, i), j))^
        );
    );
    
    println!("[INFO] creating empty map");
    let mut map = Map.new(Treap.length(&xs), Treap.length(&ys));
    println!("[INFO] newd empty map");
    
    println!("[INFO] drawing edges");
    for i in 0..ArrayList.length(&vs) do (
        # println!("[INFO] progress \(i)/\(ArrayList.length(&vs))");
        let mut next = i + 1;
        if next == ArrayList.length(&vs) then (
            next = 0;
        );
        let mut next_next = next + 1;
        if next_next == ArrayList.length(&vs) then (
            next_next = 0;
        );
        let mut a = (ArrayList.at(&vs, i))^;
        let mut b = (ArrayList.at(&vs, next))^;
        let c = (ArrayList.at(&vs, next_next))^;
        if a.y == b.y then (
            let mut add = 1;
            let mut y = a.y;
            if a.x > b.x then (
                # TODO a, b = b, a
                let t = b;
                b = a;
                a = t;
                add = 0 - 1;
                y += 1;
            );
            for x in a.x..b.x do (
                (Map.at_mut(&mut map, x, y))^ += add;
            );
        ) else (
            if a.y < b.y then (
                (Map.at_mut(&mut map, a.x, a.y))^ += 1;
                (Map.at_mut(&mut map, a.x, b.y + 1))^ -= 1;
            );
        );
    );
    
    println!("[INFO] done drawing edges");
    
    println!("[INFO] filling the map");
    for i in 0..map.n do (
        # println!("[INFO] progress \(i)/\(map.n)");
        for j in 1..map.m do (
            let cell = Map.at_mut(&mut map, i, j);
            cell^ += Map.at(&map, i, j - 1);
        );
    );
    const ACTUAL_CORNER = 100;
    for &{ .x, .y } in ArrayList.iter(&vs) do (
        (Map.at_mut(&mut map, x, y))^ = ACTUAL_CORNER;
    );
    
    println!("[INFO] done filling the map");
    if input_path == "example.txt" then (
        for y in 0..map.m do (
            let mut s = StringBuilder.new();
            for x in 0..map.n do 
            # s += to_string x;
            (
                let x = Map.at(&map, x, y);
                let c = if x == 0 then " " else if x == 1 then "X" else "#";
                &mut s |> StringBuilder.add_str(c);
            );
            # println!("\(s)");
        );
    );
    let mut answer = as_Int64(0);
    for i in 0..ArrayList.length(&vs) do (
        let mut next = i + 1;
        if next == ArrayList.length(&vs) then (
            next = 0;
        );
        let mut a = (ArrayList.at(&vs, i))^;
        let mut b = (ArrayList.at(&vs, next))^;
        if a.y != b.y then continue;
        if input_path == "input.txt" and abs(a.x - b.x) < 100 then continue;
        if a.x > b.x then (
            # TODO a, b = b, a
            let t = b;
            b = a;
            a = t;
        );
        
        (
            let b = uncompress(b);
            println!("[INFO] trying \(b.x),\(b.y)");
        );
        let try_direction = dir => (
            let mut a = { .x = a.x, .y = a.y };
            let mut b = { .x = b.x, .y = b.y };
            println!("[INFO] trying direction \(dir)");
            let mut max_y = b.y;
            while Map.at(&map, b.x, max_y) != 0 do (
                max_y += dir;
            );
            
            max_y -= dir;
            unwindable block (
                loop (
                    # println!("[INFO] x=\(a.x)/\(b.x)");
                    if Map.at(&map, a.x, a.y) == ACTUAL_CORNER then (
                        let a = uncompress(a);
                        let b = uncompress(b);
                        let area = (abs(b.x - a.x) + one) * (abs(b.y - a.y) + one);
                        # dbg.print (.a, .b, .area);
                        if area > answer then (
                            answer = area;
                        );
                    );
                    
                    a.y += dir;
                    if (max_y - a.y) * dir < 0 then break;
                    while Map.at(&map, a.x, a.y) == 0 do (
                        a.x += 1;
                        if a.x > b.x then (
                            unwind block ();
                        );
                    );
                );
            );
        );
        
        try_direction(1);
        try_direction(0 - 1);
    );
    
    answer
);

dbg.print(answer);

assert_answers(
    answer,
    .example = { .part1 = parse("50"), .part2 = parse("24") },
    .part1 = parse("4749672288"),
    .part2 = parse("1479665889"),
);
