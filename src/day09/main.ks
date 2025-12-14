#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let verbose = false;

if std.sys.argc () >= 4 and std.sys.argv_at 1 == "--svg" then (
    print "<svg viewBox=\"0 0 100000 100000\" xmlns=\"http://www.w3.org/2000/svg\"><polygon points=\"";
    print input;
    print "\" fill=\"black\" stroke=\"white\" stroke-width=\"100\"/></svg>";
);

let as_int64 :: int32 -> int64 = x => (x |> to_string |> parse);

const Coords = type (
    .x :: int64,
    .y :: int64,
);

let tiles :: list.t[Coords] = list.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 then (
            let x, y = String.split_once (line, ',');
            let x = x |> parse;
            let y = y |> parse;
            let coords :: Coords = (.x, .y);
            list.push_back (&tiles, coords);
        );
    ),
);

print "[INFO] coords read";

# TODO make lang easier to use int64 literals
let zero = as_int64 0;
let one = as_int64 1;

const Zero = [Self] type (
    .zero :: Self,
);

impl int32 as Zero = (.zero = 0);
impl int64 as Zero = (.zero = as_int64 0);

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
    let n = list.length &tiles;
    let answer = zero;
    for i in 0..n do (
        for j in 0..i do (
            let a = list.at (&tiles, i);
            let b = list.at (&tiles, j);
            let area = (abs (b^.x - a^.x) + one) * (abs (b^.y - a^.y) + one);
            if area > answer then (
                answer = area;
            );
        );
    );
    answer
) else (
    use std.collections.treap;
    let xs, ys = treap.create (), treap.create ();
    let idx_of = (t :: &treap.t[_], x :: int64) -> int32 => (
        let less, _ = treap.split (
            t^,
            data => (
                if data^.value >= x then (
                    :LeftSubtree
                ) else (
                    :RightSubtree
                )
            ),
        );
        treap.length &less
    );
    let uncompress_coord = (t, x) => (
        (treap.at (t, x))^
    );
    let uncompress = (.x, .y) => (
        .x = uncompress_coord (&xs, x),
        .y = uncompress_coord (&ys, y),
    );
    let add = (t :: &treap.t[_], x :: int64) => (
        let less, greater_or_equal = treap.split (
            t^,
            data => (
                if data^.value >= x then (
                    :LeftSubtree
                ) else (
                    :RightSubtree
                )
            ),
        );
        let _, greater = treap.split (
            greater_or_equal,
            data => (
                if data^.value >= x + one then (
                    :LeftSubtree
                ) else (
                    :RightSubtree
                )
            ),
        );
        # print "===";
        # print <| treap.to_string (t, &x => to_string x);
        # dbg.print x;
        # print <| treap.to_string (&less, &x => to_string x);
        # print <| treap.to_string (&greater, &x => to_string x);
        t^ = treap.join (less, treap.join (treap.singleton x, greater));
        # print <| treap.to_string (t, &x => to_string x);
        # print "===";
    );
    let i = 0;
    list.iter (
        &tiles,
        &(.x, .y) => (
            print (
                "[INFO] compressing coords "
                + (to_string i)
                + "/"
                + (to_string <| list.length &tiles)
            );
            add (&xs, x - one);
            add (&xs, x);
            add (&xs, x + one);
            add (&ys, y - one);
            add (&ys, y);
            add (&ys, y + one);
            i += 1;
        ),
    );
    print (
        "[INFO] coords are compressed xs="
        + (to_string <| treap.length &xs)
        + ", ys="
        + (to_string <| treap.length &ys)
    );
    
    let vs = list.create ();
    list.iter (
        &tiles,
        &(.x, .y) => (
            let x = idx_of (&xs, x);
            let y = idx_of (&ys, y);
            list.push_back (&vs, (.x, .y));
        ),
    );
    if verbose then (
        print (
            treap.to_string (
                &vs.inner,
                &(.x, .y) => (
                    to_string x + " " + to_string y
                ),
            )
        );
    );
    print "[INFO] calculated compressed polygon";
    
    const Map = (
        module:
        use std.collections.treap;
        const t = type (
            .n :: int32,
            .m :: int32,
            .repr :: treap.t[treap.t[int32]],
        );
        let create = (n, m) -> t => (
            let repr = treap.create ();
            for i in 0..n do (
                print (
                    "[INFO] progress "
                    + (to_string i)
                    + "/"
                    + (to_string n)
                );
                let row = treap.create ();
                for i in 0..m do (
                    row = treap.join (row, treap.singleton 0);
                );
                repr = treap.join (repr, treap.singleton row);
            );
            (.n, .m, .repr)
        );
        let at_mut = (map :: &t, i, j) -> &int32 => (
            treap.at (treap.at (&map^.repr, i), j)
        );
        let at = (map, i, j) => (at_mut (map, i, j))^;
    );
    print "[INFO] creating empty map";
    let map = Map.create (treap.length &xs, treap.length &ys);
    print "[INFO] created empty map";
    
    print "[INFO] drawing edges";
    for i in 0..list.length &vs do (
        print (
            "[INFO] progress "
            + (to_string i)
            + "/"
            + (to_string <| list.length &vs)
        );
        let next = i + 1;
        if next == list.length &vs then (
            next = 0;
        );
        let next_next = next + 1;
        if next_next == list.length &vs then (
            next_next = 0;
        );
        let a = (list.at (&vs, i))^;
        let b = (list.at (&vs, next))^;
        let c = (list.at (&vs, next_next))^;
        if a.y == b.y then (
            let add = 1;
            let y = a.y;
            if a.x > b.x then (
                # TODO a, b = b, a
                let t = b;
                b = a;
                a = t;
                add = 0 - 1;
                y += 1;
            );
            for x in a.x..b.x do (
                (Map.at_mut (&map, x, y))^ += add;
            );
        ) else (
            if a.y < b.y then (
                (Map.at_mut (&map, a.x, a.y))^ += 1;
                (Map.at_mut (&map, a.x, b.y + 1))^ -= 1;
            );
        );
    );
    print "[INFO] done drawing edges";
    
    print "[INFO] filling the map";
    for i in 0..map.n do (
        print (
            "[INFO] progress "
            + (to_string i)
            + "/"
            + (to_string map.n)
        );
        for j in 1..map.m do (
            let cell = Map.at_mut (&map, i, j);
            cell^ += Map.at (&map, i, j - 1);
        );
    );
    const ACTUAL_CORNER = 100;
    list.iter (
        &vs,
        &(.x, .y) => (
            (Map.at_mut (&map, x, y))^ = ACTUAL_CORNER;
        ),
    );
    print "[INFO] done filling the map";
    
    if input_path == "example.txt" then (
        for y in 0..map.m do (
            let s = "";
            for x in 0..map.n do (
                let x = Map.at (&map, x, y);
                let c = if x == 0 then " " else if x == 1 then "X" else "#";
                s += c;
                # s += to_string x;
            );
            print s;
        );
    );
    
    let answer = as_int64 0;
    for i in 0..list.length &vs do (
        let next = i + 1;
        if next == list.length &vs then (
            next = 0;
        );
        let a = (list.at (&vs, i))^;
        let b = (list.at (&vs, next))^;
        if a.y != b.y then continue;
        if input_path == "input.txt" and abs (a.x - b.x) < 100 then continue;
        if a.x > b.x then (
            # TODO a, b = b, a
            let t = b;
            b = a;
            a = t;
        );
        print (
            "[INFO] trying "
            + (
                let b = uncompress b;
                (to_string b.x) + "," + (to_string b.y)
            )
        );
        let try_direction = dir => (
            let a = (.x = a.x, .y = a.y);
            let b = (.x = b.x, .y = b.y);
            print ("[INFO] trying direction " + to_string dir);
            let max_y = b.y;
            while Map.at (&map, b.x, max_y) != 0 do (
                max_y += dir;
            );
            max_y -= dir;
            unwindable block (
                loop (
                    print ("[INFO] x=" + (to_string a.x) + "/" + (to_string b.x));
                    if Map.at (&map, a.x, a.y) == ACTUAL_CORNER then (
                        let a = uncompress a;
                        let b = uncompress b;
                        let area = (abs (b.x - a.x) + one) * (abs (b.y - a.y) + one);
                        # dbg.print (.a, .b, .area);
                        if area > answer then (
                            answer = area;
                        );
                    );
                    a.y += dir;
                    if (max_y - a.y) * dir < 0 then break;
                    while Map.at (&map, a.x, a.y) == 0 do (
                        a.x += 1;
                        if a.x > b.x then (
                            unwind block ();
                        );
                    );
                );
            );
        );
        try_direction 1;
        try_direction (0 - 1);
    );
    answer
);

dbg.print answer;

assert_answers (
    answer,
    .example = (.part1 = parse "50", .part2 = parse "24"),
    .part1 = parse "4749672288",
    .part2 = parse "1479665889",
);
