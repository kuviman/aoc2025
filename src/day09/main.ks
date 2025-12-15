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

let as_Int64 :: Int32 -> Int64 = x => (x |> to_string |> parse);

const Coords = type (
    .x :: Int64,
    .y :: Int64,
);

let tiles :: List.t[Coords] = List.create ();
String.lines (
    input,
    line => (
        if String.length line != 0 then (
            let x, y = String.split_once (line, ',');
            let x = x |> parse;
            let y = y |> parse;
            let coords :: Coords = (.x, .y);
            List.push_back (&tiles, coords);
        );
    ),
);

print "[INFO] coords read";

# TODO make lang easier to use Int64 literals
let zero = as_Int64 0;
let one = as_Int64 1;

const Zero = [Self] type (
    .zero :: Self,
);

impl Int32 as Zero = (.zero = 0);
impl Int64 as Zero = (.zero = as_Int64 0);

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
    let n = List.length &tiles;
    let answer = zero;
    for i in 0..n do (
        for j in 0..i do (
            let a = List.at (&tiles, i);
            let b = List.at (&tiles, j);
            let area = (abs (b^.x - a^.x) + one) * (abs (b^.y - a^.y) + one);
            if area > answer then (
                answer = area;
            );
        );
    );
    answer
) else (
    use std.collections.Treap;
    let xs, ys = Treap.create (), Treap.create ();
    let idx_of = (t :: &Treap.t[_], x :: Int64) -> Int32 => (
        let less, _ = Treap.split (
            t^,
            data => (
                if data^.value >= x then (
                    :RightSubtree
                ) else (
                    :LeftSubtree
                )
            ),
        );
        Treap.length &less
    );
    let uncompress_coord = (t, x) => (
        (Treap.at (t, x))^
    );
    let uncompress = (.x, .y) => (
        .x = uncompress_coord (&xs, x),
        .y = uncompress_coord (&ys, y),
    );
    let add = (t :: &Treap.t[_], x :: Int64) => (
        let less, greater_or_equal = Treap.split (
            t^,
            data => (
                if data^.value >= x then (
                    :RightSubtree
                ) else (
                    :LeftSubtree
                )
            ),
        );
        let _, greater = Treap.split (
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
        t^ = Treap.join (less, Treap.join (Treap.singleton x, greater));
        # print <| Treap.to_string (t, &x => to_string x);
        # print "===";
    );
    let i = 0;
    List.iter (
        &tiles,
        &(.x, .y) => (
            print (
                "[INFO] compressing coords "
                + (to_string i)
                + "/"
                + (to_string <| List.length &tiles)
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
        + (to_string <| Treap.length &xs)
        + ", ys="
        + (to_string <| Treap.length &ys)
    );
    
    let vs = List.create ();
    List.iter (
        &tiles,
        &(.x, .y) => (
            let x = idx_of (&xs, x);
            let y = idx_of (&ys, y);
            List.push_back (&vs, (.x, .y));
        ),
    );
    if verbose then (
        print (
            Treap.to_string (
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
        use std.collections.Treap;
        const t = type (
            .n :: Int32,
            .m :: Int32,
            .repr :: Treap.t[Treap.t[Int32]],
        );
        let create = (n, m) -> t => (
            let repr = Treap.create ();
            for i in 0..n do (
                print (
                    "[INFO] progress "
                    + (to_string i)
                    + "/"
                    + (to_string n)
                );
                let row = Treap.create ();
                for i in 0..m do (
                    row = Treap.join (row, Treap.singleton 0);
                );
                repr = Treap.join (repr, Treap.singleton row);
            );
            (.n, .m, .repr)
        );
        let at_mut = (map :: &t, i, j) -> &Int32 => (
            Treap.at (Treap.at (&map^.repr, i), j)
        );
        let at = (map, i, j) => (at_mut (map, i, j))^;
    );
    print "[INFO] creating empty map";
    let map = Map.create (Treap.length &xs, Treap.length &ys);
    print "[INFO] created empty map";
    
    print "[INFO] drawing edges";
    for i in 0..List.length &vs do (
        print (
            "[INFO] progress "
            + (to_string i)
            + "/"
            + (to_string <| List.length &vs)
        );
        let next = i + 1;
        if next == List.length &vs then (
            next = 0;
        );
        let next_next = next + 1;
        if next_next == List.length &vs then (
            next_next = 0;
        );
        let a = (List.at (&vs, i))^;
        let b = (List.at (&vs, next))^;
        let c = (List.at (&vs, next_next))^;
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
    List.iter (
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
    
    let answer = as_Int64 0;
    for i in 0..List.length &vs do (
        let next = i + 1;
        if next == List.length &vs then (
            next = 0;
        );
        let a = (List.at (&vs, i))^;
        let b = (List.at (&vs, next))^;
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
