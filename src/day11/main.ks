#!/usr/bin/env kast
include "../common.ks";
std.sys.chdir(std.path.dirname(__FILE__));
let input = std.fs.read_file(input_path);
use std.collections.Map;
let verbose = false;
let as_Int64 :: Int32 -> Int64 = x => (x |> to_string |> parse);
let zero = as_Int64(0);
let one = as_Int64(1);
const Graph = (
    module:
    const VertexId = String;
    const Vertex = [T] newtype (
        .id :: VertexId,
        .data :: T,
        .out :: List.t[type (&mut Vertex[T])],
    );
    const t = [T] newtype (
        .vs :: Map.t[VertexId, Vertex[T]],
    );
    const create = [T] () -> t[T] => (
        .vs = Map.create()
    );
    const get_or_init_vertex = [T] (
        g :: &mut t[T],
        id :: VertexId,
        init :: () -> T,
    ) -> &mut Vertex[T] => (
        Map.get_or_init(
            &mut g^.vs,
            id,
            () => (
                .id,
                .data = init(),
                .out = List.create(),
            ),
        )
    );
    const get_mut = [T] (g :: &mut t[T], id :: VertexId) -> &mut Vertex[T] => (
        get_or_init_vertex(g, id, () => panic("vertex not found"))
    );
    const get = [T] (g :: &t[T], id :: VertexId) -> &Vertex[T] => (
        Map.get(&g^.vs, id) |> Option.unwrap
    );
    const print = [T] (g :: &t[T]) => (
        for &(.key = id, .value = v) in Map.iter(&g^.vs) do (
            let mut s = id + ": ";
            let mut first = true;
            for &u in List.iter(&v.out) do (
                if first then (
                    first = false;
                ) else (
                    
                    s += ", "
                );
                
                s += u^.id;
            );
            
            std.io.print(s);
        );
    );
);
const Pt2Data = newtype (
    # [visited_dac][visited_fft]
    .false_false :: Int64,
    .false_true :: Int64,
    .true_false :: Int64,
    .true_true :: Int64,
);
const VertexData = newtype (
    .pt2 :: Pt2Data,
    .paths_to_target :: Int32,
);
let mut g :: Graph.t[VertexData] = Graph.create();
let get_or_init_vertex = (name :: String) => (
    Graph.get_or_init_vertex(
        &mut g,
        name,
        () => (
            .paths_to_target = -1,
            .pt2 = (
                .false_false = -one,
                .false_true = -one,
                .true_false = -one,
                .true_true = -one,
            ),
        ),
    )
);
for line in String.lines(input) do (
    if String.length(line) == 0 then continue;
    let v, out = String.split_once(line, ':');
    let v = get_or_init_vertex(v);
    for u in String.split(out, ' ') do (
        let u = String.trim(u);
        if String.length(u) != 0 then (
            let u = get_or_init_vertex(u);
            List.push_back(&mut v^.out, u);
        );
    );
);
if verbose then (
    Graph.print(&g);
);
let Part1 = (
    module:
    let dp = v => with_return (
        if v^.id == "out" then return 1;
        let v_result = &mut v^.data.paths_to_target;
        if v_result^ == -1 then (
            v_result^ = 0;
            for &u in List.iter(&v^.out) do (
                v_result^ += dp(u);
            );
        );
        
        v_result^
    );
    let solve = () => dp <| Graph.get_mut(&mut g, "you");
);
let Part2 = (
    module:
    let dp = (v, visited_dac, visited_fft) => with_return (
        if v^.id == "out" then (
            if visited_dac and visited_fft then (
                return one;
            ) else (
                return zero;
            );
        );
        let data = &mut v^.data.pt2;
        let v_result = if visited_dac then (
            if visited_fft then (
                &mut data^.true_true
            ) else (
                &mut data^.true_false
            )
        ) else (
            if visited_fft then (
                &mut data^.false_true
            ) else (
                &mut data^.false_false
            )
        );
        if v_result^ == -one then (
            v_result^ = zero;
            let visited_dac = visited_dac or v^.id == "dac";
            let visited_fft = visited_fft or v^.id == "fft";
            for &u in List.iter(&v^.out) do (
                v_result^ += dp(u, visited_dac, visited_fft);
            );
        );
        
        v_result^
    );
    let solve = () => dp(Graph.get_mut(&mut g, "svr"), false, false);
);
let answer = if part1 then (
    Part1.solve() |> as_Int64
) else (
    
    Part2.solve()
);

dbg.print(answer);
assert_answers(
    answer,
    .example = (.part1 = parse("5"), .part2 = parse("2")),
    .part1 = parse("615"),
    .part2 = parse("303012373210128"),
);
