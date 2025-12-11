#!/usr/bin/env kast
use (include "../common.ks").*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file input_path;

let verbose = false;

const Map = (
    module:
    use std.collections.treap;
    const KV = [K, V] type (
        .key :: K,
        .value :: V,
    );
    const t = [K, V] type (
        .inner :: treap.t[KV[K, V]],
    );
    const create = [K, V] () -> t[K, V] => (
        .inner = treap.create ()
    );
    
    const treap_split = [T] (v :: treap.t[T], f :: treap.node_splitter[T]) -> (treap.t[T], treap.t[T]) => (
        match v with (
            | :Empty => (:Empty, :Empty)
            | :Node node => match f &node with (
                | :RightSubtree => (
                    let left_left, left_right = treap_split (node.left, f);
                    let node = treap.update_data (
                        node,
                        .left = left_right,
                        .right = node.right,
                    );
                    left_left, node
                )
                | :LeftSubtree => (
                    let right_left, right_right = treap_split (node.right, f);
                    let node = treap.update_data (
                        node,
                        .left = node.left,
                        .right = right_left,
                    );
                    node, right_right
                )
                | :Node (left, right) => (
                    let left = treap.singleton left;
                    let right = treap.singleton right;
                    treap.join (node.left, left), treap.join (right, node.right)
                )
            )
        )
    );
    
    const get_or_init = [K, V] (
        map :: &t[K, V],
        key :: K,
        init :: () -> V,
    ) -> &V => (
        let less, greater_or_equal = treap_split (
            map^.inner,
            data => (
                if data^.value.key < key then (
                    :LeftSubtree
                ) else (
                    :RightSubtree
                )
            ),
        );
        let equal, greater = treap_split (
            greater_or_equal,
            data => (
                if data^.value.key <= key then (
                    :LeftSubtree
                ) else (
                    :RightSubtree
                )
            ),
        );
        if treap.length &equal == 0 then (
            equal = treap.singleton (.key, .value = init ());
        );
        map^.inner = treap.join (less, treap.join (equal, greater));
        &(treap.at (&equal, 0))^.value
    );
    
    const iter = [K, V] (map :: &t[K, V], f :: &KV[K, V] -> ()) => (
        treap.iter (&map^.inner, f)
    );
);

const Graph = (
    module:
    const vid = string;
    const vertex = [T] type (
        .id :: vid,
        .data :: T,
        .out :: list.t[type (&vertex[T])],
    );
    const t = [T] type (
        .vs :: Map.t[vid, vertex[T]],
    );
    const create = [T] () -> t[T] => (
        .vs = Map.create ()
    );
    const get_or_init_vertex = [T] (
        g :: &t[T],
        id :: vid,
        init :: () -> T,
    ) -> &vertex[T] => (
        Map.get_or_init (
            &g^.vs,
            id,
            () => (
                .id,
                .data = init (),
                .out = list.create (),
            ),
        )
    );
    const get = [T] (g :: &t[T], id :: vid) -> &vertex[T] => (
        get_or_init_vertex (g, id, () => (panic "vertex not found"))
    );
    const print = [T] (g :: &t[T]) => (
        Map.iter (
            &g^.vs,
            &(.key = id, .value = v) => (
                let s = id + ": ";
                let first = true;
                list.iter (
                    &v.out,
                    &u => (
                        if first then (
                            first = false;
                        ) else (
                            s += ", "
                        );
                        s += u^.id;
                    ),
                );
                std.io.print s;
            ),
        );
    );
);

const VertexData = type (
    .paths_to_target :: int32,
);
let g :: Graph.t[VertexData] = Graph.create ();
let get_or_init_vertex = (name :: string) => (
    Graph.get_or_init_vertex (
        &g,
        name,
        () => (
            .paths_to_target = -1,
        ),
    )
);
String.lines (
    input,
    line => with_return (
        if String.length line == 0 then return;
        let v, out = String.split_once (line, ':');
        let v = get_or_init_vertex v;
        let v = String.split (
            out,
            ' ',
            u => (
                let u = String.trim u;
                if String.length u != 0 then (
                    let u = get_or_init_vertex u;
                    list.push_back (&v^.out, u);
                );
            ),
        );
    ),
);

if verbose then (
    Graph.print &g;
);

let Part1 = (
    module:
    
    let target = "out";
    let solve = v => with_return (
        if v^.id == target then return 1;
        let v_result = &v^.data.paths_to_target;
        if v_result^ == -1 then (
            v_result^ = 0;
            list.iter (
                &v^.out,
                &u => (
                    v_result^ += solve u;
                ),
            );
        );
        v_result^
    );
);

let solve_p1 = () => (
    Part1.solve <| Graph.get (&g, "you")
);

dbg.print <| solve_p1 ();
